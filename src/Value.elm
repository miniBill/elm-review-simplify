module Value exposing (BooleanValue(..), Value(..), eval, union)

import AssocList
import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import NumberRange exposing (NumberRange)


type Value
    = DBool BooleanValue
    | DNumber NumberRange (List NumberRange)
    | DStringOneOf String (List String)
    | DStringNeitherOf String (List String)
    | DCharOneOf Char (List Char)
    | DCharNeitherOf Char (List Char)
    | DRecord (Dict String Value)
    | DUnit


type BooleanValue
    = DTrue
    | DFalse
    | DTrueOrFalse String


union : Value -> Value -> Maybe Value
union l r =
    case ( l, r ) of
        ( DRecord _, DRecord _ ) ->
            -- TODO: Combine records, BEWARE - only combine if the set of fields is the same
            Nothing

        ( DRecord _, _ ) ->
            Nothing

        ( _, DRecord _ ) ->
            Nothing

        ( DNumber lh lt, DNumber rh rt ) ->
            numberFromRanges (lh :: rh :: lt ++ rt)

        ( DNumber _ _, _ ) ->
            Nothing

        ( _, DNumber _ _ ) ->
            Nothing

        _ ->
            Debug.todo ("Value.union " ++ Debug.toString l ++ " " ++ Debug.toString r)


numberFromRanges : List NumberRange -> Maybe Value
numberFromRanges ranges =
    case NumberRange.mergeList ranges of
        [] ->
            Nothing

        h :: t ->
            Just <| DNumber h t


eval : AssocList.Dict Expression Value -> Expression -> Maybe Value
eval deduced expr =
    case AssocList.get expr deduced of
        Just v ->
            Just v

        Nothing ->
            case expr of
                Expression.FunctionOrValue [ "Basics" ] "True" ->
                    Just (DBool DTrue)

                Expression.FunctionOrValue [ "Basics" ] "False" ->
                    Just (DBool DFalse)

                Expression.FunctionOrValue _ _ ->
                    -- This should have been in the `deduced` dictionary
                    Nothing

                Expression.Application _ ->
                    Nothing

                Expression.Integer i ->
                    Just <| floatToDeduced <| toFloat i

                Expression.Hex h ->
                    Just <| floatToDeduced <| toFloat h

                Expression.Floatable f ->
                    Just <| floatToDeduced f

                Expression.Literal s ->
                    Just <| DStringOneOf s []

                Expression.CharLiteral c ->
                    Just <| DCharOneOf c []

                Expression.ParenthesizedExpression (Node _ c) ->
                    eval deduced c

                Expression.LetExpression { expression } ->
                    -- TODO: use declarations to infer more facts
                    eval deduced (Node.value expression)

                Expression.IfBlock _ (Node _ thenBranch) (Node _ elseBranch) ->
                    -- TODO: use condition to infer more facts
                    thenBranch
                        |> eval deduced
                        |> Maybe.andThen
                            (\thenValue ->
                                elseBranch
                                    |> eval deduced
                                    |> Maybe.andThen
                                        (\elseValue ->
                                            union thenValue elseValue
                                        )
                            )

                Expression.OperatorApplication op assoc (Node _ l) (Node _ r) ->
                    operatorEval deduced op assoc l r

                Expression.Negation (Node _ c) ->
                    numberOperation1 deduced NumberRange.negate c

                Expression.CaseExpression { cases } ->
                    case cases of
                        [] ->
                            Nothing

                        ( _, Node _ h ) :: t ->
                            List.foldl
                                (\( _, Node _ caseExpression ) acc ->
                                    acc
                                        |> Maybe.andThen
                                            (\accValue ->
                                                caseExpression
                                                    |> eval deduced
                                                    |> Maybe.andThen
                                                        (\caseValue ->
                                                            union accValue caseValue
                                                        )
                                            )
                                )
                                (eval deduced h)
                                t

                Expression.RecordExpr fields ->
                    fields
                        |> List.filterMap
                            (\(Node _ ( Node _ k, Node _ e )) ->
                                Maybe.map (Tuple.pair k) (eval deduced e)
                            )
                        |> Dict.fromList
                        |> DRecord
                        |> Just

                Expression.RecordUpdateExpression (Node _ recordName) fields ->
                    let
                        recordFields : List ( String, Value )
                        recordFields =
                            Expression.FunctionOrValue [] recordName
                                |> eval deduced
                                |> Maybe.andThen
                                    (\recordValue ->
                                        case recordValue of
                                            DRecord fs ->
                                                Just (Dict.toList fs)

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.withDefault []

                        updateFields : List ( String, Value )
                        updateFields =
                            List.filterMap
                                (\(Node _ ( Node _ k, Node _ e )) ->
                                    Maybe.map (Tuple.pair k) (eval deduced e)
                                )
                                fields

                        combinedFields : List ( String, Value )
                        combinedFields =
                            recordFields ++ updateFields
                    in
                    combinedFields
                        |> Dict.fromList
                        |> DRecord
                        |> Just

                Expression.RecordAccess (Node _ e) (Node _ field) ->
                    e
                        |> eval deduced
                        |> Maybe.andThen
                            (\eValue ->
                                case eValue of
                                    DRecord fields ->
                                        Dict.get field fields

                                    _ ->
                                        Nothing
                            )

                Expression.UnitExpr ->
                    Just DUnit

                Expression.ListExpr _ ->
                    -- TODO: implement
                    Nothing

                Expression.TupledExpression _ ->
                    -- TODO: implement
                    Nothing

                Expression.PrefixOperator _ ->
                    Nothing

                Expression.Operator _ ->
                    -- Not possible in practice
                    Nothing

                Expression.LambdaExpression _ ->
                    Nothing

                Expression.RecordAccessFunction _ ->
                    Nothing

                Expression.GLSLExpression _ ->
                    Nothing


operatorEval : AssocList.Dict Expression Value -> String -> InfixDirection -> Expression -> Expression -> Maybe Value
operatorEval deduced op assoc l r =
    case ( op, assoc ) of
        ( "+", Left ) ->
            numberOperation2 deduced NumberRange.plus numberFromRanges l r

        ( "-", Left ) ->
            numberOperation2 deduced NumberRange.minus numberFromRanges l r

        ( "*", Left ) ->
            -- (Node _ l) (Node _ r) ->
            -- TODO implement multiplication
            -- numberOperation2 deduced NumberRange.by l r
            Nothing

        ( "/", Left ) ->
            -- (Node _ l) (Node _ r) ->
            -- TODO implement division
            -- numberOperation2 deduced NumberRange.divide l r
            Nothing

        ( "==", Non ) ->
            Maybe.map2
                (\lValue rValue ->
                    DBool <| equals lValue rValue
                )
                (eval deduced l)
                (eval deduced r)

        ( "/=", Non ) ->
            Maybe.map2
                (\lValue rValue ->
                    DBool <| boolValueNot <| equals lValue rValue
                )
                (eval deduced l)
                (eval deduced r)

        ( "||", Right ) ->
            case eval deduced l of
                Just (DBool DFalse) ->
                    eval deduced r

                (Just (DBool DTrue)) as true ->
                    true

                _ ->
                    case eval deduced r of
                        (Just (DBool DTrue)) as true ->
                            true

                        _ ->
                            Just <| DBool (DTrueOrFalse "eval ||")

        ( "&&", Right ) ->
            case eval deduced l of
                Just (DBool DTrue) ->
                    eval deduced r

                (Just (DBool DFalse)) as false ->
                    false

                _ ->
                    case eval deduced r of
                        (Just (DBool DFalse)) as false ->
                            false

                        _ ->
                            Just <| DBool (DTrueOrFalse "eval &&")

        ( "<", Non ) ->
            numberOperation2 deduced NumberRange.lessThan combineBoolean l r

        _ ->
            Debug.todo ("TODO < None" ++ Debug.toString ( op, assoc ))


combineBoolean : List (Maybe Bool) -> Maybe Value
combineBoolean lst =
    case lst of
        [] ->
            Nothing

        h :: t ->
            List.foldl
                (\e acc ->
                    if e == acc then
                        e

                    else
                        Nothing
                )
                h
                t
                |> Maybe.map (toBoolValue >> DBool)


numberOperation1 :
    AssocList.Dict Expression Value
    -> (NumberRange -> NumberRange)
    -> Expression
    -> Maybe Value
numberOperation1 deduced f l =
    case eval deduced l of
        Just (DNumber lrange lranges) ->
            Just <| DNumber (f lrange) (List.map f lranges)

        _ ->
            Nothing


numberOperation2 :
    AssocList.Dict Expression Value
    -> (NumberRange -> NumberRange -> r)
    -> (List r -> Maybe Value)
    -> Expression
    -> Expression
    -> Maybe Value
numberOperation2 deduced map fold l r =
    case eval deduced l of
        Just (DNumber lrange lranges) ->
            case eval deduced r of
                Just (DNumber rrange rranges) ->
                    let
                        cartesian : List NumberRange -> List NumberRange -> List r
                        cartesian x y =
                            List.concatMap (\xe -> List.map (map xe) y) x
                    in
                    fold (cartesian (lrange :: lranges) (rrange :: rranges))

                _ ->
                    Nothing

        _ ->
            Nothing


equals : Value -> Value -> BooleanValue
equals l r =
    case ( l, r ) of
        ( DBool DTrue, DBool rb ) ->
            rb

        ( DBool DFalse, DBool rb ) ->
            boolValueNot rb

        ( DBool (DTrueOrFalse _), DBool _ ) ->
            DTrueOrFalse "equals ToF _"

        ( DBool _, _ ) ->
            DFalse

        ( _, DBool _ ) ->
            DFalse

        ( DStringOneOf lh [], DStringOneOf rh [] ) ->
            toBoolValue (lh == rh)

        ( DStringOneOf lh lt, DStringOneOf rh rt ) ->
            let
                ll : List String
                ll =
                    lh :: lt

                rl : List String
                rl =
                    rh :: rt
            in
            if List.any (\le -> List.any (\re -> le == re) rl) ll then
                DTrueOrFalse "equals str str"

            else
                DFalse

        ( DStringOneOf _ _, DStringNeitherOf _ _ ) ->
            Debug.todo ("equals " ++ Debug.toString l ++ " " ++ Debug.toString r)

        ( DStringNeitherOf _ _, DStringOneOf _ _ ) ->
            equals r l

        ( DStringOneOf _ _, _ ) ->
            DFalse

        ( _, DStringOneOf _ _ ) ->
            DFalse

        ( DStringNeitherOf _ _, DStringNeitherOf _ _ ) ->
            DTrueOrFalse "equals str strne"

        ( DNumber lh [], DNumber rh [] ) ->
            case ( NumberRange.isSingleton lh, NumberRange.isSingleton rh ) of
                ( Just ln, Just rn ) ->
                    toBoolValue (ln == rn)

                _ ->
                    case NumberRange.intersect lh rh of
                        Nothing ->
                            DFalse

                        Just _ ->
                            DTrueOrFalse "equals number [] number []"

        ( DNumber lh lt, DNumber rh rt ) ->
            let
                ll : List NumberRange
                ll =
                    lh :: lt

                rl : List NumberRange
                rl =
                    rh :: rt
            in
            if List.any (\le -> List.any (\re -> NumberRange.intersect le re /= Nothing) rl) ll then
                DTrueOrFalse "equals number number"

            else
                DFalse

        _ ->
            Debug.todo ("equals " ++ Debug.toString l ++ " " ++ Debug.toString r)


toBoolValue : Bool -> BooleanValue
toBoolValue b =
    if b then
        DTrue

    else
        DFalse


boolValueNot : BooleanValue -> BooleanValue
boolValueNot r =
    case r of
        DTrue ->
            DFalse

        DFalse ->
            DTrue

        DTrueOrFalse from ->
            DTrueOrFalse from


floatToDeduced : Float -> Value
floatToDeduced float =
    DNumber (NumberRange.singleton float) []
