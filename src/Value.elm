module Value exposing (BooleanValue(..), Value(..), eval, isSingleon, singleValueToString, union)

-- import Elm.Writer

import AssocList
import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import NumberRange exposing (NumberRange)
import Set


type Value
    = DBool BooleanValue
    | DNumber NumberRange (List NumberRange)
    | DStringOneOf String (List String)
    | DStringNeitherOf String (List String)
    | DCharOneOf Char (List Char)
    | DCharNeitherOf Char (List Char)
      -- A record may be incomplete!
    | DRecord (Dict String Value)
    | DUnit


type BooleanValue
    = DTrue
    | DFalse
    | DTrueOrFalse


union : Value -> Value -> Maybe Value
union l r =
    case ( l, r ) of
        ( DRecord _, DRecord _ ) ->
            -- TODO: Combine records, BEWARE - only combine if the set of fields is exactly the same
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

        ( DUnit, DUnit ) ->
            Just DUnit

        ( DUnit, _ ) ->
            Nothing

        ( _, DUnit ) ->
            Nothing

        ( DBool lb, DBool rb ) ->
            if lb == rb then
                Just <| DBool lb

            else
                Just <| DBool DTrueOrFalse

        ( _, DBool _ ) ->
            Nothing

        ( DBool _, _ ) ->
            Nothing

        ( DStringOneOf lh lt, DStringOneOf rh rt ) ->
            Just <| DStringOneOf lh (rh :: lt ++ rt)

        ( DStringOneOf lh lt, DStringNeitherOf rh rt ) ->
            let
                ls =
                    Set.fromList (lh :: lt)

                rs =
                    Set.fromList (rh :: rt)
            in
            case Set.toList (Set.diff rs ls) of
                [] ->
                    Nothing

                ih :: it ->
                    Just <| DStringNeitherOf ih it

        ( DStringNeitherOf _ _, DStringOneOf _ _ ) ->
            union r l

        ( DStringNeitherOf lh lt, DStringNeitherOf rh rt ) ->
            let
                ls =
                    Set.fromList (lh :: lt)

                rs =
                    Set.fromList (rh :: rt)
            in
            case Set.toList (Set.intersect ls rs) of
                [] ->
                    Nothing

                ih :: it ->
                    Just <| DStringNeitherOf ih it

        ( DStringOneOf _ _, _ ) ->
            Nothing

        ( _, DStringOneOf _ _ ) ->
            Nothing

        ( DStringNeitherOf _ _, _ ) ->
            Nothing

        ( _, DStringNeitherOf _ _ ) ->
            Nothing

        ( DCharOneOf lh lt, DCharOneOf rh rt ) ->
            Just <| DCharOneOf lh (rh :: lt ++ rt)

        ( DCharOneOf lh lt, DCharNeitherOf rh rt ) ->
            let
                ls =
                    Set.fromList (lh :: lt)

                rs =
                    Set.fromList (rh :: rt)
            in
            case Set.toList (Set.diff rs ls) of
                [] ->
                    Nothing

                ih :: it ->
                    Just <| DCharNeitherOf ih it

        ( DCharNeitherOf _ _, DCharOneOf _ _ ) ->
            union r l

        ( DCharNeitherOf lh lt, DCharNeitherOf rh rt ) ->
            let
                ls =
                    Set.fromList (lh :: lt)

                rs =
                    Set.fromList (rh :: rt)
            in
            case Set.toList (Set.intersect ls rs) of
                [] ->
                    Nothing

                ih :: it ->
                    Just <| DCharNeitherOf ih it


numberFromRanges : List NumberRange -> Maybe Value
numberFromRanges ranges =
    case NumberRange.mergeList ranges of
        [] ->
            Nothing

        h :: t ->
            Just <| DNumber h t


eval : AssocList.Dict Expression Value -> Expression -> Maybe Value
eval deduced expr =
    let
        -- exprToString : Expression -> String
        -- exprToString expr =
        --     Elm.Writer.write <|
        --         Elm.Writer.writeExpression
        --             (Node
        --                 { start = { row = 0, column = 0 }
        --                 , end = { row = 0, column = 0 }
        --                 }
        --                 expr
        --             )
        --
        -- _ =
        --     Debug.log "eval"
        --         { deduced =
        --             deduced
        --                 |> AssocList.toList
        --                 |> List.map (\( k, v ) -> ( exprToString k, v ))
        --         , expr = exprToString expr
        --         , result = result
        --         }
        result =
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

                        Expression.OperatorApplication op _ (Node _ l) (Node _ r) ->
                            operatorEval deduced op l r

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
    in
    result


operatorEval : AssocList.Dict Expression Value -> String -> Expression -> Expression -> Maybe Value
operatorEval deduced op l r =
    case op of
        "+" ->
            numberOperation2 deduced NumberRange.plus numberFromRanges l r

        "-" ->
            numberOperation2 deduced NumberRange.minus numberFromRanges l r

        "*" ->
            -- TODO implement multiplication
            -- numberOperation2 deduced NumberRange.by l r
            Nothing

        "/" ->
            -- TODO implement division
            -- numberOperation2 deduced NumberRange.divide l r
            Nothing

        "//" ->
            -- TODO implement division
            -- numberOperation2 deduced NumberRange.divide l r
            Nothing

        "==" ->
            Maybe.map2
                (\lValue rValue ->
                    DBool <| equals lValue rValue
                )
                (eval deduced l)
                (eval deduced r)

        "/=" ->
            Maybe.map2
                (\lValue rValue ->
                    DBool <| boolValueNot <| equals lValue rValue
                )
                (eval deduced l)
                (eval deduced r)

        "||" ->
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
                            Just <| DBool DTrueOrFalse

        "&&" ->
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
                            Just <| DBool DTrueOrFalse

        "<" ->
            numberOperation2 deduced NumberRange.isLessThan combineBoolean l r

        "<=" ->
            numberOperation2 deduced NumberRange.isLessThanOrEqual combineBoolean l r

        ">" ->
            numberOperation2 deduced NumberRange.isGreaterThan combineBoolean l r

        ">=" ->
            numberOperation2 deduced NumberRange.greaterThanOrEqual combineBoolean l r

        _ ->
            Nothing


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

        ( DBool DTrueOrFalse, DBool _ ) ->
            DTrueOrFalse

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
                DTrueOrFalse

            else
                DFalse

        ( DStringOneOf lh lt, DStringNeitherOf rh rt ) ->
            let
                rl =
                    rh :: rt
            in
            if List.all (\le -> List.member le rl) (lh :: lt) then
                DFalse

            else
                DTrueOrFalse

        ( DStringNeitherOf _ _, DStringOneOf _ _ ) ->
            equals r l

        ( DStringOneOf _ _, _ ) ->
            DFalse

        ( _, DStringOneOf _ _ ) ->
            DFalse

        ( DStringNeitherOf _ _, DStringNeitherOf _ _ ) ->
            DTrueOrFalse

        ( DStringNeitherOf _ _, _ ) ->
            DFalse

        ( _, DStringNeitherOf _ _ ) ->
            DFalse

        ( DCharOneOf lh [], DCharOneOf rh [] ) ->
            toBoolValue (lh == rh)

        ( DCharOneOf lh lt, DCharOneOf rh rt ) ->
            let
                ll : List Char
                ll =
                    lh :: lt

                rl : List Char
                rl =
                    rh :: rt
            in
            if List.any (\le -> List.any (\re -> le == re) rl) ll then
                DTrueOrFalse

            else
                DFalse

        ( DCharOneOf lh lt, DCharNeitherOf rh rt ) ->
            let
                rl =
                    rh :: rt
            in
            if List.all (\le -> List.member le rl) (lh :: lt) then
                DFalse

            else
                DTrueOrFalse

        ( DCharNeitherOf _ _, DCharOneOf _ _ ) ->
            equals r l

        ( DCharNeitherOf _ _, DCharNeitherOf _ _ ) ->
            DTrueOrFalse

        ( DNumber lh [], DNumber rh [] ) ->
            case ( NumberRange.isSingleton lh, NumberRange.isSingleton rh ) of
                ( Just ln, Just rn ) ->
                    toBoolValue (ln == rn)

                _ ->
                    case NumberRange.intersect lh rh of
                        Nothing ->
                            DFalse

                        Just _ ->
                            DTrueOrFalse

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
                DTrueOrFalse

            else
                DFalse

        ( DNumber _ _, _ ) ->
            DFalse

        ( _, DNumber _ _ ) ->
            DFalse

        ( DUnit, DUnit ) ->
            DTrue

        ( DUnit, _ ) ->
            DFalse

        ( _, DUnit ) ->
            DFalse

        ( DRecord lfs, DRecord rfs ) ->
            let
                isIncompatible : ( String, Value ) -> Bool
                isIncompatible ( lk, lv ) =
                    case Dict.get lk rfs of
                        Nothing ->
                            False

                        Just rv ->
                            equals lv rv == DFalse
            in
            if List.any isIncompatible (Dict.toList lfs) then
                DFalse

            else
                DTrueOrFalse

        ( DRecord _, _ ) ->
            DFalse

        ( _, DRecord _ ) ->
            DFalse


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

        DTrueOrFalse ->
            DTrueOrFalse


floatToDeduced : Float -> Value
floatToDeduced float =
    DNumber (NumberRange.singleton float) []


type SingleValue
    = SFloat Float
    | SString String
    | SChar Char
    | SBool Bool
    | SUnit


isSingleon : Value -> Maybe SingleValue
isSingleon val =
    case val of
        DBool DTrue ->
            Just <| SBool True

        DBool DFalse ->
            Just <| SBool False

        DNumber r [] ->
            Maybe.map SFloat <| NumberRange.isSingleton r

        DStringOneOf s [] ->
            Just <| SString s

        DCharOneOf c [] ->
            Just <| SChar c

        DUnit ->
            Just SUnit

        _ ->
            Nothing


singleValueToString : SingleValue -> String
singleValueToString sv =
    case sv of
        SFloat f ->
            String.fromFloat f

        SString s ->
            "\"" ++ s ++ "\""

        SChar c ->
            "'" ++ String.fromChar c ++ "'"

        SBool True ->
            "True"

        SBool False ->
            "False"

        SUnit ->
            "()"
