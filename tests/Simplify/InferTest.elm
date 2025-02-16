module Simplify.InferTest exposing (all)

import AssocList
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import NumberRange exposing (NumberRange)
import Simplify.Infer exposing (Fact(..), Inferred(..), deduceNewFacts, empty, falseExpr, get, infer, trueExpr)
import Test exposing (Test, describe, test)
import Value exposing (BooleanValue(..), Value(..))


all : Test
all =
    describe "Infer"
        [ simpleTests
        , detailedTests
        , deduceNewFactsTests
        , rangeTests
        ]


simpleTests : Test
simpleTests =
    describe "get"
        [ test "should infer a is true when a is True" <|
            \() ->
                empty
                    |> infer [ FunctionOrValue [] "a" ] True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just trueExpr)
        , test "should infer a is true when a is False" <|
            \() ->
                empty
                    |> infer [ FunctionOrValue [] "a" ] False
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just falseExpr)
        , test "should infer a is 1 when a == 1 is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "=="
                            Infix.Non
                            (n (FunctionOrValue [] "a"))
                            (n (Floatable 1))
                        ]
                        True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just (Floatable 1))
        , test "should not infer a when a == 1 is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "=="
                            Infix.Non
                            (n (FunctionOrValue [] "a"))
                            (n (Floatable 1))
                        ]
                        False
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal Nothing
        , test "should infer a is true when a && b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "&&"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just trueExpr)
        , test "should infer b is true when a && b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "&&"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should not infer a when a || b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal Nothing
        , test "should not infer b when a || b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal Nothing
        , test "should infer a is false when a || b is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        False
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just falseExpr)
        , test "should infer b is false when a || b is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        False
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just falseExpr)
        , test "should infer b is true when a || b is True and a is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        False
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should infer b is true when b || a is True and a is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "b"))
                            (n (FunctionOrValue [] "a"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        False
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should not infer b when a || b is True and a is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        True
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal Nothing
        ]


detailedTests : Test
detailedTests =
    describe "infer"
        [ test "should infer a when True" <|
            \() ->
                infer
                    [ FunctionOrValue [] "a" ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DBool DTrue
                              )
                            ]
                        }
        , test "should infer a when False" <|
            \() ->
                infer
                    [ FunctionOrValue [] "a" ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DBool DFalse
                              )
                            ]
                        }
        , test "should infer a == True when True" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n trueExpr)
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                trueExpr
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DBool DTrue
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                              , DBool DTrue
                              )
                            ]
                        }
        , test "should infer a == True when False" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n trueExpr)
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                falseExpr
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DBool DFalse
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                              , DBool DFalse
                              )
                            ]
                        }
        , test "should infer a == 1 when True" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n (Floatable 1))
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                (Floatable 1)
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DNumber
                                    (NumberRange.singleton 1)
                                    []
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                              , DBool DTrue
                              )
                            ]
                        }
        , test "should infer a == 1 when False" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n (Floatable 1))
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ NotEquals
                                (FunctionOrValue [] "a")
                                (Floatable 1)
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DNumber
                                    { from = -1 / 0
                                    , fromIncluded = False
                                    , to = 1
                                    , toIncluded = False
                                    }
                                    [ { from = 1
                                      , fromIncluded = False
                                      , to = 1 / 0
                                      , toIncluded = False
                                      }
                                    ]
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                              , DBool DFalse
                              )
                            ]
                        }
        , test "should infer a && b when True" <|
            \() ->
                infer
                    [ OperatorApplication "&&"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals (FunctionOrValue [] "b") trueExpr
                            , Equals (FunctionOrValue [] "a") trueExpr
                            , Equals
                                (OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", DBool DTrue )
                            , ( FunctionOrValue [] "a", DBool DTrue )
                            , ( OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , DBool DTrue
                              )
                            ]
                        }
        , test "should infer a && b when False" <|
            \() ->
                infer
                    [ OperatorApplication "&&"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Or
                                (Equals
                                    (FunctionOrValue [] "a")
                                    falseExpr
                                )
                                (Equals (FunctionOrValue [] "b") falseExpr)
                            , Equals
                                (OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , DBool DFalse
                              )
                            ]
                        }
        , test "should infer a || b when True" <|
            \() ->
                infer
                    [ OperatorApplication "||"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Or
                                (Equals
                                    (FunctionOrValue [] "a")
                                    trueExpr
                                )
                                (Equals
                                    (FunctionOrValue [] "b")
                                    trueExpr
                                )
                            , Equals
                                (OperatorApplication "||"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "||"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , DBool DTrue
                              )
                            ]
                        }
        , test "should infer a || b when False" <|
            \() ->
                infer
                    [ OperatorApplication "||"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals (FunctionOrValue [] "b") falseExpr
                            , Equals (FunctionOrValue [] "a") falseExpr
                            , Equals (OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b"))) falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", DBool DFalse )
                            , ( FunctionOrValue [] "a", DBool DFalse )
                            , ( OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")), DBool DFalse )
                            ]
                        }
        , test "should infer a || b when True and a when False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        False
                    |> expectEqual
                        { facts =
                            [ Equals (FunctionOrValue [] "b") (FunctionOrValue [ "Basics" ] "True")
                            , Equals (FunctionOrValue [] "a") (FunctionOrValue [ "Basics" ] "False")
                            , Or (Equals (FunctionOrValue [] "a") (FunctionOrValue [ "Basics" ] "True")) (Equals (FunctionOrValue [] "b") (FunctionOrValue [ "Basics" ] "True"))
                            , Equals (OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b"))) (FunctionOrValue [ "Basics" ] "True")
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", DBool DTrue )
                            , ( FunctionOrValue [] "a", DBool DFalse )
                            , ( OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")), DBool DTrue )
                            ]
                        }
        , test "should infer field values when known" <|
            \() ->
                Value.eval AssocList.empty
                    (RecordAccess
                        (n <|
                            RecordExpr
                                [ n
                                    ( n "a"
                                    , n <|
                                        OperatorApplication "-"
                                            Left
                                            (n <| Integer 2)
                                            (n <| Integer 1)
                                    )
                                ]
                        )
                        (n "a")
                    )
                    |> Expect.equal (Just (DNumber (NumberRange.singleton 1) []))
        ]


deduceNewFactsTests : Test
deduceNewFactsTests =
    describe "deduceNewFacts"
        [ test "should not deduce anything when facts don't share anything (a == True, b == True)" <|
            \() ->
                deduceNewFacts
                    (Equals (FunctionOrValue [] "b") trueExpr)
                    [ Equals (FunctionOrValue [] "a") trueExpr ]
                    |> Expect.equal []
        , test "should deduce b is True when (a || b) and (a == False)" <|
            \() ->
                deduceNewFacts
                    (Equals (FunctionOrValue [] "a") falseExpr)
                    [ Or
                        (Equals
                            (FunctionOrValue [] "a")
                            trueExpr
                        )
                        (Equals
                            (FunctionOrValue [] "b")
                            trueExpr
                        )
                    ]
                    |> Expect.equal
                        [ Equals (FunctionOrValue [] "b") trueExpr ]
        ]


rangeTests : Test
rangeTests =
    describe "combining two ranges should produce a range"
        [ Test.fuzz3
            rangeFuzzer
            rangeFuzzer
            Fuzz.float
            "such that a number belongs to the intersection range iff it belong to both input ranges"
            (\lrange rrange testPoint ->
                case NumberRange.intersect lrange rrange of
                    Nothing ->
                        Expect.false "The intersection is empty so the point should belong to at most one interval"
                            (NumberRange.belongsTo lrange testPoint && NumberRange.belongsTo rrange testPoint)

                    Just irange ->
                        Expect.true "The intersection is nonempty and the point should belong to it iff it belongs to both intervals"
                            (NumberRange.belongsTo irange testPoint == (NumberRange.belongsTo lrange testPoint && NumberRange.belongsTo rrange testPoint))
            )
        , Test.fuzz3
            rangeFuzzer
            rangeFuzzer
            Fuzz.float
            "such that a number belongs to the union range iff it belong to either of input ranges"
            (\lrange rrange testPoint ->
                case NumberRange.merge lrange rrange of
                    Nothing ->
                        -- If the union is empty there is not really much to do
                        Expect.pass

                    Just irange ->
                        let
                            belongsToUnion : Bool
                            belongsToUnion =
                                NumberRange.belongsTo irange testPoint
                        in
                        if NumberRange.belongsTo lrange testPoint then
                            Expect.true
                                ("The point belong to the left range, so it must belong to the union, but it doesn't - the union is " ++ Debug.toString irange)
                                belongsToUnion

                        else if NumberRange.belongsTo rrange testPoint then
                            Expect.true
                                ("The point belong to the right range, so it must belong to the union, but it doesn't - the union is " ++ Debug.toString irange)
                                belongsToUnion

                        else
                            Expect.false
                                ("The point belongs to neither range, so it must not belong to the union, but it does - the union is " ++ Debug.toString irange)
                                belongsToUnion
            )
        ]


rangeFuzzer : Fuzzer NumberRange
rangeFuzzer =
    let
        edgeFuzzer : Fuzzer ( Float, Bool )
        edgeFuzzer =
            Fuzz.map2 Tuple.pair Fuzz.float Fuzz.bool

        niceEdgeFuzzer : Fuzzer ( Float, Bool )
        niceEdgeFuzzer =
            Fuzz.map2 Tuple.pair
                (List.range -2 2
                    |> List.map (toFloat >> Fuzz.constant)
                    |> Fuzz.oneOf
                )
                Fuzz.bool
    in
    Fuzz.map2
        (\( from, fromIncluded ) ( to, toIncluded ) ->
            { from = from
            , fromIncluded = fromIncluded
            , to = to
            , toIncluded = toIncluded
            }
        )
        (Fuzz.oneOf [ Fuzz.constant ( -1 / 0, False ), edgeFuzzer, niceEdgeFuzzer ])
        (Fuzz.oneOf [ Fuzz.constant ( 1 / 0, False ), edgeFuzzer, niceEdgeFuzzer ])


expectEqual :
    { facts : List Fact
    , deduced : List ( Expression, Value )
    }
    -> Inferred
    -> Expectation
expectEqual record (Inferred inferred) =
    { facts = inferred.facts
    , deduced = AssocList.toList inferred.deduced
    }
        |> Expect.equal record


n : a -> Node a
n =
    Node Range.emptyRange
