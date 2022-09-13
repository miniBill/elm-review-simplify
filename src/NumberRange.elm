module NumberRange exposing (NumberRange, belongsTo, greaterThan, greaterThanOrEqual, greaterThanOrEquals, infinity, intersect, isGreaterThan, isLessThan, isLessThanOrEqual, isSingleton, lessThan, lessThanOrEquals, merge, mergeList, minus, negInfinity, negate, plus, singleton)


type alias NumberRange =
    { from : Float
    , fromIncluded : Bool
    , to : Float
    , toIncluded : Bool
    }


mergeList : List NumberRange -> List NumberRange
mergeList ranges =
    ranges
        -- We have a list of ranges, if we sort them by `from` we'll be able to merge them (if possible) by just considering adjacent ones
        |> List.sortBy (\{ from } -> -from)
        |> List.foldl
            (\e acc ->
                case acc of
                    [] ->
                        [ e ]

                    last :: rest ->
                        case merge e last of
                            Just u ->
                                u :: rest

                            Nothing ->
                                e :: acc
            )
            []
        |> List.reverse


merge : NumberRange -> NumberRange -> Maybe NumberRange
merge l r =
    if r.from < l.from then
        merge r l

    else if l.to < r.from || (l.to == r.from && not l.toIncluded && not r.fromIncluded) then
        Nothing

    else
        Just
            { from = l.from
            , fromIncluded = l.fromIncluded || (l.from == r.from && r.fromIncluded)
            , to = max l.to r.to
            , toIncluded =
                if l.to < r.to then
                    r.toIncluded

                else if l.to > r.to then
                    l.toIncluded

                else
                    l.toIncluded || r.toIncluded
            }


plus : NumberRange -> NumberRange -> NumberRange
plus l r =
    { from = l.from + r.from
    , fromIncluded = l.fromIncluded && r.fromIncluded
    , to = l.to + r.to
    , toIncluded = l.toIncluded && r.toIncluded
    }


negate : NumberRange -> NumberRange
negate c =
    { from = -c.to
    , fromIncluded = c.toIncluded
    , to = -c.from
    , toIncluded = c.fromIncluded
    }


singleton : Float -> NumberRange
singleton n =
    { from = n
    , fromIncluded = True
    , to = n
    , toIncluded = True
    }


isSingleton : NumberRange -> Maybe Float
isSingleton r =
    if r.fromIncluded && r.toIncluded && r.from == r.to then
        Just r.from

    else
        Nothing


intersect : NumberRange -> NumberRange -> Maybe NumberRange
intersect lr rr =
    let
        ( from, fromIncluded ) =
            if lr.from > rr.from then
                ( lr.from, lr.fromIncluded )

            else if lr.from < rr.from then
                ( rr.from, rr.fromIncluded )

            else
                ( lr.from, lr.fromIncluded && rr.fromIncluded )

        ( to, toIncluded ) =
            if lr.to < rr.to then
                ( lr.to, lr.toIncluded )

            else if lr.to > rr.to then
                ( rr.to, rr.toIncluded )

            else
                ( lr.to, lr.toIncluded && rr.toIncluded )
    in
    if from < to || (from == to && fromIncluded && toIncluded) then
        Just { from = from, fromIncluded = fromIncluded, to = to, toIncluded = toIncluded }

    else
        Nothing


minus : NumberRange -> NumberRange -> NumberRange
minus l r =
    plus l (negate r)


isLessThan : NumberRange -> NumberRange -> Maybe Bool
isLessThan l r =
    if l.to < r.from || ((not l.toIncluded || not r.fromIncluded) && l.to == r.from) then
        Just True

    else if r.to < l.from || ((not l.toIncluded || not r.fromIncluded) && r.to == l.from) then
        Just False

    else
        Nothing


isLessThanOrEqual : NumberRange -> NumberRange -> Maybe Bool
isLessThanOrEqual l r =
    if l.to <= r.from then
        Just True

    else if r.to < l.from then
        Just False

    else
        Nothing


isGreaterThan : NumberRange -> NumberRange -> Maybe Bool
isGreaterThan l r =
    isLessThan r l


greaterThanOrEqual : NumberRange -> NumberRange -> Maybe Bool
greaterThanOrEqual l r =
    isLessThanOrEqual r l


infinity : Float
infinity =
    1 / 0


negInfinity : Float
negInfinity =
    -1 / 0


lessThan : Float -> NumberRange
lessThan float =
    { from = negInfinity
    , fromIncluded = False
    , to = float
    , toIncluded = False
    }


lessThanOrEquals : Float -> NumberRange
lessThanOrEquals float =
    { from = negInfinity
    , fromIncluded = False
    , to = float
    , toIncluded = True
    }


greaterThan : Float -> NumberRange
greaterThan float =
    { from = float
    , fromIncluded = False
    , to = infinity
    , toIncluded = False
    }


greaterThanOrEquals : Float -> NumberRange
greaterThanOrEquals float =
    { from = float
    , fromIncluded = True
    , to = infinity
    , toIncluded = False
    }


belongsTo : NumberRange -> Float -> Bool
belongsTo { from, to, fromIncluded, toIncluded } float =
    (float > from || (float == from && fromIncluded))
        && (float < to || (float == to && toIncluded))
