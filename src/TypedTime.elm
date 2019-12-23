module TypedTime exposing
    (milliseconds
    , seconds
    , minutes
    , hours
    , equal
    , multiply
    , common
    , ratio
    , add
    , sub
    , Unit(..)
    , toString
   --  , parseStringToChar
    -- , hmsParser
    )

{-|

This library provides functions for working with typed time.
For example,

    equal (seconds 60) (minutes 1)
    --> True

    add (hours 1) (minutes 20) |> toString Minutes
    --> "01:20"

    add (hours 1) (minutes 20) |> toString Seconds
    --> "01:20:00"


## Constructors

@docs hours, minutes, seconds, milliseconds

## Operators

@docs equal, common, multiply, add, sub, ratio

## Conversion

@docs toString, Unit


-}

import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)


type TypedTime
    = TypedTime Unit Float

{-|

-}
type Unit
    = Milliseconds
    | Seconds
    | Minutes
    | Hours


ord : Unit -> Int
ord u =
    case u of
        Milliseconds -> 0
        Seconds -> 1
        Minutes -> 2
        Hours -> 3


unit : Int -> Unit
unit k =
    case (modBy 4 k) of
        0 -> Milliseconds
        1 -> Seconds
        2 -> Minutes
        3 -> Hours
        _ -> Milliseconds



{-|

    common Seconds Minutes
    --> Seconds

-}
common : Unit -> Unit -> Unit
common u v =
    unit (min (ord u) (ord v))

{-|

    milliseconds 1000  |> toString Seconds
    --> "00:00:01"


-}
milliseconds : Float -> TypedTime
milliseconds t =
    TypedTime Milliseconds t


{-|

    seconds 44 |> toString Seconds
    --> "00:00:44"


-}
seconds : Float -> TypedTime
seconds t =
    TypedTime Seconds (1000*t)

{-|

    minutes 14.5 |> toString Seconds
    --> "00:14:30"


 -}
minutes : Float -> TypedTime
minutes t =
     TypedTime Minutes (60000*t)

{-|

    hours 1.5 |> toString Minutes
    --> "01:30"

 -}
hours : Float -> TypedTime
hours t =
     TypedTime Hours (3600000*t)


{-|

    equal (milliseconds 1000) (seconds 1)
    --> True

     equal (hours 1) (minutes 60)
     --> True

     equal (minutes 1) (seconds 60)
     --> True

 -}
equal : TypedTime -> TypedTime -> Bool
equal s t =
    abs ((convertToMilliSeconds s) - (convertToMilliSeconds t)) < 0.001





{-|

    tt : List TypedTime
    tt = [seconds 30, minutes 1]

    equal (sum tt) (milliseconds 90000)

    equal (sum tt) (minutes 1.5)

-}
sum : List TypedTime -> TypedTime
sum timeList =
    timeList
        |> List.map convertToMilliSeconds
        |> List.sum
        |> TypedTime Milliseconds

--
--
--convert : Unit -> TypedTime -> TypedTime
--convert u t =



map : (Float -> Float) -> TypedTime -> TypedTime
map f (TypedTime u t) =
    (TypedTime u (f t))





map2 : (Float -> Float -> Float) -> TypedTime -> TypedTime -> TypedTime
map2 f (TypedTime u1 t1) (TypedTime u2 t2)=
    let
        u3 = common u1 u2
        t3 = f t1 t2
    in
        (TypedTime u3 t3)
{-|

    equal (multiply 0.5 (minutes 1)) (seconds 30)
    --> True

-}
multiply : Float -> TypedTime -> TypedTime
multiply f t =
   map (\x -> f * x)  t

{-|

    abs ((ratio (minutes 1) (seconds 60)) - 1.0) < 0.000001
    --> True


-}
ratio : TypedTime -> TypedTime -> Float
ratio denom num =
    convertToMilliSeconds num / convertToMilliSeconds denom



{-|

    equal (add (seconds 60) (minutes 1)) (minutes 2)
    --> True

-}
add : TypedTime -> TypedTime -> TypedTime
add s t =
    map2 (+) s t


{-|

    equal (sub (seconds 120) (minutes 1)) (minutes 1)
    --> True


-}
sub : TypedTime -> TypedTime -> TypedTime
sub s t =
    map2 (-) s t


convertToMilliSeconds : TypedTime -> Float
convertToMilliSeconds (TypedTime _ t) = t


{-|

    toString Seconds (minutes 30)
    --> "00:30:00"

    toString Seconds (hours 0.5)
    --> "00:30:00"

    toString Minutes (hours 0.5)
    --> "00:30"

 -}
toString : Unit -> TypedTime -> String
toString u (TypedTime unit_ value) =
    case u of
        Milliseconds ->
            hmsStringFromSeconds (value/1000)
        Seconds ->
            hmsStringFromSeconds (value/1000)

        Minutes ->
            hmStringFromSeconds (value/1000)

        Hours ->
            hmStringFromSeconds (value/1000)


type alias HMSRecord =
    {  hours : Int,  minutes : Int,  seconds : Int}

type alias HMSSRecord =
    { seconds : String, minutes : String, hours : String }

type alias HMRecord =
    { hours : Int, minutes : Int }


hmsRecordFromSeconds : Float -> HMSRecord
hmsRecordFromSeconds s =
    let
        s1 =
            round s

        s2 =
            modBy 60 s1

        m1 =
            s1 // 60

        m2 =
            modBy 60 m1

        h1 =
            m1 // 60
    in
    { seconds = s2, minutes = m2, hours = h1 }

--
hmsStringFromSeconds : Float -> String
hmsStringFromSeconds s =
    let
        tr =
            hmsRecordFromSeconds s
    in
    (String.padLeft 2 '0' <| String.fromInt tr.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.minutes)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.seconds)



---
---


hmRecordFromSeconds : Float -> HMRecord
hmRecordFromSeconds s =
    let
        m1 =
            round (s / 60)

        m2 =
            modBy 60 m1

        h1 =
            m1 // 60
    in
    { minutes = m2, hours = h1 }


hmStringFromSeconds : Float -> String
hmStringFromSeconds s =
    let
        tr =
            hmRecordFromSeconds s
    in
    (String.padLeft 2 '0' <| String.fromInt tr.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.minutes)



--
-- TIME PARSER
--
{-|

    import Parser

    Parser.run hmsParser "1:2:3"
    --> Ok { seconds = 3, minutes = 2, hours = 1 }

-}
hmsParser : Parser HMSRecord
hmsParser =
    Parser.succeed HMSRecord
        |= Parser.int
        |. Parser.symbol ":"
        |= Parser.int
        |. Parser.symbol ":"
        |= Parser.int


--parsePermission : Parser Document.Permission
--parsePermission =
--    parseStringToChar ')'
--        |> Parser.map Document.permissionFromString
--


{-|

    import Parser

    Parser.run (parseStringToChar '.') "foo.bar"
    --> Ok "foo"


-}
parseStringToChar : Char -> Parser String
parseStringToChar endChar =
    (Parser.getChompedString <|
        Parser.succeed identity
            |. parseWhile (\c -> c /= endChar)
    )
        |> Parser.map String.trim


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    Parser.chompWhile accepting |> Parser.getChompedString



decodeHM : String -> Maybe Float
decodeHM str =
    let
        parts =
            String.split ":" (String.trim str)
                |> List.map String.toFloat
                |> Maybe.Extra.values
    in
    case List.length parts of
        1 ->
            Maybe.map (\x -> 60 * x) (List.Extra.getAt 0 parts)

        2 ->
            let
                hoursPart =
                    Maybe.map (\x -> 3600 * x) (List.Extra.getAt 0 parts)

                minutesPart =
                    Maybe.map (\x -> 60 * x) (List.Extra.getAt 1 parts)
            in
            Maybe.map2 (+) hoursPart minutesPart

        _ ->
            Nothing


decode : String -> TypedTime
decode str =
    case decodeHM str of
        Nothing ->
            seconds 0

        Just t ->
            seconds (1.0 * t)
