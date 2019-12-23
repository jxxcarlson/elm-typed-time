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
    , sum
    , Unit(..)
    , toString
    , fromString
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

## Types

@docs Unit

## Constructors

@docs hours, minutes, seconds, milliseconds

## Operators

@docs equal, common, multiply, add, sub, sum, ratio

## Conversion

@docs toString, fromString


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

    equal (sum [seconds 30, minutes 1]) (milliseconds 90000)
    --> True

    equal (sum [seconds 30, minutes 1]) (minutes 1.5)
    --> True

-}
sum : List TypedTime -> TypedTime
sum timeList =
    timeList
        |> List.map convertToMilliSeconds
        |> List.sum
        |> TypedTime Milliseconds



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

type alias HMRecord =
    { hours : Int, minutes : Int }


type alias MSRecord =
    { minutes : Int, seconds : Int }

typedTimeFromHMRecord : HMRecord -> TypedTime
typedTimeFromHMRecord r =
    add (hours (toFloat r.hours)) (minutes (toFloat r.minutes))

typedTimeFromMSRecord : MSRecord -> TypedTime
typedTimeFromMSRecord r =
    add (minutes (toFloat r.minutes)) (seconds (toFloat r.seconds))



typedTimeFromHMSRecord : HMSRecord -> TypedTime
typedTimeFromHMSRecord r =
    sum [ (hours (toFloat r.hours)), (minutes (toFloat r.minutes)), (seconds (toFloat r.seconds))  ]


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

    fromString Milliseconds "123"
    --> Just (milliseconds 123)

    fromString Seconds "23"
    --> Just  (seconds 23)

    fromString Seconds "2.17"
    --> Just  (seconds 2.17)

    fromString Seconds "4:23"
    --> Just (add (minutes 4) (seconds 23))

    fromString Seconds "04:03"
    --> Just (add (minutes 4) (seconds 3))

    fromString Minutes "04:03"
    --> Just (add (hours 4) (minutes 3))

    fromString Minutes "4.7"
    --> Just (minutes 4.7)

 -}
fromString : Unit -> String -> Maybe TypedTime
fromString u s =
    Parser.run (timeParser u) s
      |> Result.toMaybe


{-|

    import Parser

    Parser.run (timeParser Milliseconds) "123"
    x-> Ok (milliseconds 123)

    Parser.run (timeParser Seconds) "23"
    x-> Ok  (seconds 23)

    Parser.run (timeParser Seconds) "2.17"
    x-> Ok  (seconds 2.17)

    Parser.run (timeParser Seconds) "4:23"
    x-> Ok (add (minutes 4) (seconds 23))

    Parser.run (timeParser Seconds) "04:03"
    x-> Ok (add (minutes 4) (seconds 3))

    Parser.run (timeParser Minutes) "04:03"
    x-> Ok (add (hours 4) (minutes 3))

    Parser.run (timeParser Minutes) "4.7"
    x-> Ok (minutes 4.7)

 -}
timeParser : Unit -> Parser TypedTime
timeParser u =
    case u of
        Milliseconds -> Parser.float |> Parser.map milliseconds
        Seconds -> Parser.oneOf [Parser.backtrackable hmsParser, Parser.backtrackable msParser, sParser]
        Minutes -> Parser.oneOf [Parser.backtrackable hmParser, mParser]
        _ -> hParser


sParser : Parser TypedTime
sParser =
    Parser.float |> Parser.map seconds

mParser : Parser TypedTime
mParser =
    Parser.float |> Parser.map minutes


hParser : Parser TypedTime
hParser =
    Parser.float |> Parser.map hours


{-|

    import Parser

    Parser.run hmParser "1:2"
    x-> Ok (add (hours 1) (minutes 2))

    Parser.run hmParser "02:03"
    x-> Ok (add (minutes 3) (hours 2))
-}
hmParser : Parser TypedTime
hmParser =
    (Parser.succeed HMRecord
        |= Parser.oneOf [altIntParser, Parser.int]
        |. Parser.symbol ":"
        |= Parser.oneOf [altIntParser, Parser.int]
    )
      |> Parser.map typedTimeFromHMRecord


{-|

    import Parser

    Parser.run msParser "1:2"
    x-> Ok (add (minutes 1) (seconds 2))

    Parser.run msParser "02:03"
    x-> Ok (add (seconds 3) (minutes 2))
-}
msParser : Parser TypedTime
msParser =
    (Parser.succeed MSRecord
        |= Parser.oneOf [altIntParser, Parser.int]
        |. Parser.symbol ":"
        |= Parser.oneOf [altIntParser, Parser.int]
    )
      |> Parser.map typedTimeFromMSRecord

{-|

    import Parser

    Parser.run hmsParser "1:2:3"
    x-> Ok (sum [seconds 3, minutes 2, hours 1])

    Parser.run hmsParser "1:02:03"
    x-> Ok (sum [seconds 3, minutes 2, hours 1])
-}
hmsParser : Parser TypedTime
hmsParser =
    (Parser.succeed HMSRecord
        |= Parser.oneOf [altIntParser, Parser.int]
        |. Parser.symbol ":"
        |= Parser.oneOf [altIntParser, Parser.int]
        |. Parser.symbol ":"
        |= Parser.oneOf [altIntParser, Parser.int]
      )
        |> Parser.map typedTimeFromHMSRecord


{-|

    import Parser

    Parser.run altIntParser "07"
    x-> Ok 7

-}
altIntParser : Parser Int
altIntParser =
    Parser.symbol "0" |> Parser.andThen (\_ -> Parser.int)

{-|

    import Parser

    Parser.run (parseStringToChar '.') "foo.bar"
    x-> Ok "foo"


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


