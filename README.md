## Typed Time

This library provides functions for working with typed time.
For example,

    equal (seconds 60) (minutes 1)
    --> True

    add (hours 1) (minutes 20) |> toString Minutes
    --> "01:20"

    add (hours 1) (minutes 20) |> toString Seconds
    --> "01:20:00"

To construct TypedTime values, use

    milliseconds : Float -> TypedTime
    seconds : Float -> TypedTime
    minutes : Float -> TypedTime
    hours : Float -> TypedTime
    
You can also construct such values from strings, e.g., 

    fromString Seconds "4:23"
    --> Just (add (minutes 4) (seconds 23))

    