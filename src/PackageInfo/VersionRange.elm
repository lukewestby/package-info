module PackageInfo.VersionRange
    exposing
        ( VersionRange
        , enclosing
        , contains
        , toString
        , fromString
        , decoder
        , encoder
        )

{-| A type and functions for managing version range constraint expressions found
in an elm-package.json file

# Type
@docs VersionRange

# Creation
@docs enclosing

# Comparison
@docs contains

# Strings
@docs toString, fromString

# JSON
@docs decoder, encoder
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Regex exposing (Regex)
import Utils exposing (resultAndMap)
import PackageInfo.Version as Version exposing (Version)


{-| Represents the contents of a version range constraint, expressed as
"X1.Y1.Z1 <= v < X2.Y2.Z2" in an elm-package.json file
-}
type alias VersionRange =
    { minimum : Version
    , maximum : Version
    }


{-| Create a VersionRange that starts at the given Version and ends at the next
highest major version from the given Version
-}
enclosing : Version -> VersionRange
enclosing version =
    let
        nextMajor =
            Version (version.major + 1) 0 0
    in
        VersionRange version nextMajor


splitRegex : Regex
splitRegex =
    Regex.regex "\\s*<=\\s*v\\s*<\\s*"


parseSegment : String -> Maybe String -> Result String Version
parseSegment side =
    Result.fromMaybe ("Missing " ++ side ++ " constraint")
        >> (flip Result.andThen) (Version.fromString)
        >> Result.formatError (\e -> "Unparseable " ++ side ++ " constraint: " ++ e)


{-| Parse a VersionRange from a string with the format
"X1.Y1.Z1 <= v < X2.Y2.Z2"
-}
fromString : String -> Result String VersionRange
fromString input =
    let
        split =
            Regex.split (Regex.AtMost 1) splitRegex input

        min =
            split |> List.head |> parseSegment "lefthand"

        max =
            split |> List.drop 1 |> List.head |> parseSegment "righthand"
    in
        Ok VersionRange
            `resultAndMap` min
            `resultAndMap` max


{-| Convert an existing VersionRange back into a string with the format
"X1.Y1.Z1 <= v < X2.Y2.Z2"
-}
toString : VersionRange -> String
toString { minimum, maximum } =
    (Version.toString minimum) ++ " <= v < " ++ (Version.toString maximum)


{-| Determine whether the given Version falls inside of the given VersionRange
-}
contains : Version -> VersionRange -> Bool
contains version { minimum, maximum } =
    (Version.compare version minimum /= LT) && (Version.compare version maximum == LT)


{-| Decode a JSON string of the format "X1.Y1.Z1 <= v < X2.Y2.Z2" as a
VersionRange
-}
decoder : Decoder VersionRange
decoder =
    Decode.customDecoder Decode.string fromString


{-| Encode a VersionRange as a JSON string with the format
"X1.Y1.Z1 <= v < X2.Y2.Z2"
-}
encoder : VersionRange -> Encode.Value
encoder =
    toString >> Encode.string
