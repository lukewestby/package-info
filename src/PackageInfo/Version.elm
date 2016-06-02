module PackageInfo.Version
    exposing
        ( Version
        , compare
        , toString
        , fromString
        , decoder
        , encoder
        )

{-| A type and functions for managing SemVer version strings found in an
elm-package.json file

# Type
@docs Version

# Comparison
@docs compare

# Strings
@docs toString, fromString

# JSON
@docs decoder, encoder
-}

import String
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Utils exposing (resultAndMap)


{-| Represents the contents of a SemVer version
-}
type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


parseSegment : String -> Maybe String -> Result String Int
parseSegment versionType =
    Result.fromMaybe ("Missing " ++ versionType ++ " version component")
        >> (flip Result.andThen) String.toInt
        >> Result.formatError (always ("Unparseable " ++ versionType ++ " component"))


{-| Parse a Version from a string X.Y.Z, where X is the major version, Y is the
minor version, and Z is the patch version. Subversions of format X.Y.Z-alpha.1
are not supported.
-}
fromString : String -> Result String Version
fromString input =
    let
        split =
            String.split "." input

        major =
            split |> List.head |> parseSegment "major"

        minor =
            split |> List.drop 1 |> List.head |> parseSegment "minor"

        patch =
            split |> List.drop 2 |> List.head |> parseSegment "patch"
    in
        Ok Version
            `resultAndMap` major
            `resultAndMap` minor
            `resultAndMap` patch


{-| Convert a value of type Version back into a string with the format X.Y.Z
-}
toString : Version -> String
toString { major, minor, patch } =
    [ major, minor, patch ]
        |> List.map Basics.toString
        |> String.join "."


{-| Assess whether a given version is greater than the other based on the SemVer
scheme.
-}
compare : Version -> Version -> Order
compare left right =
    if (left.major /= right.major) then
        Basics.compare left.major right.major
    else if (left.minor /= right.minor) then
        Basics.compare left.minor right.minor
    else
        Basics.compare left.patch right.patch


{-| Parse a JSON string of format X.Y.Z as a Version
-}
decoder : Decoder Version
decoder =
    Decode.customDecoder Decode.string fromString


{-| Convert a Version back into a JSON string value
-}
encoder : Version -> Encode.Value
encoder =
    toString >> Encode.string
