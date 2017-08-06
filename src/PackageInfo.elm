module PackageInfo
    exposing
        ( PackageInfo
        , Dependency
        , decoder
        , encoder
        )

{-| Types and JSON helpers for loading and manipulating the contents of an
elm-package.json file


# Types

@docs PackageInfo, Dependency


# JSON

@docs decoder, encoder

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import PackageInfo.Version as Version exposing (Version)
import PackageInfo.VersionRange as VersionRange exposing (VersionRange)


{-| A dependency of a package, by name and version constraints
-}
type alias Dependency =
    { name : String
    , versionRange : VersionRange
    }


{-| All data that can be found in an elm-package.json file
-}
type alias PackageInfo =
    { version : Version
    , summary : String
    , repository : String
    , license : String
    , sourceDirectories : List String
    , exposedModules : List String
    , dependencies : List Dependency
    , elmVersion : VersionRange
    , nativeModules : Bool
    }


dependenciesDecoder : Decoder (List Dependency)
dependenciesDecoder =
    let
        mapPairs list =
            List.map (\( name, versionRange ) -> Dependency name versionRange) list
    in
        Decode.keyValuePairs VersionRange.decoder
            |> Decode.map mapPairs


{-| A decoder for the contents of an elm-package.json file
-}
decoder : Decoder PackageInfo
decoder =
    Pipeline.decode PackageInfo
        |> Pipeline.required "version" Version.decoder
        |> Pipeline.required "summary" Decode.string
        |> Pipeline.required "repository" Decode.string
        |> Pipeline.required "license" Decode.string
        |> Pipeline.required "source-directories" (Decode.list Decode.string)
        |> Pipeline.required "exposed-modules" (Decode.list Decode.string)
        |> Pipeline.required "dependencies" dependenciesDecoder
        |> Pipeline.required "elm-version" VersionRange.decoder
        |> Pipeline.optional "native-modules" Decode.bool False


dependenciesEncoder : List Dependency -> Encode.Value
dependenciesEncoder deps =
    deps
        |> List.map (\dep -> ( dep.name, VersionRange.encoder dep.versionRange ))
        |> Encode.object


{-| An encoder to generate the JSON expected to be in an elm-package.json file
from the PackageInfo type
-}
encoder : PackageInfo -> Encode.Value
encoder packageInfo =
    Encode.object
        [ ( "version", Version.encoder packageInfo.version )
        , ( "summary", Encode.string packageInfo.summary )
        , ( "repository", Encode.string packageInfo.repository )
        , ( "license", Encode.string packageInfo.license )
        , ( "source-directories", Encode.list <| List.map Encode.string packageInfo.sourceDirectories )
        , ( "exposed-modules", Encode.list <| List.map Encode.string packageInfo.exposedModules )
        , ( "dependencies", dependenciesEncoder packageInfo.dependencies )
        , ( "elm-version", VersionRange.encoder packageInfo.elmVersion )
        , ( "native-modules", Encode.bool packageInfo.nativeModules )
        ]
