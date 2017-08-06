module PackageInfoTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import PackageInfo
import Json.Decode


elmPackageExample : String
elmPackageExample =
    """{
    "version": "2.0.0",
    "summary": "Types and helpers for dealing with the data found in an elm-package.json file",
    "repository": "https://github.com/lukewestby/package-info.git",
    "license": "MIT",
    "source-directories": [
        "./src"
    ],
    "exposed-modules": [
        "PackageInfo",
        "PackageInfo.Version",
        "PackageInfo.VersionRange"
    ],
    "dependencies": {
        "NoRedInk/elm-decode-pipeline": "3.0.0 <= v < 4.0.0",
        "elm-lang/core": "5.1.1 <= v < 6.0.0"
    },
    "elm-version": "0.18.0 <= v < 0.19.0"
}"""


suite : Test
suite =
    describe "PackageInfo"
        [ describe "PackageInfo.decoder"
            [ test "works on its own elm-package.json" <|
                \_ ->
                    elmPackageExample
                        |> Json.Decode.decodeString PackageInfo.decoder
                        |> Expect.equal
                            (Ok
                                { version =
                                    { major = 2
                                    , minor = 0
                                    , patch = 0
                                    }
                                , summary = "Types and helpers for dealing with the data found in an elm-package.json file"
                                , repository = "https://github.com/lukewestby/package-info.git"
                                , license = "MIT"
                                , sourceDirectories = [ "./src" ]
                                , exposedModules = [ "PackageInfo", "PackageInfo.Version", "PackageInfo.VersionRange" ]
                                , dependencies =
                                    [ { name = "elm-lang/core"
                                      , versionRange =
                                            { minimum = { major = 5, minor = 1, patch = 1 }
                                            , maximum = { major = 6, minor = 0, patch = 0 }
                                            }
                                      }
                                    , { name = "NoRedInk/elm-decode-pipeline"
                                      , versionRange =
                                            { minimum = { major = 3, minor = 0, patch = 0 }
                                            , maximum = { major = 4, minor = 0, patch = 0 }
                                            }
                                      }
                                    ]
                                , elmVersion =
                                    { minimum = { major = 0, minor = 18, patch = 0 }
                                    , maximum = { major = 0, minor = 19, patch = 0 }
                                    }
                                , nativeModules = False
                                }
                            )
            ]
        ]
