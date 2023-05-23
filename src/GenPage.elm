module GenPage exposing (..)

import Bytes exposing (Bytes)
import Common exposing (nbsp)
import Length exposing (Length, Meters)
import Pdf exposing (..)
import Point2d exposing (Point2d)
import Types exposing (User)
import Vector2d


slide : List Item -> Page
slide contents =
    page
        { size =
            Vector2d.millimeters 210 297
        , contents = contents
        }


pos : Float -> Float -> Point2d Meters PageCoordinates
pos x y =
    Point2d.xy (Length.points x) (Length.points y)


defaultFont : Font
defaultFont =
    courier { bold = False, oblique = False }


outraFont : Font
outraFont =
    helvetica { bold = False, oblique = False }


margin : number
margin =
    5


titleFontSize : Length
titleFontSize =
    Length.points 20


normalFontSize : Length
normalFontSize =
    Length.points 12


nbsp : Char
nbsp =
    ' '


header_separator : String
header_separator =
    "|-------------------------------------------------------------------------------|"


another_header_separator : String
another_header_separator =
    "|------------------|---------------------|---------------------------------|----|"


table : List User -> List Item
table ul =
    List.append
        (List.append
            [ text
                titleFontSize
                outraFont
                (pos margin 12)
                "User List - Soundscapes"
            , text
                normalFontSize
                defaultFont
                (pos margin 50)
                header_separator
            , text
                normalFontSize
                defaultFont
                (pos margin 65)
                ("|"
                    ++ String.padRight 18 nbsp "ID"
                    ++ "|"
                    ++ String.padRight 21 nbsp "Username"
                    ++ "|"
                    ++ String.padRight 33 nbsp "Email"
                    ++ "|"
                    ++ String.padRight 4 nbsp "Role"
                    ++ "|\n"
                )
            , text
                normalFontSize
                defaultFont
                (pos margin 80)
                another_header_separator
            ]
            (List.indexedMap
                (\i user ->
                    text
                        normalFontSize
                        defaultFont
                        (pos margin ((Basics.toFloat (i + 1) * 15.0) + 80.0))
                        ("|"
                            ++ String.padRight 18 nbsp (String.dropRight 21 user.id ++ "...")
                            ++ "|"
                            ++ String.padRight 21 nbsp user.username
                            ++ "|"
                            ++ String.padRight 33 nbsp user.email
                            ++ "|"
                            ++ (user.role |> String.fromInt |> String.padRight 4 nbsp)
                            ++ "|"
                        )
                )
                ul
            )
        )
        [ text
            normalFontSize
            defaultFont
            (pos margin ((((List.length ul |> Basics.toFloat) + 1) * 15.0) + 80.0))
            header_separator
        ]


genPdf : List User -> Bytes
genPdf users =
    pdf
        { title = "Users list"
        , pages =
            [ slide (table users)

            -- (List.append
            --     [
            --     , text
            --         normalFontSize
            --         defaultFont
            --         (pos margin 25)
            --         ("Id "
            --             ++ " | Username "
            --             ++ " | Email "
            --             ++ " | Role"
            --         )
            --     ]
            -- )
            -- )
            ]
        }
        |> toBytes
