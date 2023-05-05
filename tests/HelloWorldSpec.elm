module HelloWorldSpec exposing (suite)

import Mood exposing (mood)
import Msg
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Html


suite : Test
suite =
    Test.describe "HelloWorld"
        [ Test.test "renders and runs helloWorld" <|
            \_ ->
                mood 0
                    |> Query.fromHtml
                    |> Query.has [ Html.text "Hello, Vite + Elm!" ]
        , Test.test "Displays the current count" <|
            \_ ->
                mood 5
                    |> Query.fromHtml
                    |> Query.has [ Html.text "Count is: 5" ]
        , Test.test "clicking on the + button sends an increment message" <|
            \_ ->
                mood 0
                    |> Query.fromHtml
                    |> Query.find [ Html.tag "button", Html.containing [ Html.text "+" ] ]
                    |> Event.simulate Event.click
                    |> Event.expect Msg.Increment
        , Test.test "clicking on the - button sends a decrement message" <|
            \_ ->
                mood 0
                    |> Query.fromHtml
                    |> Query.find [ Html.tag "button", Html.containing [ Html.text "-" ] ]
                    |> Event.simulate Event.click
                    |> Event.expect Msg.Decrement
        ]
