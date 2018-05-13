module Fable.Vue
open Elmish
open Elmish.Vue
open Fable.Helpers.Vue

type Model = { count: int }
type Msg = Decrease | Increase | Reset
let init () = { count = 1 }

let button color text dispatch msg =
    button [
        Style [ BackgroundColor color; Color "white" ]
        On [ Click <| fun _ -> dispatch msg ]
    ] [ str text ]

let view model dispatch =
    div [] [
        button "blue" "-" dispatch Decrease
        div [] [ string model.count |> str ]
        button "red" "+" dispatch Increase
        button "green" "Reset" dispatch Reset
    ]

let update cmd model =
    let count f = { model with count = f model.count 1 }
    match cmd with
    | Decrease -> count (-)
    | Increase -> count (+)
    | Reset -> { model with count = 0 }

do Program.mkSimple init update view
    |> Program.withVue "#app"
    |> Program.run