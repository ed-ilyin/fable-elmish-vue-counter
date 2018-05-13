module Fable.Vue
open Elmish
open Elmish.Vue
open Fable.Helpers.Vue
open Fable.Import

type Model = { count: int }

let log (tag: string) (a: 'a) : 'a =
    do Browser.console.log (tag, a)
    a

let button color text dispatch msg =
    button [
        Style [ Color color ]
        On [ Click <| fun _ -> dispatch msg ]
    ] [ str text ]

type Msg = Decrease | Increase
let view model dispatch =
    div [] [
        button "blue" "-" dispatch Decrease
        string model.count |> str
        button "red" "+" dispatch Increase
    ]

let init () = { count = 1 }
let count model f = { model with count = f model.count 1 }

let update cmd model =
    match cmd with
    | Decrease -> count model (-)
    | Increase -> count model (+)

do Program.mkSimple init update view
    |> Program.withVue "#app"
    |> Program.run