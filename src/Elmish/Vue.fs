[<RequireQualifiedAccess>]
module Elmish.Vue.Program
open Fable.Core
open Fable.Import
open Fable.Helpers.Vue
open Fable.Core.JsInterop

let[<Import("default","vue")>]vue: Vue.VueConstructorStatic = jsNative

[<Pojo>]
type Props = { state: VNodeThunk option }

let withVue element (program:Elmish.Program<_,_,_,_>) =
    let vm: Vue.VueConstructor<Props> =
        vue.Create (
            JsInterop.keyValueList CaseRules.LowerFirst [
                Props { state = None }
                U2.Case2 element |> El
                Render (fun createElement ->
                    let div c = createElement.Invoke (U4.Case1 "div", c)
                    match JsInterop.jsThis<Props>.state with
                    | None -> U3.Case3 "Пока пусто" |> div
                    | Some (VNodeThunk vNodeThunk) ->
                        match vNodeThunk createElement with
                        | U3.Case1 vNode -> vNode
                        | U3.Case2 string -> U3.Case3 string |> div
                        | U3.Case3 children -> U3.Case1 children |> div
                )
            ] |> unbox
        )
    let setState model dispatch =
        do vm?state <- program.view model dispatch |> Some
    { program with setState = setState }