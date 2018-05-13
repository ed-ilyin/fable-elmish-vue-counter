module Fable.Helpers.Vue
open Fable.Core
open Fable.Import

type VNodeThunk =
    VNodeThunk of (Vue.CreateElement -> U3<VNode.VNode, string, VNode.VNodeChildrenArrayContents>)

type CSSProp =
    | Color of string
    | BackgroundColor of string

type EventHandler =
    | Click of (obj -> unit)

type ComponentOptions<'Props,'State> =
    | El of U2<Browser.Element, string>
    | Props of 'Props
    | Render of (Vue.CreateElement -> VNode.VNode)

type DataProp =
    | Style of CSSProp list
    | On of EventHandler list

let inline Style (css: CSSProp list): DataProp =
    unbox ("style", JsInterop.keyValueList CaseRules.LowerFirst css)

let inline On (handlers: EventHandler list): DataProp =
    unbox ("on", JsInterop.keyValueList CaseRules.LowerFirst handlers)

let str s = VNodeThunk <| fun _ -> U3.Case2 s

let vNode tag (data: DataProp list) children =
    VNodeThunk <| fun createElement ->
        createElement.Invoke (
            U4.Case1 tag,
            JsInterop.keyValueList CaseRules.LowerFirst data |> unbox,
            List.map
                (fun (VNodeThunk t) -> t createElement)
                children
                |> Array.ofList
                |> unbox
                |> U3.Case1
        ) |> U3.Case1

let span = vNode "span"
let div = vNode "div"
let button = vNode "button"
