namespace rec Fable.Import
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

module Options =
    type Vue = Vue.Vue
    type CreateElement = Vue.CreateElement
    // type CombinedVueInstance = __vue.CombinedVueInstance
    type VNode = VNode.VNode
    type VNodeData = VNode.VNodeData
    type VNodeDirective = VNode.VNodeDirective

    // type [<AllowNullLiteral>] Constructor =
    //     [<Emit "new $0($1...)">] abstract Create: [<ParamArray>] args: ResizeArray<obj option> -> obj

    type Component<'Methods, 'Computed, 'Props> =
        Component<obj, 'Methods, 'Computed, 'Props>

    type Component<'Computed, 'Props> =
        Component<obj, obj, 'Computed, 'Props>

    type Component<'Props> =
        Component<obj, obj, obj, 'Props>

    type Component =
        Component<obj, obj, obj, obj>

    type Component<'Data, 'Methods, 'Computed, 'Props> =
        U3<obj, FunctionalComponentOptions<'Props>, ComponentOptions<obj, 'Data, 'Methods, 'Computed, 'Props>>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module Component =
    //     let ofObj v: Component<'Data, 'Methods, 'Computed, 'Props> = v |> U3.Case1
    //     let isObj (v: Component<'Data, 'Methods, 'Computed, 'Props>) = match v with U3.Case1 _ -> true | _ -> false
    //     let asObj (v: Component<'Data, 'Methods, 'Computed, 'Props>) = match v with U3.Case1 o -> Some o | _ -> None
    //     let ofFunctionalComponentOptions v: Component<'Data, 'Methods, 'Computed, 'Props> = v |> U3.Case2
    //     let isFunctionalComponentOptions (v: Component<'Data, 'Methods, 'Computed, 'Props>) = match v with U3.Case2 _ -> true | _ -> false
    //     let asFunctionalComponentOptions (v: Component<'Data, 'Methods, 'Computed, 'Props>) = match v with U3.Case2 o -> Some o | _ -> None
    //     let ofComponentOptions v: Component<'Data, 'Methods, 'Computed, 'Props> = v |> U3.Case3
    //     let isComponentOptions (v: Component<'Data, 'Methods, 'Computed, 'Props>) = match v with U3.Case3 _ -> true | _ -> false
    //     let asComponentOptions (v: Component<'Data, 'Methods, 'Computed, 'Props>) = match v with U3.Case3 o -> Some o | _ -> None

    type [<AllowNullLiteral>] EsModuleComponent =
        abstract ``default``: Component with get, set

    type AsyncComponent<'Methods, 'Computed, 'Props> =
        AsyncComponent<obj, 'Methods, 'Computed, 'Props>

    type AsyncComponent<'Computed, 'Props> =
        AsyncComponent<obj, obj, 'Computed, 'Props>

    type AsyncComponent<'Props> =
        AsyncComponent<obj, obj, obj, 'Props>

    type AsyncComponent =
        AsyncComponent<obj, obj, obj, obj>

    type [<AllowNullLiteral>] AsyncComponent<'Data, 'Methods, 'Computed, 'Props> =
        [<Emit "$0($1...)">] abstract Invoke: resolve: (Component<'Data, 'Methods, 'Computed, 'Props> -> unit) * reject: (obj option -> unit) -> U2<Promise<U2<Component, EsModuleComponent>>, unit>

    // type [<AllowNullLiteral>] Accessors<'T> =
    //     interface end

    // type DataDef<'Data, 'Props, 'V> =
    //     U2<'Data, (obj -> 'Data)>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module DataDef =
    //     let ofData v: DataDef<'Data, 'Props, 'V> = v |> U2.Case1
    //     let isData (v: DataDef<'Data, 'Props, 'V>) = match v with U2.Case1 _ -> true | _ -> false
    //     let asData (v: DataDef<'Data, 'Props, 'V>) = match v with U2.Case1 o -> Some o | _ -> None
    //     let ofCase2 v: DataDef<'Data, 'Props, 'V> = v |> U2.Case2
    //     let isCase2 (v: DataDef<'Data, 'Props, 'V>) = match v with U2.Case2 _ -> true | _ -> false
    //     let asCase2 (v: DataDef<'Data, 'Props, 'V>) = match v with U2.Case2 o -> Some o | _ -> None

    type [<AllowNullLiteral>] ThisTypedComponentOptionsWithArrayProps<'V, 'Data, 'Methods, 'Computed, 'PropNames> =
        interface end

    type [<AllowNullLiteral>] ThisTypedComponentOptionsWithRecordProps<'V, 'Data, 'Methods, 'Computed, 'Props> =
        interface end

    // type DefaultData<'V> =
    //     U2<obj, ('V -> obj)>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module DefaultData =
    //     let ofObj v: DefaultData<'V> = v |> U2.Case1
    //     let isObj (v: DefaultData<'V>) = match v with U2.Case1 _ -> true | _ -> false
    //     let asObj (v: DefaultData<'V>) = match v with U2.Case1 o -> Some o | _ -> None
    //     let ofCase2 v: DefaultData<'V> = v |> U2.Case2
    //     let isCase2 (v: DefaultData<'V>) = match v with U2.Case2 _ -> true | _ -> false
    //     let asCase2 (v: DefaultData<'V>) = match v with U2.Case2 o -> Some o | _ -> None

    // type DefaultProps =
    //     Record<string, obj option>

    // type [<AllowNullLiteral>] DefaultMethods<'V> =
    //     [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> ('V -> ResizeArray<obj option> -> obj option) with get, set

    // type [<AllowNullLiteral>] DefaultComputed =
    //     [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> obj option with get, set

    type ComponentOptions<'Data, 'Methods, 'Computed, 'PropsDef, 'Props> =
        ComponentOptions<obj, 'Data, 'Methods, 'Computed, 'PropsDef, 'Props>

    type ComponentOptions<'Methods, 'Computed, 'PropsDef, 'Props> =
        ComponentOptions<obj, obj, 'Methods, 'Computed, 'PropsDef, 'Props>

    type ComponentOptions<'Computed, 'PropsDef, 'Props> =
        ComponentOptions<obj, obj, obj, 'Computed, 'PropsDef, 'Props>

    type ComponentOptions<'PropsDef, 'Props> =
        ComponentOptions<obj, obj, obj, obj, 'PropsDef, 'Props>

    type ComponentOptions<'Props> =
        ComponentOptions<obj, obj, obj, obj, obj, 'Props>

    type [<AllowNullLiteral>] ComponentOptions<'V, 'Data, 'Methods, 'Computed, 'PropsDef, 'Props> =
        abstract data: 'Data option with get, set
        abstract props: 'PropsDef option with get, set
        abstract propsData: obj option with get, set
        // abstract computed: Accessors<'Computed> option with get, set
        abstract methods: 'Methods option with get, set
        // abstract watch: Record<string, U3<WatchOptionsWithHandler<obj option>, WatchHandler<obj option>, string>> option with get, set
        abstract el: U2<Element, string> option with get, set
        abstract template: string option with get, set
        abstract render: createElement: CreateElement * hack: RenderContext<'Props> -> VNode
        // abstract renderError: ((unit -> VNode) -> Error -> VNode) option with get, set
        // abstract staticRenderFns: ResizeArray<(CreateElement -> VNode)> option with get, set
        abstract beforeCreate: this: 'V -> unit
        abstract created: unit -> unit
        abstract beforeDestroy: unit -> unit
        abstract destroyed: unit -> unit
        abstract beforeMount: unit -> unit
        abstract mounted: unit -> unit
        abstract beforeUpdate: unit -> unit
        abstract updated: unit -> unit
        abstract activated: unit -> unit
        abstract deactivated: unit -> unit
        // abstract errorCaptured: err: Error * vm: Vue * info: string -> U2<bool, unit>
        abstract directives: obj option with get, set
        abstract components: obj option with get, set
        abstract transitions: obj option with get, set
        abstract filters: obj option with get, set
        abstract provide: U2<obj, (unit -> obj)> option with get, set
        // abstract inject: InjectOptions option with get, set
        abstract model: obj option with get, set
        // abstract parent: Vue option with get, set
        // abstract mixins: ResizeArray<U2<ComponentOptions<Vue>, obj>> option with get, set
        abstract name: string option with get, set
        // abstract extends: U2<ComponentOptions<Vue>, obj> option with get, set
        abstract delimiters: string * string option with get, set
        abstract comments: bool option with get, set
        abstract inheritAttrs: bool option with get, set

    type FunctionalComponentOptions<'PropDefs> =
        FunctionalComponentOptions<obj, 'PropDefs>

    type FunctionalComponentOptions =
        FunctionalComponentOptions<obj, obj>

    type [<AllowNullLiteral>] FunctionalComponentOptions<'Props, 'PropDefs> =
        abstract name: string option with get, set
        abstract props: 'PropDefs option with get, set
        // abstract inject: InjectOptions option with get, set
        abstract functional: bool with get, set
        // abstract render: this: obj * createElement: CreateElement * context: RenderContext<'Props> -> VNode

    type RenderContext =
        RenderContext<obj>

    type [<AllowNullLiteral>] RenderContext<'Props> =
        abstract props: 'Props with get, set
        abstract children: ResizeArray<VNode> with get, set
        abstract slots: unit -> obj option
        abstract data: VNodeData with get, set
        abstract parent: Vue with get, set
        abstract listeners: obj with get, set
        abstract injections: obj option with get, set

    // type Prop<'T> =
    //     U2<(unit -> 'T), (ResizeArray<obj option> -> obj)>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module Prop =
    //     let ofInvoke v: Prop<'T> = v |> U2.Case1
    //     let isInvoke (v: Prop<'T>) = match v with U2.Case1 _ -> true | _ -> false
    //     let asInvoke (v: Prop<'T>) = match v with U2.Case1 o -> Some o | _ -> None
    //     let ofCreate v: Prop<'T> = v |> U2.Case2
    //     let isCreate (v: Prop<'T>) = match v with U2.Case2 _ -> true | _ -> false
    //     let asCreate (v: Prop<'T>) = match v with U2.Case2 o -> Some o | _ -> None

    // type PropValidator<'T> =
    //     U3<PropOptions<'T>, Prop<'T>, ResizeArray<Prop<'T>>>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module PropValidator =
    //     let ofPropOptions v: PropValidator<'T> = v |> U3.Case1
    //     let isPropOptions (v: PropValidator<'T>) = match v with U3.Case1 _ -> true | _ -> false
    //     let asPropOptions (v: PropValidator<'T>) = match v with U3.Case1 o -> Some o | _ -> None
    //     let ofProp v: PropValidator<'T> = v |> U3.Case2
    //     let isProp (v: PropValidator<'T>) = match v with U3.Case2 _ -> true | _ -> false
    //     let asProp (v: PropValidator<'T>) = match v with U3.Case2 o -> Some o | _ -> None
    //     let ofPropArray v: PropValidator<'T> = v |> U3.Case3
    //     let isPropArray (v: PropValidator<'T>) = match v with U3.Case3 _ -> true | _ -> false
    //     let asPropArray (v: PropValidator<'T>) = match v with U3.Case3 o -> Some o | _ -> None

    // type PropOptions =
    //     PropOptions<obj>

    // type [<AllowNullLiteral>] PropOptions<'T> =
    //     abstract ``type``: U2<Prop<'T>, ResizeArray<Prop<'T>>> option with get, set
    //     abstract required: bool option with get, set
    //     abstract ``default``: U2<'T, (unit -> obj)> option with get, set
    //     abstract validator: value: 'T -> bool

    // type [<AllowNullLiteral>] RecordPropsDefinition<'T> =
    //     interface end

    // type ArrayPropsDefinition<'T> =
    //     ResizeArray<obj>

    // type PropsDefinition<'T> =
    //     U2<ArrayPropsDefinition<'T>, RecordPropsDefinition<'T>>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module PropsDefinition =
    //     let ofArrayPropsDefinition v: PropsDefinition<'T> = v |> U2.Case1
    //     let isArrayPropsDefinition (v: PropsDefinition<'T>) = match v with U2.Case1 _ -> true | _ -> false
    //     let asArrayPropsDefinition (v: PropsDefinition<'T>) = match v with U2.Case1 o -> Some o | _ -> None
    //     let ofRecordPropsDefinition v: PropsDefinition<'T> = v |> U2.Case2
    //     let isRecordPropsDefinition (v: PropsDefinition<'T>) = match v with U2.Case2 _ -> true | _ -> false
    //     let asRecordPropsDefinition (v: PropsDefinition<'T>) = match v with U2.Case2 o -> Some o | _ -> None

    // type [<AllowNullLiteral>] ComputedOptions<'T> =
    //     abstract get: unit -> 'T
    //     abstract set: value: 'T -> unit
    //     abstract cache: bool option with get, set

    // type [<AllowNullLiteral>] WatchHandler<'T> =
    //     [<Emit "$0($1...)">] abstract Invoke: ``val``: 'T * oldVal: 'T -> unit

    type [<AllowNullLiteral>] WatchOptions =
        abstract deep: bool option with get, set
        abstract immediate: bool option with get, set

    // type [<AllowNullLiteral>] WatchOptionsWithHandler<'T> =
    //     inherit WatchOptions
    //     abstract handler: WatchHandler<'T> with get, set

    // type [<AllowNullLiteral>] DirectiveFunction =
    //     [<Emit "$0($1...)">] abstract Invoke: el: HTMLElement * binding: VNodeDirective * vnode: VNode * oldVnode: VNode -> unit

    // type [<AllowNullLiteral>] DirectiveOptions =
    //     abstract bind: DirectiveFunction option with get, set
    //     abstract inserted: DirectiveFunction option with get, set
    //     abstract update: DirectiveFunction option with get, set
    //     abstract componentUpdated: DirectiveFunction option with get, set
    //     abstract unbind: DirectiveFunction option with get, set

    // type InjectKey =
    //     U2<string, Symbol>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module InjectKey =
    //     let ofString v: InjectKey = v |> U2.Case1
    //     let isString (v: InjectKey) = match v with U2.Case1 _ -> true | _ -> false
    //     let asString (v: InjectKey) = match v with U2.Case1 o -> Some o | _ -> None
    //     let ofSymbol v: InjectKey = v |> U2.Case2
    //     let isSymbol (v: InjectKey) = match v with U2.Case2 _ -> true | _ -> false
    //     let asSymbol (v: InjectKey) = match v with U2.Case2 o -> Some o | _ -> None

    // type InjectOptions =
    //     U2<obj, ResizeArray<string>>

    // [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    // module InjectOptions =
    //     let ofItem v: InjectOptions = v |> U2.Case1
    //     let isItem (v: InjectOptions) = match v with U2.Case1 _ -> true | _ -> false
    //     let asItem (v: InjectOptions) = match v with U2.Case1 o -> Some o | _ -> None
    //     let ofStringArray v: InjectOptions = v |> U2.Case2
    //     let isStringArray (v: InjectOptions) = match v with U2.Case2 _ -> true | _ -> false
    //     let asStringArray (v: InjectOptions) = match v with U2.Case2 o -> Some o | _ -> None

module Plugin =
    // type Vue as _Vue = __vue.Vue as _Vue

    type [<AllowNullLiteral>] PluginFunction<'T> =
        [<Emit "$0($1...)">] abstract Invoke: Vue: obj * ?options: 'T -> unit

    type [<AllowNullLiteral>] PluginObject<'T> =
        abstract install: PluginFunction<'T> with get, set
        [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> obj option with get, set

module VNode =
    open System
    open Fable.Core
    open Fable.Import.JS

    type Vue = Vue.Vue

    type [<AllowNullLiteral>] ScopedSlot =
        [<Emit "$0($1...)">] abstract Invoke: props: obj option -> U2<VNodeChildrenArrayContents, string>

    type VNodeChildren =
        U3<VNodeChildrenArrayContents, ScopedSlot, string>

    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module VNodeChildren =
        let ofVNodeChildrenArrayContents v: VNodeChildren = v |> U3.Case1
        let isVNodeChildrenArrayContents (v: VNodeChildren) = match v with U3.Case1 _ -> true | _ -> false
        let asVNodeChildrenArrayContents (v: VNodeChildren) = match v with U3.Case1 o -> Some o | _ -> None
        let ofCase2 v: VNodeChildren = v |> U3.Case2
        let isCase2 (v: VNodeChildren) = match v with U3.Case2 _ -> true | _ -> false
        let asCase2 (v: VNodeChildren) = match v with U3.Case2 o -> Some o | _ -> None
        let ofString v: VNodeChildren = v |> U3.Case3
        let isString (v: VNodeChildren) = match v with U3.Case3 _ -> true | _ -> false
        let asString (v: VNodeChildren) = match v with U3.Case3 o -> Some o | _ -> None

    type [<AllowNullLiteral>] VNodeChildrenArrayContents =
        inherit Array<U3<VNode, string, VNodeChildrenArrayContents>>

    type [<AllowNullLiteral>] VNode =
        abstract tag: string option with get, set
        abstract data: VNodeData option with get, set
        abstract children: ResizeArray<VNode> option with get, set
        abstract text: string option with get, set
        abstract elm: Node option with get, set
        abstract ns: string option with get, set
        abstract context: Vue option with get, set
        abstract key: U2<string, float> option with get, set
        abstract componentOptions: VNodeComponentOptions option with get, set
        abstract componentInstance: Vue option with get, set
        abstract parent: VNode option with get, set
        abstract raw: bool option with get, set
        abstract isStatic: bool option with get, set
        abstract isRootInsert: bool with get, set
        abstract isComment: bool with get, set

    type [<AllowNullLiteral>] VNodeComponentOptions =
        abstract Ctor: obj with get, set
        abstract propsData: obj option with get, set
        abstract listeners: obj option with get, set
        abstract children: VNodeChildren option with get, set
        abstract tag: string option with get, set

    type [<AllowNullLiteral>] VNodeData =
        abstract key: U2<string, float> option with get, set
        abstract slot: string option with get, set
        abstract scopedSlots: obj option with get, set
        abstract ref: string option with get, set
        abstract tag: string option with get, set
        abstract staticClass: string option with get, set
        abstract ``class``: obj option with get, set
        abstract staticStyle: obj option with get, set
        abstract style: U2<ResizeArray<obj>, obj> option with get, set
        abstract props: obj option with get, set
        abstract attrs: obj option with get, set
        abstract domProps: obj option with get, set
        abstract hook: obj option with get, set
        abstract on: obj option with get, set
        abstract nativeOn: obj option with get, set
        abstract transition: obj option with get, set
        abstract show: bool option with get, set
        abstract inlineTemplate: obj option with get, set
        abstract directives: ResizeArray<VNodeDirective> option with get, set
        abstract keepAlive: bool option with get, set

    type [<AllowNullLiteral>] VNodeDirective =
        abstract name: string
        abstract value: obj option
        abstract oldValue: obj option
        abstract expression: obj option
        abstract arg: string
        abstract modifiers: obj

module Vue =
    // type Component = __options.Component
    // type AsyncComponent = __options.AsyncComponent
    // type ComponentOptions = __options.ComponentOptions
    // type FunctionalComponentOptions = __options.FunctionalComponentOptions
    // type WatchOptionsWithHandler = __options.WatchOptionsWithHandler
    // type WatchHandler = __options.WatchHandler
    // type DirectiveOptions = __options.DirectiveOptions
    // type DirectiveFunction = __options.DirectiveFunction
    // type RecordPropsDefinition = __options.RecordPropsDefinition
    // type ThisTypedComponentOptionsWithArrayProps = __options.ThisTypedComponentOptionsWithArrayProps
    // type ThisTypedComponentOptionsWithRecordProps = __options.ThisTypedComponentOptionsWithRecordProps
    // type WatchOptions = __options.WatchOptions
    type VNode = VNode.VNode
    type VNodeData = VNode.VNodeData
    type VNodeChildren = VNode.VNodeChildren
    // type ScopedSlot = __vnode.ScopedSlot
    // type PluginFunction = __plugin.PluginFunction
    // type PluginObject = __plugin.PluginObject

    // type [<AllowNullLiteral>] IExports =
    //     abstract VueConstructor: VueConstructorStatic
    //     abstract Vue: VueConstructor

    type [<AllowNullLiteral>] CreateElement =
        [<Emit "$0($1...)">] abstract Invoke: ?tag: U4<string, Options.Component<obj option, obj option, obj option, obj option>, Options.AsyncComponent<obj option, obj option, obj option, obj option>, (unit -> Options.Component)> * ?children: VNodeChildren -> VNode
        [<Emit "$0($1...)">] abstract Invoke: ?tag: U4<string, Options.Component<obj option, obj option, obj option, obj option>, Options.AsyncComponent<obj option, obj option, obj option, obj option>, (unit -> Options.Component)> * ?data: VNodeData * ?children: VNodeChildren -> VNode

    type [<AllowNullLiteral>] Vue =
        abstract ``$el``: HTMLElement
        abstract ``$options``: Options.ComponentOptions<Vue>
        abstract ``$parent``: Vue
        abstract ``$root``: Vue
        abstract ``$children``: ResizeArray<Vue>
        abstract ``$refs``: obj
        abstract ``$slots``: obj
        abstract ``$scopedSlots``: obj
        abstract ``$isServer``: bool
        // abstract ``$data``: Record<string, obj option>
        // abstract ``$props``: Record<string, obj option>
        abstract ``$ssrContext``: obj option
        abstract ``$vnode``: VNode
        // abstract ``$attrs``: Record<string, string>
        // abstract ``$listeners``: Record<string, U2<Function, ResizeArray<Function>>>
        abstract ``$mount``: ?elementOrSelector: U2<Element, string> * ?hydrating: bool -> Vue
        abstract ``$forceUpdate``: unit -> unit
        abstract ``$destroy``: unit -> unit
        abstract ``$set``: obj with get, set
        abstract ``$delete``: obj with get, set
        // abstract ``$watch``: expOrFn: string * callback: (Vue -> obj option -> obj option -> unit) * ?options: WatchOptions -> (unit -> unit)
        // abstract ``$watch``: expOrFn: (Vue -> 'T) * callback: (Vue -> 'T -> 'T -> unit) * ?options: WatchOptions -> (unit -> unit)
        // abstract ``$on``: ``event``: U2<string, ResizeArray<string>> * callback: Function -> Vue
        // abstract ``$once``: ``event``: string * callback: Function -> Vue
        // abstract ``$off``: ?``event``: U2<string, ResizeArray<string>> * ?callback: Function -> Vue
        abstract ``$emit``: ``event``: string * [<ParamArray>] args: ResizeArray<obj option> -> Vue
        abstract ``$nextTick``: callback: (Vue -> unit) -> unit
        abstract ``$nextTick``: unit -> Promise<unit>
        abstract ``$createElement``: CreateElement with get, set

    // type [<AllowNullLiteral>] CombinedVueInstance<'Instance, 'Data, 'Methods, 'Computed, 'Props> =
    //     interface end

    // type ExtendedVue<'Instance, 'Data, 'Methods, 'Computed, 'Props> =
    //     VueConstructor<obj>

    // type [<AllowNullLiteral>] VueConfiguration =
    //     abstract silent: bool with get, set
    //     abstract optionMergeStrategies: obj option with get, set
    //     abstract devtools: bool with get, set
    //     abstract productionTip: bool with get, set
    //     abstract performance: bool with get, set
    //     abstract errorHandler: err: Error * vm: Vue * info: string -> unit
    //     abstract warnHandler: msg: string * vm: Vue * trace: string -> unit
    //     abstract ignoredElements: ResizeArray<U2<string, RegExp>> with get, set
    //     abstract keyCodes: obj with get, set

    type VueConstructor =
        VueConstructor<obj>

    type [<AllowNullLiteral>] VueConstructor<'V> =
    //     abstract extend: ?options: ThisTypedComponentOptionsWithArrayProps<'V, 'Data, 'Methods, 'Computed, 'PropNames> -> ExtendedVue<'V, 'Data, 'Methods, 'Computed, Record<'PropNames, obj option>>
    //     abstract extend: ?options: ThisTypedComponentOptionsWithRecordProps<'V, 'Data, 'Methods, 'Computed, 'Props> -> ExtendedVue<'V, 'Data, 'Methods, 'Computed, 'Props>
    //     abstract extend: definition: FunctionalComponentOptions<Record<'PropNames, obj option>, ResizeArray<'PropNames>> -> ExtendedVue<'V, obj, obj, obj, Record<'PropNames, obj option>>
    //     abstract extend: definition: FunctionalComponentOptions<'Props, RecordPropsDefinition<'Props>> -> ExtendedVue<'V, obj, obj, obj, 'Props>
    //     abstract extend: ?options: ComponentOptions<'V> -> ExtendedVue<'V, obj, obj, obj, obj>
    //     abstract nextTick: callback: (unit -> unit) * ?context: ResizeArray<obj option> -> unit
    //     abstract nextTick: unit -> Promise<unit>
    //     abstract set: ``object``: obj * key: string * value: 'T -> 'T
    //     abstract set: array: ResizeArray<'T> * key: float * value: 'T -> 'T
    //     abstract delete: ``object``: obj * key: string -> unit
    //     abstract delete: array: ResizeArray<'T> * key: float -> unit
    //     abstract directive: id: string * ?definition: U2<DirectiveOptions, DirectiveFunction> -> DirectiveOptions
    //     abstract filter: id: string * ?definition: Function -> Function
    //     abstract ``component``: id: string -> VueConstructor
    //     abstract ``component``: id: string * ``constructor``: 'VC -> 'VC
    //     abstract ``component``: id: string * definition: AsyncComponent<'Data, 'Methods, 'Computed, 'Props> -> ExtendedVue<'V, 'Data, 'Methods, 'Computed, 'Props>
    //     abstract ``component``: id: string * ?definition: ThisTypedComponentOptionsWithArrayProps<'V, 'Data, 'Methods, 'Computed, 'PropNames> -> ExtendedVue<'V, 'Data, 'Methods, 'Computed, Record<'PropNames, obj option>>
    //     abstract ``component``: id: string * ?definition: ThisTypedComponentOptionsWithRecordProps<'V, 'Data, 'Methods, 'Computed, 'Props> -> ExtendedVue<'V, 'Data, 'Methods, 'Computed, 'Props>
    //     abstract ``component``: id: string * definition: FunctionalComponentOptions<Record<'PropNames, obj option>, ResizeArray<'PropNames>> -> ExtendedVue<'V, obj, obj, obj, Record<'PropNames, obj option>>
    //     abstract ``component``: id: string * definition: FunctionalComponentOptions<'Props, RecordPropsDefinition<'Props>> -> ExtendedVue<'V, obj, obj, obj, 'Props>
    //     abstract ``component``: id: string * ?definition: ComponentOptions<'V> -> ExtendedVue<'V, obj, obj, obj, obj>
        abstract ``use``: plugin: U2<Plugin.PluginObject<'T>, Plugin.PluginFunction<'T>> * ?options: 'T -> unit
        abstract ``use``: plugin: U2<Plugin.PluginObject<obj option>, Plugin.PluginFunction<obj option>> * [<ParamArray>] options: ResizeArray<obj option> -> unit
    //     abstract ``mixin``: ``mixin``: U2<VueConstructor, ComponentOptions<Vue>> -> unit
    //     abstract compile: template: string -> obj
    //     abstract config: VueConfiguration with get, set

    type [<AllowNullLiteral>] VueConstructorStatic =
        // [<Emit "new $0($1...)">] abstract Create: ?options: Options.ThisTypedComponentOptionsWithArrayProps<'V, 'Data, 'Methods, 'Computed, 'PropNames> -> VueConstructor<'V>
        // [<Emit "new $0($1...)">] abstract Create: ?options: Options.ThisTypedComponentOptionsWithRecordProps<'V, 'Data, 'Methods, 'Computed, 'Props> -> VueConstructor<'V>
        [<Emit "new $0($1...)">] abstract Create: ?options: Options.ComponentOptions<'V> -> VueConstructor<'V>
        abstract ``use``: plugin: U2<Plugin.PluginObject<'T>, Plugin.PluginFunction<'T>> * ?options: 'T -> unit
