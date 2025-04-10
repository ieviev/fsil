[<AutoOpen>]
module Fsil

open System.Runtime.CompilerServices
open System
// constrained type variables
#nowarn "64"

[<AbstractClass>]
module Internal =

    [<AbstractClass; Sealed>]
    type Default =

        static member inline Default
            (_f: option<'t> -> unit)
            : option<'t> =
            None

        static member inline Default
            (_f: voption<'t> -> unit)
            : voption<'t> =
            ValueNone

        static member inline Default
            (_f: Result<'t, ^U> -> unit)
            : Result<'t, 'u> =
            let inline call source =
                ((^U or Default): (static member Default: _ -> ^U) ((source,
                                                                     _f)))

            Error(call (fun (_: ^U) -> ()))

        static member inline Default
            (_f: ResizeArray<'t> -> unit)
            : ResizeArray<'t> =
            ResizeArray<'t>()

        static member inline Default
            (_f: list<'t> -> unit)
            : list<'t> =
            []

        static member inline Default
            (_f: array<'t> -> unit)
            : array<'t> =
            [||]

        static member inline Default(_f: byte -> unit) : byte = 0uy

        static member inline Default(_f: char -> unit) : char = char 0

        static member inline Default(_f: uint -> unit) : uint = 0u

        static member inline Default(_f: int -> unit) : int = 0

        static member inline Default(_f: bool -> unit) : bool = false

        static member inline Default(_f: string -> unit) : string = ""

        static member inline Default
            (_f: System.Guid -> unit)
            : System.Guid =
            System.Guid()

        static member inline Invoke< ^I
            when (^I or Default): (static member Default:
                (^I -> unit) -> ^I)>
            ()
            : ^I =
            ((^I or Default): (static member Default:
                (^I -> unit) -> ^I) (fun _ -> ()))


    [<AbstractClass; Sealed>]
    type IsEmpty =
        static member inline IsEmpty
            ((x: option<'t>, _f: unit -> unit))
            : bool =
            x.IsNone

        static member inline IsEmpty
            ((x: voption<'t>, _f: unit -> unit))
            : bool =
            x.IsNone

        static member inline IsEmpty
            ((x: Result<'t, _>, _f: unit -> unit))
            : bool =
            match x with
            | Ok(_) -> false
            | _ -> true

        static member inline IsEmpty
            ((x: ResizeArray<'t>, _f: unit -> unit))
            : bool =
            x.Count = 0

        static member inline IsEmpty
            ((x: list<'t>, _f: unit -> unit))
            : bool =
            x.IsEmpty

        static member inline IsEmpty
            ((x: array<'t>, _f: unit -> unit))
            : bool =
            x.Length = 0

        static member inline Invoke
            (_f: unit -> unit)
            (source: 'I)
            : bool =

            let inline call source =
                ((^I or IsEmpty): (static member IsEmpty:
                    (_ * _) -> bool) ((source, _f)))

            call source


    [<AbstractClass; Sealed>]
    type Length =
        static member inline Length
            ((x: option<'t>, _f: unit -> unit))
            : int =
            if x.IsSome then 1 else 0

        static member inline Length
            ((x: voption<'t>, _f: unit -> unit))
            : int =
            if x.IsSome then 1 else 0

        static member inline Length
            ((x: Result<'t, _>, _f: unit -> unit))
            : int =
            match x with
            | Ok(_) -> 1
            | _ -> 0

        static member inline Length
            ((x: ResizeArray<'t>, _f: unit -> unit))
            : int =
            x.Count

        static member inline Length
            ((x: list<'t>, _f: unit -> unit))
            : int =
            x.Length

        static member inline Length
            ((x: array<'t>, _f: unit -> unit))
            : int =
            x.Length

        static member inline Invoke
            (_f: unit -> unit)
            (source: 'I)
            : int =

            let inline call source =
                ((^I or Length): (static member Length: (_ * _) -> int) ((source,
                                                                          _f)))

            call source

    [<AbstractClass; Sealed>]
    type IterateWhile =

        static member inline IterateWhile
            (
                x: 't[],
                cond: byref<bool>,
                [<InlineIfLambda>] f: 't -> unit
            ) : unit =
            let mutable i = 0

            while cond && i < x.Length do
                f x[i]
                i <- i + 1

        static member inline IterateWhile
            (
                x: Span<'t>,
                cond: byref<bool>,
                [<InlineIfLambda>] f: 't -> unit
            ) : unit =
            let mutable i = 0

            while cond && i < x.Length do
                f x[i]
                i <- i + 1

        static member inline IterateWhile
            (
                x: list<'t>,
                cond: byref<bool>,
                [<InlineIfLambda>] f: 't -> unit
            ) : unit =
            let mutable curr = x

            while cond && not x.IsEmpty do
                f curr.Head
                curr <- curr.Tail


        static member inline Invoke
            (
                source: 'I,
                cond: byref<bool>,
                [<InlineIfLambda>] action: 't -> unit
            ) : unit =
            ((^I or IterateWhile): (static member IterateWhile:
                ^I * byref<bool> * (^t -> unit) -> unit) (source,
                                                          &cond,
                                                          action))


    [<AbstractClass; Sealed>]
    type Iterate =

        static member inline Iterate
            (x: option<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            Option.iter f x

        static member inline Iterate
            (x: voption<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            ValueOption.iter f x

        static member inline Iterate
            (x: 't[], [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (x: Result<'t, _>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            match x with
            | Ok v -> f v
            | _ -> ()

        static member inline Iterate
            (x: list<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            List.iter f x

        static member inline Iterate
            (x: System.Span<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (
                x: System.ReadOnlySpan<'t>,
                [<InlineIfLambda>] f: 't -> unit
            ) : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        // this breaks down the type system
        // would want to have ResizeArray here
        // static member inline Iterate
        //     (
        //         x: System.Collections.Generic.List<'t>,
        //         [<InlineIfLambda>] f: 't -> unit
        //     ) : unit =
        //     let mutable i = 0

        //     while i < x.Count do
        //         f x[i]
        //         i <- i + 1

        static member inline Invoke
            ([<InlineIfLambda>] action: ^t -> unit, source: 'I)
            : unit =
            ((^I or Iterate): (static member Iterate:
                ^I * (^t -> unit) -> unit) (source, action))


    [<AbstractClass; Sealed>]
    type IterateIndexed =

        static member inline Invoke
            ([<InlineIfLambda>] action: int -> 't -> unit, source: ^I)
            : unit =


            let mutable index = 0

            ((^I or Iterate): (static member Iterate: _ * _ -> unit) (source,
                                                                      (fun
                                                                          v ->
                                                                          action
                                                                              index
                                                                              v

                                                                          index <-
                                                                              index
                                                                              + 1)))


    [<AbstractClass; Sealed>]
    type Exists =

        static member inline Invoke
            ([<InlineIfLambda>] pred: 't -> bool, source: ^I)
            : bool =

            let mutable notfound = true

            ((^I or IterateWhile): (static member IterateWhile:
                'I * byref<bool> * _ -> unit) (source,
                                               &notfound,
                                               (fun v ->
                                                   notfound <-
                                                       not (pred v))))

            not notfound

    [<AbstractClass; Sealed>]
    type Forall =

        static member inline Invoke
            ([<InlineIfLambda>] pred: 't -> bool, source: ^I)
            : bool =

            let mutable notfound = true

            ((^I or IterateWhile): (static member IterateWhile:
                'I * byref<bool> * _ -> unit) (source,
                                               &notfound,
                                               (fun v ->
                                                   notfound <- pred v)))

            notfound


    [<AbstractClass; Sealed>]
    type Map =
        static member inline Map
            (x: option<_>, [<InlineIfLambda>] f: 't -> 'u)
            : option<'u> =
            Option.map f x

        static member inline Map
            (x: voption<_>, [<InlineIfLambda>] f: 't -> 'u)
            : voption<'u> =
            ValueOption.map f x


        static member inline Map
            (x: Result<'t, _>, [<InlineIfLambda>] f: 't -> 'u)
            : Result<'u, _> =
            match x with
            | Ok v -> Ok(f v)
            | Error v -> Error v

        static member inline Map
            (x: 't[], [<InlineIfLambda>] f: 't -> 'u)
            : 'u[] =
            let dest = Array.zeroCreate<'u> x.Length
            let mutable i = 0

            while i < x.Length do
                dest[i] <- f x[i]
                i <- i + 1

            dest

        static member inline Map
            (x: list<'t>, [<InlineIfLambda>] f: 't -> 'u)
            : list<'u> =
            List.map f x

        static member inline Invoke
            ([<InlineIfLambda>] mapping: 't -> 'u, source: ^I)
            : ^Result =

            let inline call (source: ^I) =
                ((^I or Map): (static member Map:
                    ^I * (^t -> ^u) -> ^Result) (source, mapping))

            call source

    [<AbstractClass; Sealed>]
    type MapIndexed =
        static member inline Invoke
            (mapping: int -> 't -> 'u, source: ^I)
            =

            let mutable index = -1

            ((^I or Map): (static member Map: _ * _ -> _) (source,
                                                           (fun v ->
                                                               index <-
                                                                   index
                                                                   + 1

                                                               mapping
                                                                   index
                                                                   v)))

    [<AbstractClass; Sealed>]
    type Fold =
        static member inline Invoke
            (
                [<InlineIfLambdaAttribute>] fn: ^acc -> ^t -> ^acc,
                (s0: ^acc),
                (source: ^I)
            ) : ^acc =
            let mutable state = s0
            Iterate.Invoke((fun v -> state <- fn state v), source)
            state

    [<AbstractClass; Sealed>]
    type FoldIndexed =
        static member inline Invoke
            ([<InlineIfLambdaAttribute>] fn: int -> ^acc -> ^t -> ^acc)
            (s0: ^acc)
            (source: ^I)
            : ^acc =
            let mutable state = s0

            IterateIndexed.Invoke(
                (fun i v -> state <- fn i state v),
                source
            )

            state



[<AbstractClass; Sealed; AutoOpen>]
module Abstract =

    let inline zero<'a when 'a: (static member Zero: 'a)> : ^a =
        'a.Zero

    let inline one<'a when 'a: (static member One: 'a)> : ^a = 'a.One

    let inline none<'a when 'a: (static member None: 'a)> : ^a =
        'a.None

    let inline some<'a, 'b when 'a: (static member Some: 'b -> 'a)>
        : 'b -> ^a =
        'a.Some

    let inline is_some<'a, ^b when 'a: (member IsSome: bool)>
        (arg: ^a)
        : bool =
        arg.IsSome

    let inline is_none<'a
        when 'a: (static member None: 'a) and ^a: equality>
        (arg: ^a)
        : bool =
        arg = 'a.None

    let inline value<'a, ^b when 'a: (member Value: ^b)>
        (arg: ^a)
        : ^b =
        arg.Value

    [<CompiledName("ToEnum")>]
    let inline enum (value: ^e) : ^t when ^t: enum<^e> =
        LanguagePrimitives.EnumOfValue value

    [<CompiledName("EnumValue")>]
    let inline enumv (enum: ^t when ^t: enum<^e>) : ^e =
        LanguagePrimitives.EnumToValue enum

    let inline default_with<'a, ^b
        when 'a: (member Value: ^b) and 'a: (member IsSome: bool)>
        ([<InlineIfLambda>] or_else: unit -> ^b)
        (arg: ^a)
        : ^b =
        if is_some arg then value arg else or_else ()

    let inline forall
        ([<InlineIfLambdaAttribute>] f: 't -> bool)
        (x: ^I)
        : bool =
        Internal.Forall.Invoke(f, x)

    let inline exists
        ([<InlineIfLambdaAttribute>] f: 't -> bool)
        (x: ^I)
        : bool =
        Internal.Exists.Invoke(f, x)

    let inline iter
        ([<InlineIfLambdaAttribute>] f: 't -> unit)
        (x: ^I)
        : unit =
        Internal.Iterate.Invoke(f, x)

    let inline iteri
        ([<InlineIfLambdaAttribute>] f: int -> ^t -> unit)
        (x: ^I)
        : unit =
        Internal.IterateIndexed.Invoke(f, x)

    // this is intentionally defined initial value first for type inference
    let inline fold
        (initial: ^acc)
        ([<InlineIfLambdaAttribute>] f: ^acc -> ^t -> ^acc)
        (x: ^I)
        =
        Internal.Fold.Invoke(f, initial, x)

    let inline foldi
        (initial: ^acc)
        ([<InlineIfLambdaAttribute>] f: int -> ^acc -> ^t -> ^acc)
        (x: ^I)
        =
        Internal.FoldIndexed.Invoke f initial x

    let inline map ([<InlineIfLambdaAttribute>] f: 't -> 'u) (x: ^I) =
        Internal.Map.Invoke(f, x)

    let inline mapi
        ([<InlineIfLambdaAttribute>] f: int -> 't -> 'u)
        (x: ^I)
        =
        Internal.MapIndexed.Invoke(f, x)

    let inline len (source: 't) : int =
        Internal.Length.Invoke (fun _ -> ()) source

    let inline is_empty (source: 't) : bool =
        Internal.IsEmpty.Invoke (fun _ -> ()) source

    /// default instance, calls let Default
    let inline _default< ^t
        when (^t or Internal.Default): (static member Default:
            (^t -> unit) -> ^t)>
        ()
        : ^t =
        Internal.Default.Invoke< ^t>()

#if FABLE_COMPILER
    // stdout.WriteLine is generally better but fable does not support it
    // this compiles down to console.log
    let inline print (x: obj) = System.Console.WriteLine(x)
#else
    let inline print (x: obj) = stdout.WriteLine(x)
#endif

// overloads for byref/struct/other types
// not as general but at least allowed in CIL
[<AbstractClass; Sealed; AutoOpen>]
type Abstract =
#if !FABLE_COMPILER
    static member inline span(x: ResizeArray<'t>) : System.Span<'t> =
        System.Runtime.InteropServices.CollectionsMarshal.AsSpan(x)

    static member inline span(x: array<'t>) : System.Span<'t> =
        System.MemoryExtensions.AsSpan(x)

    static member inline span(x: string) : System.ReadOnlySpan<'t> =
        System.MemoryExtensions.AsSpan(x)

    static member inline siter
        (x: System.Span<'t>, [<InlineIfLambdaAttribute>] f: 't -> unit) : unit =
        Internal.Iterate.Iterate(x, f)

    static member inline siteri
        (
            x: System.Span<'t>,
            [<InlineIfLambdaAttribute>] f: int -> 't -> unit
        ) : unit =
        let mutable index = 0

        Internal.Iterate.Iterate(
            x,
            (fun v ->
                f index v
                index <- index + 1)
        )

    // todo: some kind of simd lookups as well

    static member inline sforall
        (
            x: System.Span<'t>,
            [<InlineIfLambdaAttribute>] pred: 't -> bool
        ) : bool =
        let mutable notfound = true

        Internal.IterateWhile.IterateWhile(
            x,
            &notfound,
            (fun v -> notfound <- pred v)
        )

        notfound

    static member inline sexists
        (
            x: System.Span<'t>,
            [<InlineIfLambdaAttribute>] pred: 't -> bool
        ) : bool =
        let mutable notfound = true

        Internal.IterateWhile.IterateWhile(
            x,
            &notfound,
            (fun v -> notfound <- not (pred v))
        )

        not notfound
#endif

    static member inline siteri
        (
            x: ResizeArray<'t>,
            [<InlineIfLambdaAttribute>] f: int -> 't -> unit
        ) : unit =
        let mutable i = 0

        while i < x.Count do
            f i x[i]
            i <- i + 1

    static member inline siter
        (x: ResizeArray<'t>, [<InlineIfLambdaAttribute>] f: 't -> unit) : unit =
        let mutable i = 0

        while i < x.Count do
            f x[i]
            i <- i + 1
