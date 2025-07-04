﻿[<AutoOpen>]
module Fsil

open System.Collections.Generic
open System.Runtime.CompilerServices
open System

[<AbstractClass>]
module Internal =

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Value =
        static member inline Value(x: Result<'t, _>) : 't =
            match x with
            | Ok(v) -> v
            | _ -> failwith "no value"

        static member inline Value(x: ^t) : ^v = (^t: (member Value: ^v) x)

        static member inline Invoke< ^I, ^v
            when (^I or Value): (static member Value: ^I -> ^v)>
            (source: _)
            =
            ((^I or Value): (static member Value: ^I -> ^v) (source))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Item1 =

        static member inline Item1(struct (x, _)) : 't = x
        static member inline Item1(struct (x, _, _)) : 't = x
        static member inline Item1(x: ^t) : ^r = (^t: (member Item1: ^r) x)

        static member inline Invoke< ^I, ^r
            when (^I or Item1): (static member Item1: ^I -> ^r)>
            (source: _)
            : ^r =
            ((^I or Item1): (static member Item1: _ -> _) (source))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Item2 =

        static member inline Item2(struct (_, x)) : 't = x
        static member inline Item2(struct (_, x, _)) : 't = x
        static member inline Item2(x: ^t) : ^r = (^t: (member Item2: ^r) x)

        static member inline Invoke< ^I, ^r
            when (^I or Item2): (static member Item2: ^I -> ^r)>
            (source: _)
            : ^r =
            ((^I or Item2): (static member Item2: _ -> _) (source))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Item3 =

        static member inline Item3(struct (_, _, x)) : 't = x
        static member inline Item3(x: ^t) : ^r = (^t: (member Item3: ^r) x)

        static member inline Invoke< ^I, ^r
            when (^I or Item3): (static member Item3: ^I -> ^r)>
            (source: _)
            : ^r =
            ((^I or Item3): (static member Item3: _ -> _) (source))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Default =

        static member inline Default(_f: option<'t> -> unit) : option<'t> = None

        static member inline Default(_f: voption<'t> -> unit) : voption<'t> = ValueNone

        static member inline Default(_f: Result<'t, ^U> -> unit) : Result<'t, ^U> =
            let inline call source =
                ((^U or Default): (static member Default: _ -> ^U) ((source, _f)))

            Error(call (fun _ -> ()))

        static member inline Default(_f: ResizeArray<'t> -> unit) : ResizeArray<'t> =
            ResizeArray<'t>()

        static member inline Default(_f: list<'t> -> unit) : list<'t> = []

        static member inline Default(_f: array<'t> -> unit) : array<'t> = [||]

        static member inline Default(_f: byte -> unit) : byte = 0uy

        static member inline Default(_f: char -> unit) : char = char 0

        static member inline Default(_f: uint -> unit) : uint = 0u

        static member inline Default(_f: int -> unit) : int = 0

        static member inline Default(_f: bool -> unit) : bool = false

        static member inline Default(_f: string -> unit) : string = ""

        static member inline Default(_f: System.Guid -> unit) : System.Guid =
            System.Guid()

        static member inline Invoke< ^I
            when (^I or Default): (static member Default: (^I -> unit) -> ^I)>
            ()
            : _ =
            ((^I or Default): (static member Default: (^I -> unit) -> _) (fun _ -> ()))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type DefaultWith =

        static member inline DefaultWith
            (x: option<'t>, [<InlineIfLambda>] f: unit -> 't)
            : 't =
            Option.defaultWith f x

        static member inline DefaultWith
            (x: voption<'t>, [<InlineIfLambda>] f: unit -> 't)
            : 't =
            ValueOption.defaultWith f x

        static member inline DefaultWith
            (x: Result<'t, 'e>, [<InlineIfLambda>] f: unit -> 't)
            : 't =
            match x with
            | Ok(v) -> v
            | _ -> f ()

        static member inline Invoke
            ([<InlineIfLambda>] action: unit -> ^t, source: 'I)
            : 't =
            ((^I or DefaultWith): (static member DefaultWith: ^I * (unit -> ^t) -> ^t) (source,
                                                                                        action))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IsEmpty =
        // if it has length defined then use length
        static member inline IsEmpty(x: ^t) : bool = (^t: (member Length: int) x) = 0
        static member inline IsEmpty(x: option<'t>) : bool = x.IsNone
        static member inline IsEmpty(x: voption<'t>) : bool = x.IsNone

        static member inline IsEmpty(x: Result<'t, _>) : bool =
            match x with
            | Ok(_) -> false
            | _ -> true

        static member inline IsEmpty(x: ResizeArray<'t>) : bool = x.Count = 0
        static member inline IsEmpty(x: Dictionary<'k, 'v>) : bool = x.Count = 0


        static member inline Invoke< ^I
            when (^I or IsEmpty): (static member IsEmpty: ^I -> bool)>
            (source: _)
            : bool =
            ((^I or IsEmpty): (static member IsEmpty: ^I -> bool) (source))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Length =

        static member inline Length(x: option<'t>) : int =
            match x with
            | Some _ -> 1
            | _ -> 0

        static member inline Length(x: voption<'t>) : int =
            match x with
            | ValueSome _ -> 1
            | _ -> 0

        static member inline Length(x: Result<'t, _>) : int =
            match x with
            | Ok(_) -> 1
            | _ -> 0

        static member inline Length(x: ResizeArray<'t>) : int = x.Count


        static member inline Length(x: ^t) : int = (^t: (member Length: int) x)
        static member inline Length(x: Dictionary<'k, 'v>) : int = x.Count


        static member inline Invoke< ^I
            when (^I or Length): (static member Length: ^I -> int)>
            (source: _)
            : int =
            ((^I or Length): (static member Length: _ -> _) (source))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Item =
        static member inline Item(x: Dictionary< ^k, ^v >, key: ^k) : ^v = x[key]
        static member inline Item(x: array< ^t >, key: int) : ^t = x[key]
        static member inline Item(x: list< ^t >, key: int) : ^t = x.Item key

        static member inline Item< ^I, ^k, ^v when ^I: (member Item: ^k -> ^v)>
            (x: ^I, key: ^k)
            : ^v =
            x.Item key

        static member inline Invoke< ^I, ^k, ^v
            when (^I or Item): (static member Item: ^I * ^k -> ^v)>
            (source: _, key: ^k)
            : _ =
            ((^I or Item): (static member Item: ^I * ^k -> ^v) (source, key))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IterateWhile =

        static member inline IterateWhile
            (x: 't[], cond: byref<bool>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0
            let length = x.Length

            while cond && i < length do
                f x[i]
                i <- i + 1

        static member inline IterateWhile
            (x: Span<'t>, cond: byref<bool>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0
            let length = x.Length

            while cond && i < length do
                f x[i]
                i <- i + 1

        static member inline IterateWhile
            (x: list<'t>, cond: byref<bool>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable curr = x

            while cond && not curr.IsEmpty do
                f curr.Head
                curr <- curr.Tail

        static member inline IterateWhile
            (
                x: Dictionary<'k, 'v>,
                cond: byref<bool>,
                [<InlineIfLambda>] f: KeyValuePair<'k, 'v> -> unit
            ) : unit =
            use mutable e = x.GetEnumerator()

            while cond && e.MoveNext() do
                f e.Current

        static member inline IterateWhile
            (x: option<'t>, cond: byref<bool>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            if cond && x.IsSome then
                f x.Value

        static member inline IterateWhile
            (x: voption<'t>, cond: byref<bool>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            if cond && x.IsSome then
                f x.Value

        static member inline Invoke
            (source: _, cond: byref<bool>, [<InlineIfLambda>] action: 't -> unit)
            : unit =
            ((^I or IterateWhile): (static member IterateWhile:
                ^I * byref<bool> * (^t -> unit) -> unit) (source, &cond, action))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type TryItem =
        static member inline TryItem(x: array<'t>, key: int) : voption<'t> =
            if key < x.Length then ValueSome(x[key]) else ValueNone

        static member inline TryItem(x: Dictionary<'k, 'v>, key: 'k) : voption<'v> =
            match x.TryGetValue(key) with
            | true, v -> ValueSome(v)
            | _ -> ValueNone

        static member inline Invoke< ^I, ^k, ^v
            when (^I or TryItem): (static member TryItem: ^I * ^k -> voption< ^v >)>
            (source: _, key: ^k)
            : _ =
            ((^I or TryItem): (static member TryItem: ^I * ^k -> voption< ^v >) (source,
                                                                                 key))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
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

        static member inline Iterate(x: 't[], [<InlineIfLambda>] f: 't -> unit) : unit =
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
            (x: System.ReadOnlySpan<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (x: ResizeArray<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0

            while i < x.Count do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (x: Dictionary<'k, 'v>, [<InlineIfLambda>] f: KeyValuePair<'k, 'v> -> unit)
            : unit =
            use mutable e = x.GetEnumerator()

            while e.MoveNext() do
                f e.Current

        static member inline Invoke< ^I, ^t
            when (^I or Iterate): (static member Iterate: ^I * (^t -> unit) -> unit)>
            (source: _, [<InlineIfLambda>] action: ^t -> unit)
            : unit =
            ((^I or Iterate): (static member Iterate: ^I * (^t -> unit) -> unit) (source,
                                                                                  action))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IterateIndexed =

        static member inline Invoke
            (source: _, [<InlineIfLambda>] action: int -> 't -> unit)
            : unit =
            let mutable index = 0

            Iterate.Invoke<_, ^t>(
                source,
                (fun v ->
                    action index v
                    index <- index + 1)
            )

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Exists =

        static member inline Invoke
            (source: ^I, [<InlineIfLambda>] pred: 't -> bool)
            : bool =

            let mutable notfound = true

            ((^I or IterateWhile): (static member IterateWhile:
                'I * byref<bool> * _ -> unit) (source,
                                               &notfound,
                                               (fun v -> notfound <- not (pred v))))

            not notfound

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Find =

        static member inline Invoke
            (source: ^I, [<InlineIfLambda>] pred: 't -> bool)
            : voption<'t> =

            let mutable looking = true
            let mutable result = ValueNone

            IterateWhile.Invoke(
                source,
                &looking,
                (fun v ->
                    if pred v then
                        looking <- false
                        result <- ValueSome v)
            )

            result

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Pick =

        static member inline Invoke
            (source: ^I, [<InlineIfLambda>] selector: 't -> voption<'v>)
            : voption<'v> =

            let mutable looking = true
            let mutable result = ValueNone

            IterateWhile.Invoke(
                source,
                &looking,
                (fun v ->
                    result <- selector v

                    if result.IsSome then
                        looking <- false)
            )

            result

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type FindIndex =

        static member inline Invoke
            (source: ^I, [<InlineIfLambda>] pred: 't -> bool)
            : voption<int> =

            let mutable looking = true
            let mutable i = 0

            IterateWhile.Invoke(
                source,
                &looking,
                (fun v -> if pred v then looking <- false else i <- i + 1)
            )

            if looking then ValueNone else ValueSome i

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Forall =

        static member inline Invoke
            (source: ^I, [<InlineIfLambda>] pred: 't -> bool)
            : bool =

            let mutable notfound = true

            ((^I or IterateWhile): (static member IterateWhile:
                'I * byref<bool> * _ -> unit) (source,
                                               &notfound,
                                               (fun v -> notfound <- pred v)))

            notfound

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
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

        static member inline Map(x: 't[], [<InlineIfLambda>] f: 't -> 'u) : 'u[] =
            let dest = Array.zeroCreate<'u> x.Length
            let mutable i = 0

            while i < x.Length do
                dest[i] <- f x[i]
                i <- i + 1

            dest

        static member inline Map(x: list<'t>, [<InlineIfLambda>] f: 't -> 'u) : list<'u> =
            List.map f x


        static member inline Invoke< ^I, ^t, ^u, ^r
            when (^I or Map): (static member Map: ^I * (^t -> ^u) -> ^r)>
            (source: _, [<InlineIfLambda>] action: ^t -> ^u)
            : _ =
            ((^I or Map): (static member Map: ^I * (^t -> ^u) -> _) (source, action))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type MapIndexed =
        static member inline Invoke(source: _, mapping: int -> 't -> 'u) =

            let mutable index = 0

            Map.Invoke(
                source,
                (fun v ->
                    let v = mapping index v
                    index <- index + 1
                    v)
            )


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Bind =
        static member inline Bind
            (x: option<'t>, [<InlineIfLambda>] f: 't -> option<'u>)
            : option<'u> =
            Option.bind f x

        static member inline Bind
            (x: voption<'t>, [<InlineIfLambda>] f: 't -> voption<'u>)
            : voption<'u> =
            ValueOption.bind f x

        static member inline Bind
            (x: array<'t>, [<InlineIfLambda>] f: 't -> array<'u>)
            : array<'u> =
            let result = ResizeArray()
            Iterate.Invoke(x, (fun v -> result.AddRange(f v)))
            result.ToArray()

        static member inline Bind
            (x: ResizeArray<'t>, [<InlineIfLambda>] f: 't -> ResizeArray<'u>)
            : ResizeArray<'u> =
            let result = ResizeArray()
            Iterate.Invoke(x, (fun v -> result.AddRange(f v)))
            result

        static member inline Bind
            (x: list<'t>, [<InlineIfLambda>] f: 't -> list<'u>)
            : list<'u> =
            List.collect f x

        static member inline Invoke< ^I, ^t, ^u, ^r
            when (^I or Bind): (static member Bind: ^I * (^t -> ^u) -> ^r)>
            ([<InlineIfLambda>] fn: ^t -> ^u, source: _)
            : _ =
            ((^I or Bind): (static member Bind: ^I * (^t -> ^u) -> _) (source, fn))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Fold =
        static member inline Invoke
            ((source: _), (s0: _), [<InlineIfLambdaAttribute>] fn: ^acc -> ^t -> ^acc)
            : ^acc =
            let mutable state = s0
            Iterate.Invoke(source, (fun v -> state <- fn state v))
            state

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type FoldIndexed =
        static member inline Invoke
            (s0: ^acc)
            (source: _)
            ([<InlineIfLambdaAttribute>] fn: int -> ^acc -> ^t -> ^acc)
            : ^acc =
            let mutable state = s0

            IterateIndexed.Invoke(source, (fun i v -> state <- fn i state v))

            state


#if FABLE_COMPILER
[<Fable.Core.Erase>]
#endif
[<AbstractClass; Sealed; AutoOpen>]
module Abstract =

    let inline is_empty(source: _) : bool = Internal.IsEmpty.Invoke source

    let inline is_null_or_empty(source: _) : bool =
        source = null || Internal.IsEmpty.Invoke source

    let inline value(source: _) : _ = Internal.Value.Invoke(source)

    let inline default_with ([<InlineIfLambdaAttribute>] or_else: unit -> _) (x: _) : _ =
        Internal.DefaultWith.Invoke(or_else, x)

    let inline zero<'a when 'a: (static member Zero: 'a)> : ^a = 'a.Zero

    let inline one<'a when 'a: (static member One: 'a)> : ^a = 'a.One

    let inline none<'a when 'a: (static member None: 'a)> : ^a = 'a.None

    // tbd: perhaps this should just be an alias for ValueSome
    let inline some<'a, 'b when 'a: (static member Some: 'b -> 'a)> : 'b -> ^a = 'a.Some

    let inline is_some<'a when 'a: (member IsSome: bool)>(arg: ^a) : bool = arg.IsSome

    let inline is_none<'a when 'a: (member IsNone: bool)>(arg: ^a) : bool = arg.IsNone

    let inline is_ok<'a when 'a: (member IsOk: bool)>(arg: ^a) : bool = arg.IsOk

    let inline enum(value: ^e) : ^t when ^t: enum<^e> =
        LanguagePrimitives.EnumOfValue value

    let inline enumv(enum: ^t when ^t: enum<^e>) : ^e =
        LanguagePrimitives.EnumToValue enum

    let inline forall ([<InlineIfLambdaAttribute>] f: 't -> bool) (x: _) : bool =
        Internal.Forall.Invoke(x, f)

    let inline exists ([<InlineIfLambdaAttribute>] f: 't -> bool) (x: _) : bool =
        Internal.Exists.Invoke(x, f)

    let inline iter_range
        ([<InlineIfLambdaAttribute>] f: int -> unit)
        (limit_exclusive: int)
        : unit =
        let mutable i = 0

        while i < limit_exclusive do
            f i
            i <- i + 1

    let inline iter ([<InlineIfLambdaAttribute>] f) (x: _) : unit =
        Internal.Iterate.Invoke(x, f)

    let inline iteri ([<InlineIfLambdaAttribute>] f) (x: _) : unit =
        Internal.IterateIndexed.Invoke(x, f)

    // this is intentionally defined initial value first for type inference
    let inline fold (initial) ([<InlineIfLambdaAttribute>] f) (x: _) =
        Internal.Fold.Invoke(x, initial, f)

    let inline foldi (initial) ([<InlineIfLambdaAttribute>] f) (x: _) =
        Internal.FoldIndexed.Invoke initial x f

    let inline map ([<InlineIfLambdaAttribute>] f) (x: _) = Internal.Map.Invoke(x, f)

    let inline mapi ([<InlineIfLambdaAttribute>] f) (x: _) =
        Internal.MapIndexed.Invoke(x, f)

    let inline bind ([<InlineIfLambdaAttribute>] f) (x: _) = Internal.Bind.Invoke(f, x)

    let inline len(source: _) : int = Internal.Length.Invoke source
    /// checked indexer
    let inline try_item k (source: _) = Internal.TryItem.Invoke(source, k)
    /// alias for try_item, use `item` for unchecked
    let inline get k (source: _) = Internal.TryItem.Invoke(source, k)

    /// tuple indexer (x,_)
    let inline _1(source: _) = Internal.Item1.Invoke(source)
    /// tuple indexer (_,x)
    let inline _2(source: _) = Internal.Item2.Invoke(source)
    /// tuple indexer (_,_,x)
    let inline _3(source: _) = Internal.Item3.Invoke(source)


    /// wrap potentially exception
    let inline catch fn =
        try
            Ok(fn ())
        with e ->
            Error(e)

    /// unchecked indexer/key like `.Item`
    let inline item k (source: _) = Internal.Item.Invoke(source, k)
    let inline find k (source: _) = Internal.Find.Invoke(source, k)
    let inline pick k (source: _) = Internal.Pick.Invoke(source, k)

    let inline find_index k (source: _) = Internal.FindIndex.Invoke(source, k)

    // default from type parameter
    let inline default_< ^t
        when (^t or Internal.Default): (static member Default: (^t -> unit) -> ^t)>
        ()
        : ^t =
        Internal.Default.Invoke< ^t>()

#if FABLE_COMPILER
    // stdout.WriteLine is generally better but fable does not support it
#if FABLE_COMPILER_RUST
    let inline print(x: 't) = printfn $"%A{x}"
#else
    let inline print(x: 't) = System.Console.WriteLine(x)
#endif
#else
    let inline print(x: 't) = stdout.WriteLine(x)
#endif

#if !FABLE_COMPILER
// overloads for byref/struct/other types
// not as general but at least allowed in CIL
[<AbstractClass; Sealed; AutoOpen>]
type Abstract =

    static member inline span(x: ResizeArray<'t>) : System.Span<'t> =
        System.Runtime.InteropServices.CollectionsMarshal.AsSpan(x)

    static member inline span(x: array<'t>) : System.Span<'t> =
        System.MemoryExtensions.AsSpan(x)

    static member inline span(x: string) : System.ReadOnlySpan<char> =
        System.MemoryExtensions.AsSpan(x)

    static member inline span_iter
        (x: System.Span<'t>, [<InlineIfLambdaAttribute>] f: 't -> unit)
        : unit =
        Internal.Iterate.Iterate(x, f)

    static member inline span_iter_while
        (x: System.Span<'t>, cond: byref<bool>, [<InlineIfLambdaAttribute>] f: 't -> unit)
        : unit =
        Internal.IterateWhile.IterateWhile(x, &cond, f)

    static member inline span_iteri
        (x: System.Span<'t>, [<InlineIfLambdaAttribute>] f: int -> 't -> unit)
        : unit =
        let mutable index = 0

        Internal.Iterate.Iterate(
            x,
            (fun v ->
                f index v
                index <- index + 1)
        )

    // todo: some kind of simd lookups as well

    static member inline span_forall
        (x: System.Span<'t>, [<InlineIfLambdaAttribute>] pred: 't -> bool)
        : bool =
        let mutable notfound = true

        Internal.IterateWhile.IterateWhile(x, &notfound, (fun v -> notfound <- pred v))

        notfound

    static member inline span_exists
        (x: System.Span<'t>, [<InlineIfLambdaAttribute>] pred: 't -> bool)
        : bool =
        let mutable notfound = true

        Internal.IterateWhile.IterateWhile(
            x,
            &notfound,
            (fun v -> notfound <- not (pred v))
        )

        not notfound
#endif
