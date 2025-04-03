[<AutoOpen>]
module Fsil

// constrained type variables
#nowarn "64"

[<AbstractClass; Sealed; AutoOpen>]
module Abstract =
    let inline zero<'a when 'a: (static member Zero: 'a)> : ^a = 'a.Zero
    let inline one<'a when 'a: (static member One: 'a)> : ^a = 'a.One
    let inline none<'a when 'a: (static member None: 'a)> : ^a = 'a.None

    let inline some<'a, 'b when 'a: (static member Some: 'b -> 'a)> : 'b -> ^a =
        'a.Some

    let inline is_some<'a, ^b when 'a: (member IsSome: bool)> (arg: ^a) : bool =
        arg.IsSome

    let inline is_none<'a when 'a: (static member None: 'a) and ^a: equality>
        (arg: ^a)
        : bool =
        arg = 'a.None

    let inline value<'a, ^b when 'a: (member Value: ^b)> (arg: ^a) : ^b = arg.Value

    [<CompiledName("ToEnum")>]
    let inline enum (value: ^e) : ^t when ^t: enum<^e> =
        LanguagePrimitives.EnumOfValue value

    [<CompiledName("EnumValue")>]
    let inline enumv (enum: ^t when ^t: enum<^e>) : ^e =
        LanguagePrimitives.EnumToValue enum

    let inline default_with<'a, ^b
        when 'a: (member Value: ^b) and 'a: (member IsSome: bool)>
        (or_else: unit -> ^b)
        (arg: ^a)
        : ^b =
        if is_some arg then value arg else or_else ()

#if FABLE_COMPILER
    // stdout.WriteLine is generally better but fable does not support it
    // this compiles down to console.log
    let inline print (x: obj) = System.Console.WriteLine(x)
#else
    let inline print (x: obj) = stdout.WriteLine($"%A{x}")
#endif


[<AbstractClass>]
module Internal =

    [<AbstractClass; Sealed>]
    type Default =

        static member inline Default((_f: option<'t> -> unit)) : option<'t> = None

        static member inline Default((_f: voption<'t> -> unit)) : voption<'t> =
            ValueNone

        static member inline Default((_f: Result<'t, ^U> -> unit)) : Result<'t, 'u> =
            let inline call source =
                ((^U or Default): (static member Default: _ -> ^U) ((source, _f)))

            Error(call (fun (v: ^U) -> ()))

        static member inline Default
            ((_f: ResizeArray<'t> -> unit))
            : ResizeArray<'t> =
            ResizeArray<'t>()

        static member inline Default((_f: list<'t> -> unit)) : list<'t> = []

        static member inline Default((_f: array<'t> -> unit)) : array<'t> = [||]

        static member inline Default((_f: byte -> unit)) : byte = 0uy
        static member inline Default((_f: char -> unit)) : char = char 0
        static member inline Default((_f: uint -> unit)) : uint = 0u
        static member inline Default((_f: int -> unit)) : int = 0
        static member inline Default((_f: bool -> unit)) : bool = false
        static member inline Default((_f: string -> unit)) : string = ""

        static member inline Default((_f: System.Guid -> unit)) : System.Guid =
            System.Guid()

        static member inline Invoke< ^I
            when (^I or Default): (static member Default: (^I -> unit) -> ^I)>
            ()
            : ^I =
            ((^I or Default): (static member Default: (^I -> unit) -> ^I) (fun _ ->
                ()))

    [<AbstractClass; Sealed>]
    type Length =
        static member inline Length((x: option<'t>, _f: unit -> unit)) : int =
            if is_some x then 1 else 0

        static member inline Length((x: voption<'t>, _f: unit -> unit)) : int =
            if is_some x then 1 else 0

        static member inline Length((x: Result<'t, _>, _f: unit -> unit)) : int =
            match x with
            | Ok(_) -> 1
            | _ -> 0

        static member inline Length((x: ResizeArray<'t>, _f: unit -> unit)) : int =
            x.Count

        static member inline Length((x: list<'t>, _f: unit -> unit)) : int = x.Length

        static member inline Length((x: array<'t>, _f: unit -> unit)) : int =
            x.Length

        static member inline Invoke (_f: unit -> unit) (source: 'I) : int =

            let inline call source =
                ((^I or Length): (static member Length: (_ * _) -> int) ((source, _f)))

            call source


    [<AbstractClass; Sealed>]
    type IterateIndexed =
        static member inline IterateIndexed
            ((x: option<'t>, f: int -> 't -> unit))
            : unit =
            if is_some x then
                f 0 (value x)

        static member inline IterateIndexed
            ((x: voption<'t>, f: int -> 't -> unit))
            : unit =
            if is_some x then
                f 0 (value x)

        static member inline IterateIndexed((x: 't[], f: int -> 't -> unit)) : unit =
            let mutable i = 0

            while i < x.Length do
                f i x[i]
                i <- i + 1

        static member inline IterateIndexed
            ((x: Result<'t, _>, f: int -> 't -> unit))
            : unit =
            match x with
            | Ok v -> f 0 v
            | _ -> ()

        static member inline IterateIndexed
            ((x: ResizeArray<'t>, f: int -> 't -> unit))
            : unit =
            let mutable i = 0

            while i < x.Count do
                f i x[i]
                i <- i + 1

        static member inline IterateIndexed
            ((x: 't list, f: int -> 't -> unit))
            : unit =
            List.iteri f x

        static member inline Invoke
            (action: int -> 't -> unit)
            (source: 'source)
            : unit =

            let inline call (source: ^I) =
                ((^I or IterateIndexed): (static member IterateIndexed:
                    (_ * _) -> unit) ((source, action)))

            call source

    [<AbstractClass; Sealed>]
    type Iterate =

        static member inline Iterate((x: option<'t>, f: 't -> unit)) : unit =
            Option.iter f x

        static member inline Iterate((x: voption<'t>, f: 't -> unit)) : unit =
            ValueOption.iter f x

        static member inline Iterate((x: 't[], f: 't -> unit)) : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate((x: Result<'t, _>, f: 't -> unit)) : unit =
            match x with
            | Ok v -> f v
            | _ -> ()

        static member inline Iterate((x: ResizeArray<'t>, f: 't -> unit)) : unit =
            let mutable i = 0

            while i < x.Count do
                f x[i]
                i <- i + 1

        static member inline Iterate((x: list<'t>, f: 't -> unit)) : unit =
            List.iter f x


        static member inline Invoke (action: 't -> unit) (source: 'source) : unit =
            let inline call (source: ^I) =
                ((^I or Iterate): (static member Iterate: (_ * _) -> unit) ((source,
                                                                             action)))

            call source

    [<AbstractClass; Sealed>]
    type Map =
        static member inline Map((x: option<_>, f: 't -> 'u)) : option<'u> =
            Option.map f x

        static member inline Map((x: voption<_>, f: 't -> 'u)) : voption<'u> =
            ValueOption.map f x


        static member inline Map((x: Result<'t, _>, f: 't -> 'u)) : Result<'u, _> =
            match x with
            | Ok v -> Ok(f v)
            | Error v -> Error v

        static member inline Map((x: 't[], f: 't -> 'u)) : 'u[] =
            let dest = Array.zeroCreate<'u> x.Length
            let mutable i = 0

            while i < x.Length do
                dest[i] <- f x[i]
                i <- i + 1

            dest


        static member inline Map
            ((x: ResizeArray<'t>, f: 't -> 'u))
            : ResizeArray<'u> =
            let dest = ResizeArray()

            for item in x do
                dest.Add(f item)

            dest

        static member inline Map((x: list<'t>, f: 't -> 'u)) : list<'u> =
            List.map f x

        static member inline Invoke (mapping: ^t -> ^u) (source: ^I) : 'Result =

            let inline call (source: ^I) =
                ((^I or Map): (static member Map: (^I * (^t -> ^u)) -> 'Result) ((source,
                                                                                  mapping)))

            call source

    [<AbstractClass; Sealed>]
    type MapIndexed =
        static member inline MapIndexed
            ((x: option<'t>, f: int -> 't -> ^u))
            : option< ^u > =
            if is_some x then Some(f 0 (value x)) else None

        static member inline MapIndexed
            ((x: voption<'t>, f: int -> 't -> ^u))
            : voption<'u> =
            if is_some x then ValueSome(f 0 (value x)) else ValueNone

        static member inline MapIndexed((x: 't[], f: int -> 't -> ^u)) : 'u[] =
            let dest = Array.zeroCreate<'u> x.Length
            let mutable i = 0

            while i < x.Length do
                dest[i] <- f i x[i]
                i <- i + 1

            dest

        static member inline MapIndexed
            ((x: Result<'t, _>, f: int -> 't -> ^u))
            : Result<'u, _> =
            match x with
            | Ok v -> Ok(f 0 v)
            | Error e -> Error e

        static member inline MapIndexed
            ((x: ResizeArray<'t>, f: int -> 't -> ^u))
            : ResizeArray<'u> =
            let dest = ResizeArray x.Count
            let mutable i = 0

            while i < x.Count do
                dest.Add(f i x[i])
                i <- i + 1

            dest

        static member inline MapIndexed
            ((x: list<'t>, f: int -> 't -> ^u))
            : list<'u> =
            List.mapi f x

        static member inline Invoke (action: int -> ^t -> ^u) (source: 'source) =

            let inline call (source: ^I) =
                ((^I or MapIndexed): (static member MapIndexed: (_ * _) -> _) ((source,
                                                                                action)))

            call source



[<AbstractClass; Sealed; AutoOpen>]
type Abstract =

    static member inline iter (f: 't -> unit) (x: ^I) : unit =
        Internal.Iterate.Invoke f x

    static member inline iteri (f: int -> 't -> unit) (x: ^I) : unit =
        Internal.IterateIndexed.Invoke f x

    static member inline map (f: 't -> 'u) (x: ^I) = Internal.Map.Invoke f x

    static member inline mapi (f: int -> 't -> 'u) (x: ^I) =
        Internal.MapIndexed.Invoke f x

    static member inline len(source: 't) : int =
        Internal.Length.Invoke (fun _ -> ()) source

    /// default instance, calls static member Default
    static member inline _default< ^t
        when (^t or Internal.Default): (static member Default: (^t -> unit) -> ^t)>
        ()
        : ^t =
        Internal.Default.Invoke< ^t>()
