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


    let inline default_value<'a, ^b
        when 'a: (member Value: ^b) and 'a: (member IsSome: bool)>
        (or_else: ^b)
        (arg: ^a)
        : ^b =
        if is_some arg then value arg else or_else


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
    type Collections =
        static member inline Length(x: option<'t>) = if is_some x then 1 else 0
        static member inline Length(x: voption<'t>) = if is_some x then 1 else 0
        static member inline Length(x: array<'t>) = x.Length
        static member inline Length(x: ResizeArray<'t>) = x.Count


    [<AbstractClass; Sealed>]
    type IterateIndexed =
        static member inline IterateIndexed
            ((x: option<'t>, f: int -> 't -> unit), _tmp: IterateIndexed -> unit)
            : unit =
            if is_some x then
                f 0 (value x)

        static member inline IterateIndexed
            ((x: voption<'t>, f: int -> 't -> unit), _tmp: IterateIndexed -> unit)
            : unit =
            if is_some x then
                f 0 (value x)

        static member inline IterateIndexed
            ((x: 't[], f: int -> 't -> unit), _tmp: IterateIndexed -> unit)
            : unit =
            let mutable i = 0

            while i < x.Length do
                f i x[i]
                i <- i + 1

        static member inline IterateIndexed
            ((x: Result<'t, _>, f: int -> 't -> unit), _tmp: IterateIndexed -> unit)
            : unit =
            match x with
            | Ok v -> f 0 v
            | _ -> ()

        static member inline IterateIndexed
            ((x: ResizeArray<'t>, f: int -> 't -> unit), _tmp: IterateIndexed -> unit) : unit =
            let mutable i = 0

            while i < x.Count do
                f i x[i]
                i <- i + 1


        static member inline Invoke
            (action: int -> 't -> unit)
            (source: 'Functor)
            : unit =

            let inline call (tmp: ^M -> unit, source: ^I) =
                ((^M or ^I): (static member IterateIndexed: (_ * _) * _ -> unit) (source,
                                                                                  action),
                                                                                 tmp)

            call ((fun (_: IterateIndexed) -> ()), source)

    [<AbstractClass; Sealed>]
    type Iterate =
        static member inline Iterate
            ((x: option<'t>, f: 't -> unit), _tmp: Iterate -> unit)
            : unit =
            Option.iter f x

        static member inline Iterate
            ((x: voption<'t>, f: 't -> unit), _tmp: Iterate -> unit)
            : unit =
            ValueOption.iter f x

        static member inline Iterate
            ((x: 't[], f: 't -> unit), _tmp: Iterate -> unit)
            : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate
            ((x: Result<'t, _>, f: 't -> unit), _tmp: Iterate -> unit)
            : unit =
            match x with
            | Ok v -> f v
            | _ -> ()

        static member inline Iterate
            ((x: ResizeArray<'t>, f: 't -> unit), _tmp: Iterate -> unit)
            : unit =
            let mutable i = 0

            while i < x.Count do
                f x[i]
                i <- i + 1

        static member inline Invoke (action: 't -> unit) (source: 'Functor) : unit =

            let inline call (tmp: ^M -> unit, source: ^I) =
                ((^M or ^I): (static member Iterate: (_ * _) * _ -> unit) (source,
                                                                           action),
                                                                          tmp)

            call ((fun (_: Iterate) -> ()), source)

    [<AbstractClass; Sealed>]
    type Map =
        static member inline Map
            ((x: option<_>, f: 't -> 'u), _tmp: Map -> unit)
            : option<'u> =
            Option.map f x

        static member inline Map
            ((x: voption<_>, f: 't -> 'u), _tmp: Map -> unit)
            : voption<'u> =
            ValueOption.map f x


        static member inline Map
            ((x: Result<'t, _>, f: 't -> 'u), _tmp: Map -> unit)
            : Result<'u, _> =
            match x with
            | Ok v -> Ok(f v)
            | Error v -> Error v

        static member inline Map((x: 't[], f: 't -> 'u), _tmp: Map -> unit) : 'u[] =
            let dest = Array.zeroCreate<'u> x.Length
            let mutable i = 0

            while i < x.Length do
                dest[i] <- f x[i]
                i <- i + 1

            dest


        static member inline Map
            ((x: ResizeArray<'t>, f: 't -> 'u), _tmp: Map -> unit)
            : ResizeArray<'u> =
            let coll = ResizeArray()

            for item in x do
                coll.Add(f item)

            coll

        static member inline Invoke (mapping: ^t -> ^u) (source: ^I) : 'Result =

            let inline call (tmp: ^M -> unit, source: ^I) =
                ((^M or ^I): (static member Map: (^I * (^t -> ^u)) * _ -> 'Result) (source,
                                                                                    mapping),
                                                                                   tmp)

            call ((fun (_: Map) -> ()), source)








[<AbstractClass; Sealed; AutoOpen>]
type Abstract =

    static member inline iter (f: 't -> unit) (x: 'input) : unit =
        Internal.Iterate.Invoke f x

    static member inline iteri (f: int -> 't -> unit) (x: 'input) : unit =
        Internal.IterateIndexed.Invoke f x

    static member inline map (f: 't -> 'u) (x: 'input) : 'result =
        Internal.Map.Invoke f x

    static member inline len(x: 'input) : 'result = Internal.Collections.Length x
