module fsil_rust

open System.Runtime.InteropServices
open System.Runtime.CompilerServices
// NOTE:
// this is experimental;
// the generated code is breaks down sometimes and
// Fable.Rust output has to be changed directly for it

#if FABLE_COMPILER_RUST

open Fable.Core
open Fable.Core.RustInterop

[<Erase; Emit("$0")>]
let inline cast(arg: 't) : 'u = nativeOnly


[<Erase; Emit("Rc<$0>")>]
type Rc<'t> =
    class
    end

    [<Emit("$0")>]
    member self.value: 't = nativeOnly

    [<Emit("Rc::new($0)")>]
    static member from(arg: 't) : Rc<'t> = nativeOnly

    [<Emit("Rc::get_mut($0)")>]
    static member get_mut(arg: Rc<'t>) : Option<'t> = nativeOnly

    [<Emit("$0[&$1]")>]
    static member idx(arg: Rc<'t>, key: 'k) : 'v = nativeOnly

[<Erase; Emit("&$0")>]
type Ref<'t> =
    class
    end

    [<Emit("$0")>]
    member self.value: 't = nativeOnly

    [<Emit("&$0")>]
    static member from(arg: 't) : Ref<'t> = nativeOnly

    [<Emit("*$0")>]
    member self.deref: 't = nativeOnly

[<Erase; Emit("&mut $0")>]
type MutRef<'t> =
    class
    end

    [<Emit("$0")>]
    member self.value: 't = nativeOnly

    [<Emit("&mut $0")>]
    static member from(arg: 't) : MutRef<'t> = nativeOnly


[<Erase; Struct; Emit("*mut $0")>]
type MutPtr<'t> =
    struct
    end

    [<Emit("Box::into_raw(Box::new($0))")>]
    static member from(arg: 't) : MutPtr<'t> = nativeOnly

    [<Emit("Box::into_raw(Box::new($0))")>]
    static member from_boxed(arg: 't) : MutPtr<'t> = nativeOnly

    [<Emit("($0 as u32)")>]
    static member as_u32(arg: MutPtr<'t>) : uint32 = nativeOnly

    [<Emit("unsafe {drop(Box::from_raw($0))}")>]
    static member drop(arg: MutPtr<'t>) : unit = nativeOnly

    [<Emit("$0.as_ref()")>]
    member self.as_ref() : Option<Ref<'t>> = nativeOnly

    [<Emit("$0.as_mut()")>]
    member self.as_mut() : Option<MutRef<'t>> = nativeOnly

    [<Emit("($0 as u32)")>]
    member this.as_u32(_: MutPtr<'t>) : uint32 = nativeOnly

    [<Emit("$0.as_mut()")>]
    member self.as_mut_cast<'u>() : Option<'u> = nativeOnly

    [<Emit("unsafe{$0.as_mut().unwrap()}")>]
    member self.deref_unsafe_cast<'u>() : 'u = nativeOnly

    [<Emit("unsafe{$0.as_mut().unwrap()}")>]
    member self.deref_unsafe() : MutRef<'t> = nativeOnly


type pmut<'t> = MutPtr<'t>
type rmut<'t> = MutRef<'t>
type r<'t> = Ref<'t>

let inline (~&) x = Ref.from x
let inline (!&) x = MutRef.from x
let inline (!*) x = MutPtr.from x

[<Erase; Emit("&str")>]
type str =
    class
    end

    [<Emit("$0.to_string()")>]
    member _.to_string() : string = nativeOnly

    static member inline from(s: string) : str = str.as_str s

    [<Emit("$0.as_str()")>]
    static member as_str(s: string) : str = nativeOnly


[<Erase; Struct; Emit("Vec<$0>")>]
type Vec<'t> =
    struct
    end

    [<Emit("Vec::new()")>]
    static member new_() : Vec<'t> = nativeOnly

    [<Emit("$0.push($1)")>]
    member _.push() : unit = nativeOnly

    [<Emit("$0.len()")>]
    member inline _.len() : unativeint = nativeOnly

    [<Emit("$0[$1 as usize].clone()")>]
    member inline _.Item(_: unativeint) : 't = nativeOnly

    [<Emit("$0.contains(&$1)")>]
    member inline _.contains(_: 't) : bool = nativeOnly

    static member inline Iterate(arg: Vec<'t>, [<InlineIfLambda>] fn: 't -> unit) : unit =
        let mutable i: unativeint = 0un
        let endlen = arg.len ()

        while i < endlen do
            fn (arg[i])
            i <- i + 1un

[<AbstractClass; Sealed; AutoOpen; Erase>]
type Abstract =
    [<Emit("$0.unwrap()")>]
    static member inline unwrap(source: Result<'t, _>) : 't = nativeOnly

    [<Emit("$0.unwrap()")>]
    static member inline unwrap(source: Option<'t>) : 't = nativeOnly

    /// requires const string
    static member inline emit(s: string) = RustInterop.emitRustExpr "" s


[<Struct>]
[<Erase; Emit("std::fs::File")>]
type File =
    [<Emit("std::fs::File::open($0.as_str()).map_err(|e| e.to_string())?")>]
    static member open_(path: string) = nativeOnly


[<Erase; Emit("std::io::BufReader<_>")>]
type BufReader<'T> =
    [<Emit("std::io::BufReader::new($0)")>]
    static member new_(inner: 'T) : BufReader<'T> = nativeOnly

    [<Emit("$0.lines().map(|res| res.map_err(|e| e.to_string()))")>]
    member _.lines() = nativeOnly


[<AutoOpen>]
module serde_json =
    [<Emit("serde_json::from_slice(&$0)")>]
    let inline from_vec<'t>(v: Vec<byte>) : Result<'t, obj> =
        RustInterop.emitRustExpr v $"serde_json::from_slice(&$0)"

    // [<Emit("serde_json::from_slice(&$0)")>]
    let inline from_vec_t<'t> (v: Vec<byte>) (t: string) : Result<'t, obj> =
        RustInterop.emitRustExpr (v, t) ($"serde_json::from_slice::<$2>(&$1)")

module io =
    [<Erase; Emit("std::io::Error")>]
    type Error =
        [<Emit("std::io::Error::last_os_error()")>]
        static member last_os_error() : Error = nativeOnly

        [<Emit("$0.raw_os_error()")>]
        member _.raw_os_error() : int option = nativeOnly

    [<Erase; Emit("std::io::BufReader<_>")>]
    type BufReader<'T> =
        [<Emit("std::io::BufReader::new($0)")>]
        static member new_(inner: 'T) : BufReader<'T> = nativeOnly

        [<Emit("$0.lines().map(|res| res.map_err(|e| e.to_string()))")>]
        member _.lines() = nativeOnly


[<Erase>]
type fs =

    [<Emit("std::fs::read($0.as_str()).map_err(|e| e.to_string())")>]
    static member read(path: string) : Result<Vec<byte>, string> = nativeOnly

    // [<Emit("std::fs::read($0)")>]
    // static member read(path: _str) : Result<Vec<byte>, String> = nativeOnly

    [<Emit("std::fs::read($0.as_str()).unwrap()")>]
    static member read_unsafe(path: string) : Vec<byte> = nativeOnly

    // [<Emit("std::fs::read($0).unwrap()")>]
    // static member read_unsafe(path: _str) : Vec<byte> = nativeOnly

    [<Emit("std::fs::read_to_string($0.as_str()).map_err(|e| e.to_string())")>]
    static member read_to_string(path: string) : Result<string, string> = nativeOnly


#endif //FABLE_COMPILER_RUST
