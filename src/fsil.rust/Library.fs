[<AutoOpenAttribute>]
module fsil_rust

// NOTE:
// this is experimental;
// the generated code is breaks down sometimes and
// Fable.Rust output has to be changed directly for it

#if FABLE_COMPILER_RUST


open Fable.Core
open Fable.Core.RustInterop

/// &str: string slice
[<Erase; Emit("&str")>]
type _str = interface end

[<Erase; Emit("Vec<$0>")>]
type Vec<'u8> =
    struct
    end

    [<Emit("Vec::new()")>]
    static member new_() : Vec<'u8> = nativeOnly

    [<Emit("$0.push($1)")>]
    member _.push() : unit = nativeOnly

    [<Emit("$0.len()")>]
    member inline _.len() : unativeint = nativeOnly

    [<Emit("$0[$1 as usize]")>]
    member inline _.Item(_: unativeint) : 'u8 = nativeOnly

    static member inline Iterate
        (arg: Vec<'u8>, [<Emit("fn($0)")>] fn: 'u8 -> unit)
        : unit =
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

    /// requires const string
    static member inline str(s: string) : _str =
        RustInterop.emitRustExpr "" ("\"" + s + "\"")


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
module string =
    [<Erase; Emit("String")>]
    type String =
        [<Emit("String::from($0.as_str())")>]
        member _.from(s: string) : String = nativeOnly

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
    static member read(path: string) : Result<Vec<byte>, String> = nativeOnly

    [<Emit("std::fs::read($0)")>]
    static member read(path: _str) : Result<Vec<byte>, String> = nativeOnly

    [<Emit("std::fs::read($0.as_str()).unwrap()")>]
    static member read_unsafe(path: string) : Vec<byte> = nativeOnly

    [<Emit("std::fs::read($0).unwrap()")>]
    static member read_unsafe(path: _str) : Vec<byte> = nativeOnly

    [<Emit("std::fs::read_to_string($0.as_str()).map_err(|e| e.to_string())")>]
    static member read_to_string(path: string) : Result<String, String> = nativeOnly


#endif //FABLE_COMPILER_RUST
