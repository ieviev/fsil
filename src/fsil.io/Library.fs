module Fsil.IO 

#if !FABLE_COMPILER

open System.Collections.Generic
open System.Runtime.CompilerServices
open System

[<AbstractClass>]
module Internal =

    [<AbstractClass; Sealed>]
    type Write =

        static member inline Write
            (path:string, data: byte[])
            : Result<unit,exn> =
            catch (fun _ -> System.IO.File.WriteAllBytes(path,data) )

        static member inline Write
            (path:string, data: string seq)
            : Result<unit,exn> =
            catch (fun _ -> System.IO.File.WriteAllLines(path,data) )

        static member inline Write
            (path:string, data: string)
            : Result<unit,exn> =
            try Ok(System.IO.File.WriteAllText(path,data))
            with e -> Error(e)

        static member inline Write
            (path:string, data: ReadOnlySpan<byte>)
            : Result<unit,exn> =
            try Ok(System.IO.File.WriteAllBytes(path,data))
            with e -> Error(e)

        static member inline Write
            (path:string, data: ReadOnlySpan<char>)
            : Result<unit,exn> =
            try Ok(System.IO.File.WriteAllText(path,data))
            with e -> Error(e)
       
        static member inline Invoke< ^I, ^t
            when (^I or Write): (static member Write: string * ^I -> Result<unit,exn>)>
            (path: string, data: ^I)
            : Result<unit,exn> =
            ((^I or Write): (static member Write: string * ^I -> Result<unit,exn>) (path,
                                                                                  data))


open System.IO
[<AbstractClass; Sealed; AutoOpen>]
type IO =
    static member inline read(path: string) = 
        catch (fun _ -> 
            System.IO.File.ReadAllBytes(path)
        )

    static member inline read_json<'t>(path: string) = 
        catch (fun _ -> 
            use stream = System.IO.File.Open(path, FileMode.Open)
            System.Text.Json.JsonSerializer.Deserialize<'t>(stream)
        )

    static member inline read_string(path: string) = 
        catch (fun _ -> 
            System.IO.File.ReadAllText(path)
        )

    static member inline read_dir(path: string) = 
        catch (fun _ -> 
            System.IO.Directory.EnumerateFileSystemEntries(path)
        )

    static member inline write (path: _) (data: _) = 
        Internal.Write.Invoke(path, data) 
    
    static member inline path_exists (path: string) = 
        Path.Exists(path)

    static member inline dirname (path: string) = 
        Path.GetDirectoryName(path)

    static member inline filename (path: string) = 
        Path.GetFileName(path)

    static member inline path_extension (path: string) = 
        Path.GetExtension(path)

    static member inline with_extension (extension:string) (path: string) = 
        Path.ChangeExtension(path, extension)

    /// get env variable by name
    static member inline env (name:string) = 
        match System.Environment.GetEnvironmentVariable(name) with
        | null -> ValueNone
        | x -> ValueSome x

    /// ReadOnlySpan byte -> string
    static member inline from_utf8 (utf8bytes:ReadOnlySpan<byte>) = 
        System.Text.Encoding.UTF8.GetString(utf8bytes)

    static member inline utf8 (string:string) = 
        System.Text.Encoding.UTF8.GetBytes(string)

    static member inline json (anytype:'t) = System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(anytype)
    static member inline json_str (anytype:'t) = System.Text.Json.JsonSerializer.Serialize(anytype)

    /// string -> lines
    static member inline lines (string:string) = string.Split('\n')

type System.Threading.Tasks.Task with
    static member await (task': System.Threading.Tasks.Task<'t>) = task'.GetAwaiter().GetResult()

#endif
