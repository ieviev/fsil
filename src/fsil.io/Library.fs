module Fsil.IO 

open System.Collections.Generic
open System.Runtime.CompilerServices
open System

[<AbstractClass>]
module Internal =
#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
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

         
#if FABLE_COMPILER
[<Fable.Core.Erase>]
#endif
[<AbstractClass; Sealed>]
module Io =
    open System.IO

    let inline read(path: string) = 
        catch (fun _ -> 
            System.IO.File.ReadAllBytes(path)
        )

    let inline read_json<'t>(path: string) = 
        catch (fun _ -> 
            use stream = System.IO.File.Open(path, FileMode.Open)
            System.Text.Json.JsonSerializer.Deserialize<'t>(stream)
        )

    let inline read_string(path: string) = 
        catch (fun _ -> 
            System.IO.File.ReadAllText(path)
        )

    let inline read_dir(path: string) = 
        catch (fun _ -> 
            System.IO.Directory.EnumerateFileSystemEntries(path)
        )

    let inline write (path: _) (data: _) = 
        Internal.Write.Invoke(path, data) 
    
    let inline exists (path: string) = 
        Path.Exists(path)

    let inline extension (path: string) = 
        Path.GetExtension(path)

    let inline with_extension (extension:string) (path: string) = 
        Path.ChangeExtension(path, extension)

    /// get env variable by name
    let inline env (name:string) = 
        match System.Environment.GetEnvironmentVariable(name) with
        | null -> ValueNone
        | x -> ValueSome x

    [<assembly: AutoOpen("Fsil.IO")>]
    do ()
