open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Benches() =

    member val SomeOption: option<int> = Unchecked.defaultof<_> with get, set
    member val SomeValueOption: voption<int> = Unchecked.defaultof<_> with get, set
    member val SomeArray: array<int> = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.SomeOption <- Some 1
        this.SomeValueOption <- ValueSome 1
        this.SomeArray <- [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
        ()

    [<Benchmark>]
    member self.FSharpPlusOptionMap() =
        self.SomeOption
        |> FSharpPlus.Operators.map (fun v -> v + 10)
        |> FSharpPlus.Operators.map (fun v -> v + 10)
        |> FSharpPlus.Operators.map (fun v -> v + 10)
        |> FSharpPlus.Operators.map (fun v -> v + 10)
        |> FSharpPlus.Operators.map (fun v -> v + 10)
        |> FSharpPlus.Operators.map (fun v -> v + 10)
        |> FSharpPlus.Operators.map (fun v -> v + 10)
        |> Option.defaultValue 5


    [<Benchmark>]
    member self.FsilOptionMap() =
        self.SomeOption
        |> Fsil.Abstract.map (fun v -> v + 10)
        |> Fsil.Abstract.map (fun v -> v + 10)
        |> Fsil.Abstract.map (fun v -> v + 10)
        |> Fsil.Abstract.map (fun v -> v + 10)
        |> Fsil.Abstract.map (fun v -> v + 10)
        |> Fsil.Abstract.map (fun v -> v + 10)
        |> Fsil.Abstract.map (fun v -> v + 10)
        |> Option.defaultValue 5

//     [<Benchmark>]
//     member self.FSharpPlusValueOptionMap() =
//         self.SomeValueOption
//         |> FSharpPlus.Operators.map (fun v -> v + 10)
//         |> FSharpPlus.Operators.map (fun v -> v + 10)
//         |> FSharpPlus.Operators.map (fun v -> v + 10)
//         |> FSharpPlus.Operators.map (fun v -> v + 10)
//         |> FSharpPlus.Operators.map (fun v -> v + 10)
//         |> FSharpPlus.Operators.map (fun v -> v + 10)
//         |> FSharpPlus.Operators.map (fun v -> v + 10)
//         |> ValueOption.defaultValue 5

//     [<Benchmark>]
//     member self.FsilValueOptionMap() =
//         self.SomeValueOption
//         |> Fsil.Abstract.map (fun v -> v + 10)
//         |> Fsil.Abstract.map (fun v -> v + 10)
//         |> Fsil.Abstract.map (fun v -> v + 10)
//         |> Fsil.Abstract.map (fun v -> v + 10)
//         |> Fsil.Abstract.map (fun v -> v + 10)
//         |> Fsil.Abstract.map (fun v -> v + 10)
//         |> Fsil.Abstract.map (fun v -> v + 10)
//         |> ValueOption.defaultValue 5


    [<Benchmark>]
    member self.FSharpPlusArrayIter() =
        let mutable result = 0

        self.SomeArray |> FSharpPlus.Operators.iter (fun v -> result <- result + v)
        self.SomeArray |> FSharpPlus.Operators.iter (fun v -> result <- result + v)
        self.SomeArray |> FSharpPlus.Operators.iter (fun v -> result <- result + v)
        self.SomeArray |> FSharpPlus.Operators.iter (fun v -> result <- result + v)
        self.SomeArray |> FSharpPlus.Operators.iter (fun v -> result <- result + v)


    [<Benchmark>]
    member self.FsilArrayIter() =
        let mutable result = 0

        self.SomeArray |> Fsil.Abstract.iter (fun v -> result <- result + v)
        self.SomeArray |> Fsil.Abstract.iter (fun v -> result <- result + v)
        self.SomeArray |> Fsil.Abstract.iter (fun v -> result <- result + v)
        self.SomeArray |> Fsil.Abstract.iter (fun v -> result <- result + v)
        self.SomeArray |> Fsil.Abstract.iter (fun v -> result <- result + v)


BenchmarkRunner.Run(typeof<Benches>) |> ignore
