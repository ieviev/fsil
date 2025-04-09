# fsil: F# inline library

<a href="https://www.nuget.org/packages/fsil"><img alt="Nuget" src="https://img.shields.io/nuget/v/fsil"></a>

Features:

- a small generic utils library like [FSharpPlus](https://github.com/fsprojects/FSharpPlus)
- all functions are inline and trimmable, won't bloat your binary size: you pay for only what you use
- Fable compatible
- **zero cost**: does not create junk variables or implicit allocations
- uses `[<AutoOpen>]` so all functions are in the global namespace

Example:

![](./data/demo.png)

[Benchmark comparisons](./src/fsil.benchmarks/Program.fs) to FSharpPlus

![](./data/benchmarks.png)

####

I love F# for high performance programming and this makes high-level generic F# a little more feasible without punishing the user performance-wise.
The functions in this library compile down to **exactly the same form as the (optimal) resolved implementation** so `iter f x` to the compiler is **identical** to e.g., `ValueOption.iter f x` or `Array.iter f x`.

Currently this library contains a fairly small set of functions:

`iter`, `iteri`, `map`, `mapi`, `is_some`, `is_none`, `some`, `none`, `value`, `len`, `enum`, `enumv`, `_default`, `default_with`, `zero`, `one`, `print`.

You can also define your own implementations as static members. [here is an example](./src/fsil.test/Program.fs) for `iter` and `map` on a Tree, for documentation just look at the [source code itself](./src/fsil/Library.fs).

```fsharp
type Tree<'T> =
    | Leaf of 'T
    | Node of 'T * Tree<'T> * Tree<'T>

    static member Map(self: Tree<'T>, fn: 'T -> 'U) : Tree<'U> =
        match self with
        | Leaf x -> Leaf(fn x)
        | Node(v, l, r) ->
            let new_l = Tree.Map((l, fn))
            let new_r = Tree.Map((r, fn))
            Node(fn v, new_l, new_r)

    static member Iterate(self: Tree<'T>, fn: 'T -> unit) : unit =
        match self with
        | Leaf x -> fn x
        | Node(v, l, r) ->
            fn v
            Tree.Iterate((l, fn))
            Tree.Iterate((r, fn))

let tree1 = Leaf 1
let iter = tree1 |> iter (fun v -> print v)
let mapped: Tree<int> = tree1 |> map (fun v -> v + 1)
// these implementations are generated from Iterate and Map
let iteri = tree1 |> iteri (fun idx v -> print v)
let mapped: Tree<int> = tree1 |> mapi (fun idx v -> idx + v + 1)
let sum = tree1 |> fold 0 (fun acc v -> acc + 1 )
let sum2 = tree1 |> foldi 0 (fun idx acc v -> acc + 1 )
```

Most important remember to have (f#)un! :)
