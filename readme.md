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

`iter`, `iteri`, `map`, `mapi`, `is_some`, `is_none`, `some`, `none`, `value`, `len`, `enum`, `enumv`, `default_value`, `zero`, `one`, `print`.

You can also define your own implementations as static members. here is an example for `iter` and `map` on a Tree, for documentation just look at the [source code itself](./src/fsil/Library.fs).

```fsharp
type Tree<'T> =
    | Leaf of 'T
    | Node of 'T * Tree<'T> * Tree<'T>

    static member Map
        ((self: Tree<'T>, fn: 'T -> 'U), _tmp: Fsil.Internal.Map -> unit)
        : Tree<'U> =
        match self with
        | Leaf x -> Leaf(fn x)
        | Node(v, l, r) ->
            let new_l = Tree.Map((l, fn), _tmp)
            let new_r = Tree.Map((r, fn), _tmp)
            Node(fn v, new_l, new_r)

    static member Iterate
        ((self: Tree<'T>, fn: 'T -> unit), _tmp: Fsil.Internal.Iterate -> unit)
        : unit =
        match self with
        | Leaf x -> fn x
        | Node(v, l, r) ->
            fn v
            Tree.Iterate((l, fn), _tmp)
            Tree.Iterate((r, fn), _tmp)

let tree1 = Leaf 1
let tree2: Tree<int> = tree1 |> map (fun v -> v + 1)
let itertree = tree1 |> iter (fun v -> print v)
```

Most important remember to have (f#)un! :)

