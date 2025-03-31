# FSIL: FSharp Inlines Library

<a href="https://www.nuget.org/packages/fsil"><img alt="Nuget" src="https://img.shields.io/nuget/v/fsil"></a>

Features:
- a small generic utils library like (https://github.com/fsprojects/FSharpPlus)
- all functions are inline and trimmable, won't bloat your binary size in any way
- Fable compatible
- does not create junk variables
- does not have implicit allocations
- uses `[<AutoOpen>]` so all functions are in the global namespace

Example:

![](./data/demo.png)

[benchmarks](./src/fsil.benchmarks/Program.fs) compared to FSharpPlus
    
![](./data/benchmarks.png)

#### 

I love F# for high performance programming and i've found this little utils library very useful as it makes high-level generic F# a little more feasible without punishing the user performance-wise. 
An interesting piece of trivia about this is that the functions in this library compile down to **exactly the same implementation
as the resolved generic** so `iter f x` to the compiler is **identical** to `ValueOption.iter f x`.

You can also define your own implementations for `iter` and `map` as static members. here is an example for a Tree, for documentation just look at the [source code itself](./src/fsil/Library.fs).

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

Most importantly remember to have fun! :)

