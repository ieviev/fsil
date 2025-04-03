open Expecto
open System

let eq expected actual =
    Expect.equal actual expected "not equal"


type Tree<'T> =
    | Leaf of 'T
    | Node of 'T * Tree<'T> * Tree<'T>

    static member Map((self: Tree<'T>, fn: 'T -> 'U)) : Tree<'U> =
        match self with
        | Leaf x -> Leaf(fn x)
        | Node(v, l, r) ->
            let new_l = Tree.Map((l, fn))
            let new_r = Tree.Map((r, fn))
            Node(fn v, new_l, new_r)

    static member Iterate((self: Tree<'T>, fn: 'T -> unit)) : unit =
        match self with
        | Leaf x -> fn x
        | Node(v, l, r) ->
            fn v
            Tree.Iterate((l, fn))
            Tree.Iterate((r, fn))

    static member IterateIndexed((self: Tree<'T>, fn: int -> 'T -> unit)) : unit = ()

    static member MapIndexed((self: Tree<'T>, fn: int -> 'T -> 'U)) : Tree<'U> =
        Leaf Unchecked.defaultof<_>

    static member Length((self: Tree<'T>, _f: unit -> unit)) : int =
        let mutable len_total = 0
        self |> iter (fun _ -> len_total <- len_total + 1)
        len_total

    static member inline Default<'inner
        when (^inner or Internal.Default): (static member Default:
            (^inner -> unit) -> ^inner)>
        ((_f))
        : Tree< ^inner > =
        let inner_default: 'inner = _default ()
        Leaf(inner_default)

// ensure all basic members exist, compile and run
let inline basic_collection_tests (data: ^a) : Test =
    testList $"collection tests {data.GetType().Name}" [
        test "iter" {
            let iter_: unit = data |> iter (fun v -> ())
            ()
        }
        test "iteri" {
            let iteri_: unit = data |> iteri (fun (i: int) v -> ())
            ()
        }
        test "map" {
            let map_: 'c = data |> map (fun v -> v + 1)
            ()
        }
        test "mapi" {
            let mapi_: 'd = data |> mapi (fun (i: int) v -> ())
            ()
        }
        test "len" {
            let len_: int = len data
            ()
        }
        test "default" {
            let default_: ^a = _default< ^a> ()
            ()
        }
    ]

open Fsil.Internal

let testRoot =
    testList "root" [
        basic_collection_tests (ResizeArray [ 1; 2; 3 ])
        basic_collection_tests [| 1; 2; 3 |]
        basic_collection_tests [ 1; 2; 3 ]
        basic_collection_tests (Some 1)
        basic_collection_tests (ValueSome 1)
        test "tree_tests" {
            let data = (Tree.Node(1, Tree.Leaf 1, Tree.Leaf 2))
            data |> iter (fun v -> ())
            data |> iteri (fun (i: int) v -> ())
            let _ = data |> map (fun v -> v + 1)
            let _ = data |> mapi (fun (i: int) v -> ())
            let _ = len data
            let _ = _default<Tree<int>> ()
            ()
        }
        test "defaults" {
            let _ = _default<int> ()
            let _ = _default<uint> ()
            let _ = _default<string> ()
            let _ = _default<bool> ()
            let _ = _default<byte> ()
            ()
        }
    ]


[<EntryPoint>]
let main argv = runTestsWithCLIArgs [] argv testRoot
