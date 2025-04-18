﻿open Expecto
open System

let eq expected actual =
    Expect.equal actual expected "not equal"


type Tree<'T> =
    | Leaf of 'T
    | Node of 'T * Tree<'T> * Tree<'T>

    static member Map(self: Tree<'T>, fn: 'T -> 'U) : Tree<'U> =
        match self with
        | Leaf x -> Leaf(fn x)
        | Node(v, l, r) ->
            let new_l = Tree.Map(l, fn)
            let new_r = Tree.Map(r, fn)
            Node(fn v, new_l, new_r)

    static member Iterate(self: Tree<'T>, fn: 'T -> unit) : unit =
        match self with
        | Leaf x -> fn x
        | Node(v, l, r) ->
            Tree.Iterate(l, fn)
            fn v
            Tree.Iterate(r, fn)

    static member IterateWhile
        (self: Tree<'T>, cond: byref<bool>, fn: 'T -> unit)
        : unit =
        if cond then
            match self with
            | Leaf x -> fn x
            | Node(v, l, r) ->
                Tree.IterateWhile(l, &cond, fn)

                if cond then
                    fn v

                if cond then
                    Tree.IterateWhile(r, &cond, fn)

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
            let mapi_ = data |> mapi (fun (i: int) v -> v + 1)
            ()
        }
        test "len" {
            let len_: int = len data
            ()
        }
        test "is_empty" {
            let empty: bool = is_empty data
            ()
        }
        test "default" {
            let default_: ^a = _default< ^a> ()
            ()
        }
    ]

open Fsil.Internal
open System.Runtime.InteropServices

let testRoot =
    testList "root" [


        basic_collection_tests [| 1; 2; 3 |]
        basic_collection_tests [ 1; 2; 3 ]
        basic_collection_tests (Some 1)
        basic_collection_tests (ValueSome 1)

        test "tree_tests" {
            let data = (Tree.Node(15, Tree.Leaf 2, Tree.Leaf 21))
            let tree_count = data |> fold 0 (fun acc _ -> acc + 1)
            eq tree_count 3
            let tree_sum = data |> fold 0 (fun acc v -> acc + v)
            eq tree_sum 38
            let mappedx2 = data |> map (fun v -> v * 2)
            eq (Tree.Node(30, Tree.Leaf 4, Tree.Leaf 42)) mappedx2
            eq 3 (len data)
            let indices = [| 0; 1; 2 |]
            data |> iteri (fun (i: int) v -> eq indices[i] i)
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

        test "quantifiers" {
            let data = (Tree.Node(15, Tree.Leaf 2, Tree.Leaf 21))
            eq false (data |> forall (fun v -> v > 2))
            eq true (data |> forall (fun v -> v >= 2))
            eq true (data |> exists (fun v -> v = 15))
            eq false (data |> exists (fun v -> v = 16))

            let data2 = [| 15; 2; 21 |]

            eq false (data2 |> forall (fun v -> v > 2))
            eq true (data2 |> forall (fun v -> v >= 2))
            eq true (data2 |> exists (fun v -> v = 15))
            eq false (data2 |> exists (fun v -> v = 16))
        }

        test "spans" {
            let data = ResizeArray [ 15; 2; 21 ]
            let span_data = span data

            eq false (span_forall (span_data, (fun v -> v > 2)))
            eq true (span_forall (span_data, (fun v -> v >= 2)))
            eq true (span_exists (span_data, (fun v -> v = 15)))
            eq false (span_exists (span_data, (fun v -> v = 16)))
        }
    ]

[<EntryPoint>]
let main argv = runTestsWithCLIArgs [] argv testRoot
