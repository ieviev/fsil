open Expecto
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

    static member IterateWhile(self: Tree<'T>, cond: byref<bool>, fn: 'T -> unit) : unit =
        if cond then
            match self with
            | Leaf x -> fn x
            | Node(v, l, r) ->
                Tree.IterateWhile(l, &cond, fn)

                if cond then
                    fn v

                if cond then
                    Tree.IterateWhile(r, &cond, fn)

    static member Length(self: Tree<'T>) : int =
        let mutable len_total = 0
        self |> iter (fun _ -> len_total <- len_total + 1)
        len_total

    static member inline Default<'inner
        when (^inner or Internal.Default): (static member Default:
            (^inner -> unit) -> ^inner)>
        ((_f))
        : Tree< ^inner > =
        let inner_default: 'inner = default_type ()
        Leaf(inner_default)


// ensure all basic members exist, compile and run
let inline basic_collection_tests(data) : Test =
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
            let map_ = data |> map (fun v -> v + 1)
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
    ]

open Fsil.Internal
open System.Runtime.InteropServices

let test_is_empty =
    test "is_empty" {
        let l = is_empty ""
        ()
    }

let test_value =
    test "value" {
        eq 1 (value (ValueSome 1))
        eq 1 (value (Some 1))
        eq 1 (value (Ok 1))
        ()
    }


let testRoot =
    testList "root" [
        basic_collection_tests [|
            1
            2
            3
        |]
        basic_collection_tests [
            1
            2
            3
        ]
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

            let indices = [| 0..2 |]

            data |> iteri (fun (i: int) v -> eq indices[i] i)
            ()
        }

        test "defaults" {
            let _ = default_type<int> ()
            let _ = default_type<uint> ()
            let _ = default_type<string> ()
            let _ = default_type<bool> ()
            let _ = default_type<byte> ()
            ()
        }

        test "quantifiers" {
            let data = (Tree.Node(15, Tree.Leaf 2, Tree.Leaf 21))
            eq false (data |> forall (fun v -> v > 2))
            eq true (data |> forall (fun v -> v >= 2))
            eq true (data |> exists (fun v -> v = 15))
            eq false (data |> exists (fun v -> v = 16))

            let data2 = [|
                15
                2
                21
            |]

            eq false (data2 |> forall (fun v -> v > 2))
            eq true (data2 |> forall (fun v -> v >= 2))
            eq true (data2 |> exists (fun v -> v = 15))
            eq false (data2 |> exists (fun v -> v = 16))

            eq true (None |> forall (fun v -> v = 1))
            eq true (Some 1 |> forall (fun v -> v = 1))
            eq false (Some 2 |> forall (fun v -> v = 1))
        }

        test "dict" {
            let d =
                System.Collections.Generic.Dictionary(
                    dict [
                        "key1", 1
                        "key2", 2
                    ]
                )

            eq (some 1) (d |> try_item "key1")
            eq (none) (d |> try_item "key3")
            eq (2) (d |> try_item "key3" |> default_with (fun v -> 2))
        }

        test "result" {
            let res = Ok(5)

            let args = [|
                if is_ok res then
                    value res
            |]

            eq args [| 5 |]
        }

        test "find" {
            let res = [| 1..3 |]

            eq (some 3) (res |> find (fun v -> v = 3))
            eq none (res |> find (fun v -> v = 4))
            eq none ([ 1..3 ] |> find (fun v -> v = 4))
        }

        test "bind" {
            let res = [| 1..3 |]

            eq [||] (res |> bind (fun v -> [||]))

            eq
                [|
                    1
                    1
                    2
                    2
                    3
                    3
                |]
                (res
                 |> bind (fun v -> [|
                     v
                     v
                 |]))
        }
        test "position" {
            let res = [| 1..3 |]

            eq (some 0) (res |> position (fun v -> v = 1))
            eq (some 1) (res |> position (fun v -> v = 2))
            eq (some 2) (res |> position (fun v -> v = 3))
            eq (none) (res |> position (fun v -> v = 10))
        }

        test "item" {
            let res = [|
                5
                10
                15
            |]

            eq 5 (item 0 res)

            eq
                5
                (item 0 [
                    5
                    10
                    15
                ])
        }

        test "tuples" {
            let t2 = (1, 2)
            let t3 = (1, 2, 3)
            let t3s = struct (1, 2, 3)

            eq 1 (_1 t2)
            eq 2 (_2 t2)

            eq 1 (_1 t3)
            eq 2 (_2 t3)
            eq 3 (_3 t3)

            eq 1 (_1 t3s)
            eq 2 (_2 t3s)
            eq 3 (_3 t3s)
        }
        test_is_empty
    ]

[<EntryPoint>]
let main argv = runTestsWithCLIArgs [] argv testRoot
