open Expecto

let eq expected actual =
    Expect.equal actual expected "not equal"



let testRoot =
    testList "root" [
        test "array tests" {
            let arr = [| 1; 2; 3 |]
            let lambda1 = fun v -> v + 1
            eq (Array.map lambda1 arr) (map lambda1 arr)
            let lambda2 = fun i v -> () // just that it compiles
            eq (Array.iteri lambda2 arr) (iteri lambda2 arr)
        }
    ]


[<EntryPoint>]
let main argv = runTestsWithCLIArgs [] argv testRoot
