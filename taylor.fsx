// function: x(3 - x) / (1 - x)^3
// series: 3x + 8x^2 + ... + n * (n + 2) * x^n
// a = 0.0
// b = 0.5

let eps = 1e-5

let f x = x * (3.0 - x) / (1.0 - x) ** 3.0
let a = 0.0
let b = 0.5
let n = 10

// Define a function to compute f using naive taylor series method
let taylor_naive x =
    let rec taylor_naive_rec x n acc =
        let term = float n * (float n + 2.0) * (x ** float n)
        if term > eps then
            taylor_naive_rec x (n + 1) (acc + term)
        else
            (acc, n)
    taylor_naive_rec x 1 0.0

// Define a function to do the same in a more efficient way
let taylor x =
    let rec taylor_rec x n acc prev =
        let term = prev * x * (float n + 1.0) * (float n + 3.0) / (float n * (float n + 2.0))
        if term > eps then
            taylor_rec x (n + 1) (acc + term) term
        else
            (acc, n)
    taylor_rec x 1 (3.0 * x) (3.0 * x)

let main =
    printfn "|  x  |   Builtin  | Smart Taylor | # terms | Dumb Taylor | # terms |"
    printfn "|-----|------------|--------------|---------|-------------|---------|"
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let (smartTaylor_value, smartTaylor_terms) = taylor x
        let (dumbTaylor_value, dumbTaylor_terms) = taylor_naive x
        printfn "|%5.2f| %10.6f |  %10.6f  | %5d |  %10.6f  | %5d |" x (f x) smartTaylor_value smartTaylor_terms dumbTaylor_value dumbTaylor_terms

main
