let eps = 1e-10

let dichotomy f a b =
    let rec loop a b =
        let c = (a + b) / 2.0
        if abs(f c) < eps then c
        else if f a * f c < 0.0 then loop a c
        else loop c b
    loop a b

let iterations phi x0 = 
    let rec loop x =
        let x1 = phi x
        if abs(x1 - x) < eps then x1
        else loop x1
    loop x0

let newton f f' x0 =
    let rec loop x =
        let x1 = x - f x / f' x
        if abs(x1 - x) < eps then x1
        else loop x1
    loop x0

let f1 x = 3. * (log x) ** 2. + 6. * log x - 5.0
let f2 x = 0.6 * (3. ** x) - 2.3 * x - 3.0
let f3 x = x ** 2. - log(1. + x) - 3.0

let f1' x = (6. * log x) / x + 6. / x
let f2' x = 0.6 * log 3. * (3. ** x) - 2.3
let f3' x = 2. * x - 1. / (1. + x)

let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x
let phi3 x = x - f3 x / f3' x

let main =
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f1 1. 3.) (iterations phi1 1.0) (newton f1 f1' 2.5)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f2 2. 3.) (iterations phi2 2.5) (newton f2 f2' 2.5)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f3 2. 3.) (iterations phi3 2.0) (newton f3 f3' 2.5)

main
