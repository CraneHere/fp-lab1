let eps = 1e-10

let rec dichotomy f a b =
    let fa = f a
    let fb = f b
    let rec loop a b fa fb =
        let c = (a + b) / 2.
        let fc = f c
        if abs(fc) < eps then c
        elif fa * fc < 0. then
            loop a c fa fc
        else                    
            loop c b fc fb
    loop a b fa fb 

let rec iterations f phi x =
    let x1 = phi x
    if abs(x1 - x) < eps then x1
    else iterations f phi x1

let rec newton f f' x =
    let x1 = x - f(x) / f'(x)
    if abs(x1 - x) < eps then x1 
    else newton f f' x1

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
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f1 1. 3.) (iterations f1 phi1 1.0) (newton f1 f1' 2.5)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f2 2. 3.) (iterations f2 phi2 2.5) (newton f2 f2' 2.5)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f3 2. 3.) (iterations f3 phi3 2.0) (newton f3 f3' 2.5)

main
