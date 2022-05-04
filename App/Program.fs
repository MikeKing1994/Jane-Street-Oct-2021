module Operators = 
    let (/) x y = 
        (x |> double) / (y |> double)

open Operators

let rec p (r: int) (m: int) (n: int): double = 
    if n = 0 
        then double 1
    else if n > 0 && r = 0 
        then double 0 
    else 
        (m/(m+n))*(p (r-1) m n) + (n/(m+n))*(p (r-1) (m+1) (n-1))

for i in [0..9] do
    printfn "%A" ((double 1)-(p (3*i-1) 0 i))