module SumOfMultiples
let sum (numbers: int list) (upperBound: int): int = 
    let isMultipleOfAny x = List.exists (fun num -> x % num = 0) numbers
    List.where isMultipleOfAny [1..(upperBound - 1)] |> List.sum