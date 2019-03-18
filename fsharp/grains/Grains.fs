module Grains
open System.Numerics

let squareImpl n = BigInteger.Pow(bigint 2, n - 1)

let square (n: int): Result<uint64,string> = 
    match n with
    | x when x < 1 || x > 64 -> Error "square must be between 1 and 64"
    | n -> n |> squareImpl |> uint64 |> Ok

let total: Result<uint64,string> = 
    [1..64]
    |> List.map squareImpl
    |> List.fold (fun a b -> BigInteger.Add(a, b)) (BigInteger.Zero)
    |> uint64
    |> Ok