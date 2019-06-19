module DiffieHellman

open System.Security.Cryptography
open System.Numerics

let generator = new RNGCryptoServiceProvider()

let rec createKey (max: BigInteger) = 
    let bytes = Array.create 8 0uy
    generator.GetBytes(bytes)
    let key = BigInteger(bytes) % max

    if (key > 1I) then key else (createKey max)

let privateKey (primeP : BigInteger) = 
    createKey primeP

let publicKey primeP primeG privateKey = 
    BigInteger.ModPow(primeG, privateKey, primeP)

let secret primeP publicKey privateKey = 
    BigInteger.ModPow(publicKey, privateKey, primeP)
