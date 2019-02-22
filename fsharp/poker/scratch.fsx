let isEven num = 
    printfn "Number %i" num
    if num % 2 = 0 then
        Some num
    else
        None

[1..10]
|> Seq.choose isEven
|> Seq.tryHead