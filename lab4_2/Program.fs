open System

type Tree =
    | Empty
    | Node of int * Tree * Tree

let rec readNumber prompt =
    printf "%s" prompt
    match Int32.TryParse(Console.ReadLine()) with
    | (true, n) when n >= 0 -> n
    | _ -> 
        printfn "Ошибка! Введите целое положительное число!"
        readNumber prompt

let rnd = Random()
let rec buildRandom (size: int) : Tree =
    if size <= 0 then Empty
    else
        let value = rnd.Next(1, 21)
        let leftSize = size / 2
        let rightSize = size - 1 - leftSize
        Node(value, buildRandom leftSize, buildRandom rightSize)

let rec calculateSums (tree: Tree) : int * int =
    match tree with
    | Empty -> (0, 0)
    | Node (value, left, right) ->
        let (lEven, lOdd) = calculateSums left
        let (rEven, rOdd) = calculateSums right
        
        if value % 2 = 0 then
            (value + lEven + rEven, lOdd + rOdd)
        else
            (lEven + rEven, value + lOdd + rOdd)


let rec printTree (level: int) (tree: Tree) =
    match tree with
    | Empty -> ()
    | Node (value, left, right) ->
        printfn "%s- %d" (String.replicate level "    ") value
        printTree (level + 1) left
        printTree (level + 1) right

[<EntryPoint>]
let main _ =
    let n = readNumber "Введите количество узлов: "

    let tree = buildRandom n
    printfn "\n--- Структура дерева ---"
    printTree 0 tree

    let (evenSum, oddSum) = calculateSums tree
    
    printfn "\nСумма четных: %d" evenSum
    printfn "Сумма нечетных: %d" oddSum

    if evenSum > oddSum then printfn "Результат: Сумма ЧЕТНЫХ больше."
    elif oddSum > evenSum then printfn "Результат: Сумма НЕЧЕТНЫХ больше."
    else printfn "Результат: Суммы равны."
    0