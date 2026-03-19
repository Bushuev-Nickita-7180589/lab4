//Бушуев Никита Николаевич БАС-1(2024)
//Вариант 2
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

let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then
            Node(v, insert value left, right)
        else if value > v then
            Node(v, left, insert value right)
        else
            tree

let buildSearchTree count =
    let rec generate uniqueValues remaining =
        if remaining <= 0 then 
            uniqueValues
        else
            let value = rnd.Next(1, 21)
            if List.contains value uniqueValues then
                generate uniqueValues remaining
            else
                generate (value :: uniqueValues) (remaining - 1)
    
    let values = generate [] count
    printfn "Сгенерированные числа (в случайном порядке): %A" values
    
    List.fold (fun tree value -> insert value tree) Empty values

let rec fold f acc tree =
    match tree with
    | Empty -> acc
    | Node(value, left, right) ->
        let accLeft = fold f acc left
        let accCurrent = f accLeft value
        fold f accCurrent right

let rec printTree level tree =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printfn "%s- %d" (String.replicate level "    ") value
        printTree (level + 1) left
        printTree (level + 1) right

[<EntryPoint>]
let main _ =
    let n = readNumber "Введите количество узлов: "
    
    let tree = buildSearchTree n
    printfn "\nДерево поиска (повёрнутое, слева меньше, справа больше):"
    printTree 0 tree
    
    let evenSum = fold (fun acc x -> if x % 2 = 0 then acc + x else acc) 0 tree
    let oddSum = fold (fun acc x -> if x % 2 <> 0 then acc + x else acc) 0 tree
    
    printfn "\nСумма чётных: %d" evenSum
    printfn "Сумма нечётных: %d" oddSum
    
    if evenSum > oddSum then printfn "Чётных больше"
    elif oddSum > evenSum then printfn "Нечётных больше"
    else printfn "Суммы равны"
    0
