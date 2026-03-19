//Бушуев Никита Николаевич БАС-1(2024)
//Вариант 2
open System

type Tree =
    | Empty
    | Node of string * Tree * Tree

let rec readNumber prompt =
    printf "%s" prompt
    match Int32.TryParse(Console.ReadLine()) with
    | (true, n) when n >= 0 -> n
    | _ -> 
        printfn "Ошибка! Введите целое положительное число!"
        readNumber prompt

let rnd = Random()

let randomString() =
    String(Array.init 3 (fun _ -> char (rnd.Next(97, 123))))

let rec buildRandomTree size =
    if size <= 0 then Empty
    else
        let value = randomString()
        let leftSize = size / 2
        let rightSize = size - 1 - leftSize
        Node(value, buildRandomTree leftSize, buildRandomTree rightSize)

let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node(value, left, right) ->
        Node(f value, map f left, map f right)

let rec printTree level tree =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printfn "%s- %s" (String.replicate level "    ") value
        printTree (level + 1) left
        printTree (level + 1) right

[<EntryPoint>]
let main _ =
    let n = readNumber "Введите количество узлов: "
    printf "Какой символ? "
    let c = Console.ReadLine().[0]
    
    let tree = buildRandomTree n
    printfn "\nИсходное дерево:"
    printTree 0 tree
    
    let newTree = map (fun s -> string c + s) tree
    
    printfn "\nПосле добавления символа:"
    printTree 0 newTree
    0
