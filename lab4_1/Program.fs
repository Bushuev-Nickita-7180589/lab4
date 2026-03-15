open System

type Tree =
    | Empty
    | Node of string * Tree * Tree

let rec prependChar (symbol: char) (tree: Tree) : Tree =
    match tree with
    | Empty -> Empty
    | Node (text, left, right) ->
        Node ($"{symbol}{text}", prependChar symbol left, prependChar symbol right)

let rec printSimple (level: int) (tree: Tree) =
    match tree with
    | Empty -> ()
    | Node (text, left, right) ->
        let indent = String.replicate level "    "
        printfn "%s- %s" indent text
        printSimple (level + 1) left
        printSimple (level + 1) right

let rnd = Random()
let rec buildRandom (size: int) : Tree =
    if size <= 0 then Empty
    else
        let s = String(Array.init 3 (fun _ -> char (rnd.Next(97, 123))))
        let leftSize = size / 2
        let rightSize = size - 1 - leftSize
        Node(s, buildRandom leftSize, buildRandom rightSize)

let rec readNumber prompt =
    printf "%s" prompt
    match Int32.TryParse(Console.ReadLine()) with
    | (true, n) when n >= 0 -> n
    | _ -> 
        printfn "Ошибка! Введите целое положительное число!"
        readNumber prompt

let rec readChar prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    if not (String.IsNullOrEmpty(input)) then 
        input.[0] 
    else 
        printfn "Ошибка! Введите хотя бы один символ!"
        readChar prompt

[<EntryPoint>]
let main _ =
    let n = readNumber "Сколько узлов создать? "
    let sym = readChar "Какой символ добавить в начало? "

    let tree = buildRandom n
    
    printfn "\n--- Исходное дерево ---"
    printSimple 0 tree

    let newTree = prependChar sym tree
    
    printfn "\n--- Обновленное дерево (с символом '%c') ---" sym
    printSimple 0 newTree

    printfn "\nНажмите любую клавишу для выхода..."
    Console.ReadKey() |> ignore
    0