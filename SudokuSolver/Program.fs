open Types
open Rules

let print (board: Board): unit =
    let formatTile (tile: Tile): string =
        sprintf "%s" (match tile with | Number n -> sprintf "%i " n | _ -> "  ")

    board
    |> Array2D.iteri (fun i j value ->
        printf "%s%s" (formatTile value) (if j = Array2D.length2 board - 1 then "\n" else ""))

let createBoard (input: int[,]) =
    input
    |> Array2D.map (fun i -> if i = 0 then Pencil([||]) else Number(i))

let board : int[,] = array2D [
    [0; 0; 9; 7; 6; 2; 0; 0; 0]
    [2; 0; 0; 1; 0; 0; 0; 0; 0]
    [0; 0; 0; 0; 0; 0; 6; 2; 1]
    [8; 5; 0; 0; 0; 0; 1; 0; 4]
    [7; 0; 6; 0; 0; 0; 9; 0; 2]
    [9; 0; 2; 0; 0; 0; 0; 8; 5]
    [6; 9; 8; 0; 0; 0; 0; 0; 0]
    [0; 0; 0; 0; 0; 1; 0; 0; 9]
    [0; 0; 0; 9; 8; 5; 2; 0; 0]
]

let mappedBoard =
    board
    |> Array2D.map (fun i -> if i = 0 then Pencil(allPencils) else Number(i))

let getPencilCount (board: Board) =
    board
    |> Seq.cast<Tile>
    |> Seq.choose (|Pencil|_|)
    |> Seq.length

let loopUntilNoChanges action = 
    let mutable pencilCount = getPencilCount mappedBoard

    while pencilCount > 0 do
        mappedBoard
        |> Array2D.iteri (
            fun x y tile ->
                match tile with
                | Pencil _ ->
                    action ({ x = x; y = y })
                | _ -> ()
        )

        let newPencilCount = getPencilCount mappedBoard
        pencilCount <- if (newPencilCount = pencilCount) then -1 else newPencilCount

print mappedBoard
printfn "-------------------"

let rules = [naive; onlyInSet]
loopUntilNoChanges (fun point -> (
    rules
    |> Seq.iter (fun rule -> loopUntilNoChanges (fun point -> rule point mappedBoard))
))

print mappedBoard