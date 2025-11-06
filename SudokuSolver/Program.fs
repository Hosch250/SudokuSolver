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
    [7; 0; 0; 0; 1; 0; 5; 0; 0]
    [9; 0; 8; 0; 0; 0; 0; 3; 4]
    [0; 0; 0; 8; 0; 0; 2; 0; 1]
    [0; 0; 0; 5; 2; 0; 0; 0; 0]
    [0; 3; 0; 0; 0; 0; 0; 1; 0]
    [0; 0; 0; 0; 8; 4; 0; 0; 0]
    [3; 0; 2; 0; 0; 8; 0; 0; 0]
    [6; 9; 0; 0; 0; 0; 8; 0; 7]
    [0; 0; 4; 0; 6; 0; 0; 0; 5]
]

let loopUntilChange action board =
    seq {0..8}
    |> Seq.exists (fun x ->
        seq{0..8}
        |> Seq.exists (fun y -> action { x = x; y = y } board))

let runRule key point (board: Board) =
    match key with
    | nameof setPencil -> board.[point.x, point.y].IsPencil
    | nameof naive -> board.[point.x, point.y].IsPencil
    | nameof onlyInSet -> board.[point.x, point.y].IsPencil
    | nameof trimPencilsForExclusiveLineInSquare -> point.x % 3 = point.y % 3
    | _ -> raise (System.NotImplementedException ())

let rules = dict [
    nameof setPencil, setPencil
    nameof naive, naive
    nameof onlyInSet, onlyInSet
    //nameof trimPencilsForExclusiveLineInSquare, trimPencilsForExclusiveLineInSquare
    nameof combinations, combinations
]

let rec runRules board =
    let change =
        rules
        |> Seq.exists (fun rule -> loopUntilChange (fun point board -> runRule rule.Key point board && rule.Value point board) board)
        
    if change then runRules board

let mappedBoard =
    board
    |> Array2D.map (fun i -> if i = 0 then Pencil(allPencils) else Number(i))

runRules mappedBoard

print mappedBoard