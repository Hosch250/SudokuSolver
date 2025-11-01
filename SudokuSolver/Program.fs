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
    [0; 3; 0; 0; 0; 0; 0; 7; 4]
    [0; 0; 0; 0; 2; 0; 0; 0; 3]
    [6; 0; 0; 0; 7; 1; 8; 0; 0]
    [4; 0; 0; 7; 0; 0; 0; 3; 0]
    [0; 9; 0; 0; 0; 0; 0; 5; 0]
    [0; 8; 0; 0; 0; 9; 0; 0; 1]
    [0; 0; 2; 9; 3; 0; 0; 0; 6]
    [8; 0; 0; 0; 6; 0; 0; 0; 0]
    [3; 5; 0; 0; 0; 0; 0; 2; 0]
]


let getPencilCount (board: Board) =
    board
    |> Seq.cast<Tile>
    |> Seq.choose (|Pencil|_|)
    |> Seq.length

let loopUntilChange action board =
    seq {0..8}
    |> Seq.exists (fun x ->
        seq{0..8}
        |> Seq.exists (fun y -> action { x = x; y = y } board))

let rules = [setPencil; naive; onlyInSet]
let rec runRules board =
    let change =
        rules
        |> Seq.exists (fun rule -> loopUntilChange rule board)
        
    if change then runRules board

let mappedBoard =
    board
    |> Array2D.map (fun i -> if i = 0 then Pencil(allPencils) else Number(i))

runRules mappedBoard

print mappedBoard