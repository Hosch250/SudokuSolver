type Pencil = int[]
type Tile = Number of int | Pencil of Pencil

let (|Number|_|) = function | Number n -> Some n | _ -> None
let (|Pencil|_|) = function | Pencil p -> Some p | _ -> None

type Board = Tile [,]

type Point = { x: int; y: int }

let getSquare (point: Point) (board: Board) : Tile[,] =
    let startRow = point.x / 3 * 3
    let startCol = point.y / 3 * 3
    board.[startRow..(startRow + 2), startCol..(startCol + 2)]

let getRow (row: int) (board: Board): Tile[] =
    board.[row, *]

let getColumn (column: int) (board: Board): Tile[] =
    board.[*, column]

let trimPencils (point: Point) (board: Board): Pencil =
    let allPencils = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]

    let getExisting (known: Tile[]) (set: Pencil) =
        let existingNums = Seq.choose (|Number|_|) known
        Array.except existingNums set

    let getValidInSquare (set: Pencil) =
        let square = getSquare point board
        getExisting (Seq.cast<Tile> square |> Seq.toArray) set

    let getValidInRow (set: Pencil) =
        let row = getRow point.x board
        getExisting row set

    let getValidInCol (set: Pencil) =
        let col = getColumn point.y board
        getExisting col set

    allPencils
    |> getValidInSquare
    |> getValidInRow
    |> getValidInCol

let setTile (point: Point) (pencil: Pencil) (board: Board): unit =
    Array2D.set board point.x point.y (if pencil.Length = 1 then Number(pencil.[0]) else Pencil(pencil))

let formatTile (tile: Tile): string =
    sprintf "%s" (match tile with | Number n -> sprintf "%i " n | _ -> "  ")

let print (board: Board): unit =
    board
    |> Array2D.iteri (fun i j value ->
        printf "%s%s" (formatTile value) (if j = Array2D.length2 board - 1 then "\n" else ""))

let board : int[,] = array2D [
    [4; 0; 0; 7; 0; 3; 0; 5; 0]
    [0; 0; 9; 8; 0; 4; 0; 0; 0]
    [0; 8; 7; 2; 0; 0; 0; 6; 0]
    [0; 0; 0; 0; 0; 0; 2; 3; 5]
    [0; 3; 2; 6; 0; 5; 4; 9; 0]
    [9; 5; 8; 0; 0; 0; 0; 0; 0]
    [0; 4; 0; 0; 0; 7; 9; 2; 0]
    [0; 0; 0; 4; 0; 6; 5; 0; 0]
    [0; 6; 0; 5; 0; 8; 0; 0; 3]
]

let mappedBoard =
    board
    |> Array2D.map (fun i -> if i = 0 then Pencil([||]) else Number(i))

let getPencilCount (board: Board) =
    board
    |> Seq.cast<Tile>
    |> Seq.choose (|Pencil|_|)
    |> Seq.length

let mutable pencilCount = getPencilCount mappedBoard

while pencilCount > 0 do
    mappedBoard
    |> Array2D.iteri (
        fun x y tile ->
            match tile with
            | Pencil _ ->
                let point = { x = x; y = y }
                let pencil = trimPencils point mappedBoard
                setTile point pencil mappedBoard
            | _ -> ()
    )

    let newPencilCount = getPencilCount mappedBoard
    pencilCount <- if (newPencilCount = pencilCount) then -1 else newPencilCount

print mappedBoard