module Types

type Pencil = int[]
type Tile =
| Number of int
| Pencil of Pencil

let (|Number|_|) = function | Number n -> Some n | _ -> None
let (|Pencil|_|) = function | Pencil p -> Some p | _ -> None

type Board = Tile [,]

type Point = { x: int; y: int }

let allPencils = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]

let getSquare (point: Point) (board: Board) : Tile[] =
    let startRow = point.x / 3 * 3
    let startCol = point.y / 3 * 3

    board.[startRow..(startRow + 2), startCol..(startCol + 2)]
    |> Seq.cast<Tile>
    |> Seq.toArray

let getRow (row: int) (board: Board): Tile[] =
    board.[row, *]

let getColumn (column: int) (board: Board): Tile[] =
    board.[*, column]

let setTile (point: Point) (tile: Tile) (board: Board): unit =
    let newTile =
        match tile with
        | Pencil p when p.Length = 1 -> Number(p.[0])
        | _ -> tile

    Array2D.set board point.x point.y newTile