module Types

type Pencil = int[]
type Tile =
| Number of int
| Pencil of Pencil

let (|Number|_|) = function | Number n -> Some n | _ -> None
let (|Pencil|_|) = function | Pencil p -> Some p | _ -> None

type Board = Tile [,]

type Point = { x: int; y: int }

let allPencils = [| 1..9 |]

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


//let cboard : int[,] = array2D [
//    [7; 2; 6; 4; 1; 3; 5; 8; 9]
//    [9; 1; 8; 6; 5; 2; 7; 3; 4]
//    [5; 4; 3; 8; 9; 7; 2; 6; 1]
//    [4; 8; 9; 5; 2; 1; 6; 7; 3]
//    [2; 3; 5; 9; 7; 6; 4; 1; 8]
//    [1; 6; 7; 3; 8; 4; 9; 5; 2]
//    [3; 5; 2; 7; 4; 8; 1; 9; 6]
//    [6; 9; 1; 2; 3; 5; 8; 4; 7]
//    [8; 7; 4; 1; 6; 9; 3; 2; 5]
//]

//let validate (point: Point) (board: Board) =
//    match board.[point.x, point.y] with
//    | Pencil p when System.Linq.Enumerable.Contains(p, cboard.[point.x, point.y]) -> true
//    | Number n when n = cboard.[point.x, point.y] -> true
//    | _ -> raise (new System.Exception(sprintf "Error found: %d, %d" point.x point.y))

let setTile (point: Point) (tile: Tile) (board: Board): unit =
    //if (validate point board) then
    Array2D.set board point.x point.y tile