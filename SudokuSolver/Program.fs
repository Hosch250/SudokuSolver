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

let printLine (line: Tile[]): unit =
    printf "| "

    line
    |> Array.map formatTile
    |> Array.map (fun i -> printf "%s" i)
    |> ignore

    printfn " |"

let board : Board = array2D [
        [Number(4); Pencil([||]); Pencil([||]); Number(7); Pencil([||]); Number(3); Pencil([||]); Number(5); Pencil([||])]
        [Pencil([||]); Pencil([||]); Number(9); Number(8); Pencil([||]); Number(4); Pencil([||]); Pencil([||]); Pencil([||])]
        [Pencil([||]); Number(8); Number(7); Number(2); Pencil([||]); Pencil([||]); Pencil([||]); Number(6); Pencil([||])]
        [Pencil([||]); Pencil([||]); Pencil([||]); Pencil([||]); Pencil([||]); Pencil([||]); Number(2); Number(3); Number(5)]
        [Pencil([||]); Number(3); Number(2); Number(6); Pencil([||]); Number(5); Number(4); Number(9); Pencil([||])]
        [Number(9); Number(5); Number(8); Pencil([||]); Pencil([||]); Pencil([||]); Pencil([||]); Pencil([||]); Pencil([||])]
        [Pencil([||]); Number(4); Pencil([||]); Pencil([||]); Pencil([||]); Number(7); Number(9); Number(2); Pencil([||])]
        [Pencil([||]); Pencil([||]); Pencil([||]); Number(4); Pencil([||]); Number(6); Number(5); Pencil([||]); Pencil([||])]
        [Pencil([||]); Number(6); Pencil([||]); Number(5); Pencil([||]); Number(8); Pencil([||]); Pencil([||]); Number(3)]
    ]


let getPencilCount () =
    board
    |> Seq.cast<Tile>
    |> Seq.choose (|Pencil|_|)
    |> Seq.length

let mutable pencilCount = getPencilCount ()

while pencilCount > 0 do
    board
    |> Array2D.iteri (
        fun x y tile ->
            match tile with
            | Pencil _ ->
                let point = { x = x; y = y }
                let pencil = trimPencils point board
                setTile point pencil board
            | _ -> ()
    )

    let newPencilCount = getPencilCount ()
    pencilCount <- if (newPencilCount = pencilCount) then -1 else newPencilCount

print board