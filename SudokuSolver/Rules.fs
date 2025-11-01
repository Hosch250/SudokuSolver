module Rules
open Types

/// this rule will filter the pencil marks based on known numbers.
/// e.g. having a known 5 in the same row/column/square as the point
/// being checked will remove `5` from the pencil set for this point
let naive (point: Point) (board: Board) =
    let getExisting (known: Tile[]) (set: Pencil) =
        let existingNums = Seq.choose (|Number|_|) known
        Array.except existingNums set

    let getValidInSquare (set: Pencil) =
        let square = getSquare point board
        getExisting square set

    let getValidInRow (set: Pencil) =
        let row = getRow point.x board
        getExisting row set

    let getValidInCol (set: Pencil) =
        let col = getColumn point.y board
        getExisting col set

    let tile =
        match board.[point.x, point.y] with
        | Pencil p -> 
            p
            |> getValidInSquare
            |> getValidInRow
            |> getValidInCol
            |> Pencil
        | _ -> board.[point.x, point.y]

    board
    |> setTile point (
        match tile with
        | Pencil p when p.Length = 1 -> Number(p.[0])
        | _ -> tile)

/// this rule will filter the pencil marks based on whether they are the only valid option for a given mark in the row/column/square
/// e.g. if a square is the only possible option for the number 5 its row/column/square it will be set to the number 5
let onlyInSet (point: Point) (board: Board) =
    let isOnlyOption (tiles: Tile[]) (set: Pencil) =
        let numCount (num: int) =
            tiles
            |> Seq.choose (|Pencil|_|)
            |> Seq.filter (fun f -> Seq.contains num f)
            |> Seq.tryExactlyOne

        let value =
            set 
            |> Seq.map (fun i -> (i, numCount i))
            |> Seq.choose (fun (num, count) -> if count.IsNone then None else Some num)
            |> Seq.tryExactlyOne

        match value with
        | Some s -> [|s|]
        | None -> set

    let isOnlyOptionInSquare (set: Pencil) =
        let square = getSquare point board
        isOnlyOption square set

    let isOnlyOptionInRow (set: Pencil) =
        let row = getRow point.x board
        isOnlyOption row set

    let isOnlyOptionInCol (set: Pencil) =
        let col = getColumn point.y board
        isOnlyOption col set

    let tile =
        match board.[point.x, point.y] with
        | Pencil p -> 
            p
            |> isOnlyOptionInSquare
            |> isOnlyOptionInRow
            |> isOnlyOptionInCol
            |> Pencil
        | _ -> board.[point.x, point.y]

    board
    |> setTile point (
        match tile with
        | Pencil p when p.Length = 1 -> Number(p.[0])
        | _ -> tile)