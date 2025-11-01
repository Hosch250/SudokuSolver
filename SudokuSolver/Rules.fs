module Rules
open Types

/// this rule will filter the pencil marks based on known numbers.
/// e.g. having a known 5 in the same row/column/square as the point
/// being checked will remove `5` from the pencil set for this point
/// returns false (because naively updating pencils doesn't affect the state of the board)
let setPencil (point: Point) (board: Board) =
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

    setTile point tile board
    false

/// this rule will assign a number to a tile if only one possible value for that tile is valid
/// returns true if a tile was set
let naive (point: Point) (board: Board) =
    match board.[point.x, point.y] with
    | Pencil p when p.Length = 1 ->
        setTile point (Number(p.[0])) board
        true
    | _ -> false

/// this rule will assign a number to a tile based on whether it is the only valid option for a given number in the row/column/square
/// e.g. if a square is the only possible option for the number 5 in its row/column/square it will be set to the number 5
/// returns true if a tile was set
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

        let x =
            set 
            |> Seq.map (fun i -> (i, numCount i))
            |> Seq.toArray

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

    match tile with
    | Pencil p when p.Length = 1 ->
        setTile point (Number(p.[0])) board
        true
    | _ -> false