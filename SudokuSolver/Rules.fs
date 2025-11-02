module Rules
open Types
open System.Linq
open System.Collections.Generic

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

/// this rule will filter pencils based on whether a row/column in a square is the only option for a given number
/// e.g. if tiles in the top row of a square are the only possible option for the number 5 in that square,
/// no tiles in that row outside of that square can be 5
/// returns true if any pencils were updated or square was set
let trimPencilsForExclusiveLineInSquare (point: Point) (board: Board) =
    let trimPencilsFromOtherSquares remove (otherTiles: IDictionary<Point, Tile>) =
        let tilesToChange =
            otherTiles.Values
            |> Seq.choose (|Pencil|_|)
            |> Seq.map (fun tile -> (Enumerable.Intersect(remove, tile) |> Seq.length))
            |> Seq.sum
        
        if tilesToChange > 0 then
            otherTiles
            |> Seq.choose (fun kv -> if kv.Value.IsPencil then Some kv else None)
            |> Seq.iter (fun tile -> (
                match tile.Value with
                | Pencil p -> setTile tile.Key (Array.except remove p |> Pencil) board
                | _ -> ()))
            true
        else false

    let getExclusiveInSet set otherTiles =
        let isNotInOtherSet (num: int) =
            otherTiles
            |> Seq.choose (|Pencil|_|)
            |> Seq.filter (fun f -> Seq.contains num f)
            |> Seq.isEmpty

        let values =
            set
            |> Seq.choose (|Pencil|_|)
            |> Seq.fold (fun acc next -> (Enumerable.Union(acc, next))) [||]
            |> Seq.filter isNotInOtherSet
            |> Seq.toArray

        values

    let hasOnlyOptionInRow () =
        let square = getSquare point board

        let setBeingChecked =
            match point.x with
            | 0 | 1 | 2 -> [square.[0]; square.[1]; square.[2]]
            | 3 | 4 | 5 -> [square.[3]; square.[4]; square.[5]]
            | 6 | 7 | 8 -> [square.[6]; square.[7]; square.[8]]

        let otherSets =
            match point.x with
            | 0 | 1 | 2 -> [square.[3]; square.[4]; square.[5]; square.[6]; square.[7]; square.[8]]
            | 3 | 4 | 5 -> [square.[0]; square.[1]; square.[2]; square.[6]; square.[7]; square.[8]]
            | 6 | 7 | 8 -> [square.[0]; square.[1]; square.[2]; square.[3]; square.[4]; square.[5]]

        let toRemove = getExclusiveInSet setBeingChecked otherSets
        
        if toRemove.Length > 0 then
            let row = getRow point.x board
            let otherTilesInRow =
                match point.y with
                | 0 | 1 | 2 -> dict [
                        { point with y = 3 }, row.[3]
                        { point with y = 4 }, row.[4]
                        { point with y = 5 }, row.[5]
                        { point with y = 6 }, row.[6]
                        { point with y = 7 }, row.[7]
                        { point with y = 8 }, row.[8]
                    ]
                | 3 | 4 | 5 -> dict [
                        { point with y = 0 }, row.[0]
                        { point with y = 1 }, row.[1]
                        { point with y = 2 }, row.[2]
                        { point with y = 6 }, row.[6]
                        { point with y = 7 }, row.[7]
                        { point with y = 8 }, row.[8]
                    ]
                | 6 | 7 | 8 -> dict [
                        { point with y = 0 }, row.[0]
                        { point with y = 1 }, row.[1]
                        { point with y = 2 }, row.[2]
                        { point with y = 3 }, row.[3]
                        { point with y = 4 }, row.[4]
                        { point with y = 5 }, row.[5]
                    ]

            trimPencilsFromOtherSquares toRemove otherTilesInRow
        else
            false

    let hasOnlyOptionInCol () =
        let square = getSquare point board

        let setBeingChecked =
            match point.y with
            | 0 | 3 | 6 -> [square.[0]; square.[3]; square.[6]]
            | 1 | 4 | 7 -> [square.[1]; square.[4]; square.[7]]
            | 2 | 5 | 8 -> [square.[2]; square.[5]; square.[8]]

        let otherSets =
            match point.y with
            | 0 | 3 | 6 -> [ square.[1]; square.[4]; square.[7]; square.[2]; square.[5]; square.[8]]
            | 1 | 4 | 7 -> [square.[0]; square.[3]; square.[6]; square.[2]; square.[5]; square.[8]]
            | 2 | 5 | 8 -> [square.[0]; square.[3]; square.[6]; square.[1]; square.[4]; square.[7]]

        let toRemove = getExclusiveInSet setBeingChecked otherSets
        
        if toRemove.Length > 0 then
            let col = getColumn point.y board
            let otherTilesInCol =
                match point.x with
                | 0 | 1 | 2 -> dict [
                        { point with x = 3 }, col.[3]
                        { point with x = 4 }, col.[4]
                        { point with x = 5 }, col.[5]
                        { point with x = 6 }, col.[6]
                        { point with x = 7 }, col.[7]
                        { point with x = 8 }, col.[8]
                    ]
                | 3 | 4 | 5 -> dict [
                        { point with x = 0 }, col.[0]
                        { point with x = 1 }, col.[1]
                        { point with x = 2 }, col.[2]
                        { point with x = 6 }, col.[6]
                        { point with x = 7 }, col.[7]
                        { point with x = 8 }, col.[8]
                    ]
                | 6 | 7 | 8 -> dict [
                        { point with x = 0 }, col.[0]
                        { point with x = 1 }, col.[1]
                        { point with x = 2 }, col.[2]
                        { point with x = 3 }, col.[3]
                        { point with x = 4 }, col.[4]
                        { point with x = 5 }, col.[5]
                    ]

            trimPencilsFromOtherSquares toRemove otherTilesInCol
        else
            false

    let updateRow = hasOnlyOptionInRow ()
    let updateCol = hasOnlyOptionInCol ()

    updateRow || updateCol