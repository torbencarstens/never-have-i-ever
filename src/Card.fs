module Card

type RawType =
    { id: int
      text: string
      count: int }

[<CustomEquality; NoComparison>]
type Type =
    { Id: int
      Text: string
      Count: int }
    override this.Equals(other) =
        match other with
        | :? Type as other ->
            this.Id = other.Id
        | _ -> false

    override this.GetHashCode() = hash this.Id

let Into raw =
        {
            Id = raw.id
            Text = raw.text
            Count = raw.count
        }

let decreaseCount card cards =
    match card with
    | Some card ->
        List.map (fun c ->
            if c = card then { c with Count = c.Count - 1 } else c) cards
    | None -> cards

let getDistinctCount cards =
    (List.map (fun c -> c.Id) cards |> List.distinct).Length
