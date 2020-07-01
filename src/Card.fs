module Card

type RawType =
    { id: int
      text: string }

[<CustomEquality; NoComparison>]
type Type =
    { Id: int
      Text: string }
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
        }

let getDistinctCount cards =
    (List.map (fun c -> c.Id) cards |> List.distinct).Length
