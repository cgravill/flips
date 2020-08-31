namespace Flips.SliceMap

open System

type SMap<'Key, 'Value when 'Key : comparison and 'Value : equality> (keys:Memory<'Key>, values:Memory<'Value>) =
    let kComparer = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key>.Compare

    let keys = keys
    let values = values

    member _.Comparer = kComparer
    member _.Keys = keys
    member _.Values = values

    override this.ToString() =
        sprintf "SMap %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap<'Key, 'Value> as s -> SliceData.equals this.Comparer (this.Keys, this.Values) (s.Keys, s.Values)
        | _ -> false

    override this.GetHashCode () =
        hash this.Values

    member this.ContainsKey k =
        SliceData.contains this.Comparer k this.Keys

    member this.AsMap =
        seq {
            for idx in 0 .. this.Keys.Length ->
                keys.Span.[idx], values.Span.[idx]
        }
        |> Map.ofSeq

    // Slices
    // 1D
    member this.Item
        with get (k1f) =
            let filter = Utilities.SliceFilterBuilder this.Comparer k1f
            SliceData.filterByKey filter id (this.Keys, this.Values)
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k) =
            SliceData.getItem this.Comparer k (this.Keys, this.Values)

    // Operators
    static member inline (*) (coefficient, sm:SMap<_,_>) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap (sm.Keys, newValues)

    static member inline (*) (sm:SMap<_,_>, coefficient) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap (sm.Keys, newValues)

    static member inline (.*) (a:SMap<_,_>, b:SMap<_,_>) =
        SliceData.hadamardProduct a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap

    static member inline (+) (a:SMap<_,_>, b:SMap<_,_>) =
        SliceData.add a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap

    static member inline Sum (m:SMap<_,_>) =
        SliceData.sum m.Values

    //static member inline Sum (m:SMap<_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap<_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap =

    let ofSeq (s:seq<'Key * 'Value>) =
        s |> SliceData.ofSeq |> SMap

    let toSeq (m:SMap<_,_>) =
        SliceData.toSeq (m.Keys, m.Values)

    let ofMap m =
        m |> SMap

    let toMap (m:SMap<_,_>) =
        SliceData.toMap (m.Keys, m.Values)

    let ofList m =
        m |> ofSeq

    let toList (m:SMap<_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> SMap

    let toArray (m:SMap<_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap<_,_>) =
        SliceData.contains m.Comparer k m.Keys