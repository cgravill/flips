namespace Flips.SliceMap

open System

type SMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Value : equality> (keys:Memory<struct ('Key1 * 'Key2)>, values:Memory<'Value>) =
    let k1Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key1>.Compare
    let k2Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key2>.Compare

    let compare ((ak1, ak2):struct ('Key1 * 'Key2), (bk1, bk2):struct ('Key1 * 'Key2)) =
        let c1 = k1Compare (ak1, bk1)
        let c2 = k2Compare (ak2, bk2)

        if c1 = 0 then
            c2
        else
            c1

    let keyFilterBuilder k1f k2f =
        let k1Filter = Utilities.SliceFilterBuilder k1Compare k1f
        let k2Filter = Utilities.SliceFilterBuilder k2Compare k2f
        let keyFilter struct (k1, k2) = k1Filter k1 && k2Filter k2
        keyFilter

    let keys = keys
    let values = values

    member _.Comparer = compare
    member _.Keys = keys
    member _.Values = values

    new(m:Map<_,_>) =
      let (keys, values) = m  |> Map.toSeq |> Seq.map (fun ((k1, k2), v) -> struct (k1, k2), v ) |> SliceData.ofSeq
      SMap2 (keys, values)

    override this.ToString() =
        sprintf "SMap2 %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap2<'Key1, 'Key2, 'Value> as s -> SliceData.equals this.Comparer (this.Keys, this.Values) (s.Keys, s.Values)
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
    // 2D
    member this.Item
        with get (k1f, k2f) =
            let keyFilter = keyFilterBuilder k1f k2f
            let reKey = id
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    // 1D
    member this.Item
        with get (k1, k2f) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f
            let reKey struct (k1, k2) = k2
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1f, k2) =
            let keyFilter = keyFilterBuilder k1f (Equals k2)
            let reKey struct (k1, k2) = k2
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2) =
            SliceData.getItem this.Comparer (k1, k2) (this.Keys, this.Values)

    // Operators
    static member inline (*) (coefficient, sm:SMap2<_,_,_>) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap2 (sm.Keys, newValues)

    static member inline (*) (sm:SMap2<_,_,_>, coefficient) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap2 (sm.Keys, newValues)

    static member inline (.*) (a:SMap2<_,_,_>, b:SMap2<_,_,_>) =
        SliceData.hadamardProduct a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap2

    static member inline (.*) (sm2:SMap2<_,_,_>, sm:SMap<_,_>) =
        let comparer (struct (k1, k2), bKey) = FSharp.Core.LanguagePrimitives.FastGenericComparer<_>.Compare (k2, bKey)
        SliceData.hadamardProduct comparer (sm2.Keys, sm2.Values) (sm.Keys, sm.Values)
        |> SMap2

    static member inline (.*) (sm:SMap<_,_>, sm2:SMap2<_,_,_>) =
        let comparer (struct (k1, k2), bKey) = FSharp.Core.LanguagePrimitives.FastGenericComparer<_>.Compare (k1, bKey)
        SliceData.hadamardProduct comparer (sm2.Keys, sm2.Values) (sm.Keys, sm.Values)
        |> SMap2

    static member inline (+) (a:SMap2<_,_,_>, b:SMap2<_,_,_>) =
        SliceData.add a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap2

    static member inline Sum (m:SMap2<_,_,_>) =
        SliceData.sum m.Values

    //static member inline Sum (m:SMap2<_,_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap2<_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap2 =

    let ofSeq (s:seq<('Key1 * 'Key2) * 'Value>) =
        s |> Seq.map (fun ((k1, k2), v) -> struct (k1, k2), v ) |> SliceData.ofSeq |> SMap2

    let toSeq (m:SMap2<_,_,_>) =
        SliceData.toSeq (m.Keys, m.Values)
        |> Seq.map (fun (struct (k1, k2), v) -> (k1, k2), v)

    let ofMap m =
        m |> Map.toSeq |> ofSeq

    let toMap (m:SMap2<_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> List.toSeq |> ofSeq

    let toList (m:SMap2<_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Array.toSeq |> ofSeq

    let toArray (m:SMap2<_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap2<_,_,_>) =
        SliceData.contains m.Comparer k m.Keys