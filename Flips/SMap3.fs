namespace Flips.SliceMap

open System

type SMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Value : equality> (keys:Memory<struct ('Key1 * 'Key2 * 'Key3)>, values:Memory<'Value>) =
    let k1Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key1>.Compare
    let k2Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key2>.Compare
    let k3Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key3>.Compare

    let compare ((ak1, ak2, ak3):struct ('Key1 * 'Key2 * 'Key3), (bk1, bk2, bk3):struct ('Key1 * 'Key2 * 'Key3)) =
        let c1 = k1Compare (ak1, bk1)
        let c2 = k2Compare (ak2, bk2)
        let c3 = k3Compare (ak3, bk3)

        if c1 = 0 then
            if c2 = 0 then
                c3
            else
                c2
        else
            c1

    let keyFilterBuilder k1f k2f k3f =
        let k1Filter = Utilities.SliceFilterBuilder k1Compare k1f
        let k2Filter = Utilities.SliceFilterBuilder k2Compare k2f
        let k3Filter = Utilities.SliceFilterBuilder k3Compare k3f
        let keyFilter struct (k1, k2, k3) = k1Filter k1 && k2Filter k2 && k3Filter k3
        keyFilter

    let keys = keys
    let values = values

    member _.Comparer = compare
    member _.Keys = keys
    member _.Values = values

    new(m:Map<_,_>) =
      let (keys, values) = m |> Map.toSeq |> Seq.map (fun ((k1, k2, k3), v) -> struct (k1, k2, k3), v ) |> SliceData.ofSeq
      SMap3 (keys, values)

    override this.ToString() =
        sprintf "SMap3 %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap3<'Key1, 'Key2, 'Key3, 'Value> as s -> SliceData.equals this.Comparer (this.Keys, this.Values) (s.Keys, s.Values)
        | _ -> false

    override this.GetHashCode () =
        hash this.Values

    member this.ContainsKey k =
        SliceData.contains this.Comparer k this.Keys

    member this.AsMap =
        seq {
            for idx in 0 .. this.Keys.Length - 1 ->
                keys.Span.[idx], values.Span.[idx]
        }
        |> Map.ofSeq

    // Slices
    // 3D
    member this.Item
        with get (k1f, k2f, k3f) =
            let keyFilter = keyFilterBuilder k1f k2f k3f
            let reKey = id
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    // 2D
    member this.Item
        with get (k1, k2f, k3f) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f k3f
            let reKey struct (k1, k2, k3) = struct (k2, k3)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1f, k2, k3f) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) k3f
            let reKey struct (k1, k2, k3) = struct (k1, k3)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1f, k2f, k3) =
            let keyFilter = keyFilterBuilder k1f k2f (Equals k3)
            let reKey struct (k1, k2, k3) = struct (k1, k2)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    // 1D
    member this.Item
        with get (k1, k2, k3f) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) k3f
            let reKey struct (k1, k2, k3) = k3
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1, k2f, k3) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f (Equals k3)
            let reKey struct (k1, k2, k3) = k2
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1f, k2, k3) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) (Equals k3)
            let reKey struct (k1, k2, k3) = k1
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3) =
            SliceData.getItem compare (struct (k1 ,k2, k3)) (keys, values)

    // Operators
    static member inline (*) (coefficient, sm:SMap3<_,_,_,_>) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap3 (sm.Keys, newValues)

    static member inline (*) (sm:SMap3<_,_,_,_>, coefficient) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap3 (sm.Keys, newValues)

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap3<_,_,_,_>) =
        SliceData.hadamardProduct a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap3

    static member inline (.*) (sm3:SMap3<_,_,_,_>, sm2:SMap2<_,_,_>) =
        let keyMapper struct (k1, k2, k3) = struct (k2, k3)
        SliceData.projectHadamardProduct keyMapper sm2.Comparer (sm3.Keys, sm3.Values) (sm2.Keys, sm2.Values)
        |> SMap3

    static member inline (.*) (sm2:SMap2<_,_,_>, sm3:SMap3<_,_,_,_>) =
        let keyMapper struct (k1, k2, k3) = struct (k1, k2)
        SliceData.projectHadamardProduct keyMapper sm2.Comparer (sm3.Keys, sm3.Values) (sm2.Keys, sm2.Values)
        |> SMap3

    static member inline (.*) (sm3:SMap3<_,_,_,_>, sm:SMap<_,_>) =
        let keyMapper struct (k1, k2, k3) = k3
        SliceData.projectHadamardProduct keyMapper sm.Comparer (sm3.Keys, sm3.Values) (sm.Keys, sm.Values)
        |> SMap3

    static member inline (.*) (sm:SMap<_,_>, sm3:SMap3<_,_,_,_>) =
        let keyMapper struct (k1, k2, k3) = k1
        SliceData.projectHadamardProduct keyMapper sm.Comparer (sm3.Keys, sm3.Values) (sm.Keys, sm.Values)
        |> SMap3

    static member inline (+) (a:SMap3<_,_,_,_>, b:SMap3<_,_,_,_>) =
        SliceData.add a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap3

    static member inline Sum (m:SMap3<_,_,_,_>) =
        SliceData.sum m.Values

    //static member inline Sum (m:SMap3<_,_,_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap3<_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap3 =

    let ofSeq (s:seq<('Key1 * 'Key2 * 'Key3) * 'Value>) =
        s |> Seq.map (fun ((k1, k2, k3), v) -> struct (k1, k2, k3), v ) |> SliceData.ofSeq |> SMap3

    let toSeq (m:SMap3<_,_,_,_>) =
        SliceData.toSeq (m.Keys, m.Values)
        |> Seq.map (fun (struct (k1, k2, k3), v) -> (k1, k2, k3), v)

    let ofMap m =
        m |> Map.toSeq |> ofSeq

    let toMap (m:SMap3<_,_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> List.toSeq |> ofSeq

    let toList (m:SMap3<_,_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Array.toSeq |> ofSeq

    let toArray (m:SMap3<_,_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap3<_,_,_,_>) =
        SliceData.contains m.Comparer k m.Keys

