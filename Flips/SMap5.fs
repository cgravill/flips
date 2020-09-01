namespace Flips.SliceMap

open System

type SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Key5 : comparison and 'Value : equality> (keys:Memory<struct ('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5)>, values:Memory<'Value>) =
    let k1Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key1>.Compare
    let k2Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key2>.Compare
    let k3Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key3>.Compare
    let k4Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key4>.Compare
    let k5Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key5>.Compare

    let compare ((ak1, ak2, ak3, ak4, ak5):struct ('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5), (bk1, bk2, bk3, bk4, bk5):struct ('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5)) =
        let c1 = k1Compare (ak1, bk1)
        let c2 = k2Compare (ak2, bk2)
        let c3 = k3Compare (ak3, bk3)
        let c4 = k4Compare (ak4, bk4)
        let c5 = k5Compare (ak5, bk5)

        if c1 = 0 then
            if c2 = 0 then
                if c3 = 0 then
                    if c4 = 0 then
                      c5
                    else
                      c4
                else
                    c3
            else
                c2
        else
            c1


    let keyFilterBuilder k1f k2f k3f k4f k5f =
        let k1Filter = Utilities.SliceFilterBuilder k1Compare k1f
        let k2Filter = Utilities.SliceFilterBuilder k2Compare k2f
        let k3Filter = Utilities.SliceFilterBuilder k3Compare k3f
        let k4Filter = Utilities.SliceFilterBuilder k4Compare k4f
        let k5Filter = Utilities.SliceFilterBuilder k5Compare k5f
        let keyFilter struct (k1, k2, k3, k4, k5) = k1Filter k1 && k2Filter k2 && k3Filter k3 && k4Filter k4 && k5Filter k5
        keyFilter

    let keys = keys
    let values = values

    member _.Comparer = compare
    member _.Keys = keys
    member _.Values = values

    new(m:Map<_,_>) =
      let (keys, values) = m |> Map.toSeq |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> struct (k1, k2, k3, k4, k5), v ) |> SliceData.ofSeq
      SMap5 (keys, values)

    override this.ToString() =
        sprintf "SMap5 %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> as s -> SliceData.equals this.Comparer (this.Keys, this.Values) (s.Keys, s.Values)
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
    // 5D
    member this.Item 
        with get (k1f, k2f, k3f, k4f, k5f) =
            let keyFilter = keyFilterBuilder k1f k2f k3f k4f k5f
            let reKey = id
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap5

    // 4D
    member this.Item
        with get (k1, k2f, k3f, k4f, k5f) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f k3f k4f k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k2, k3, k4, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap4


    member this.Item
        with get (k1f, k2, k3f, k4f, k5f) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) k3f k4f k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k3, k4, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap4

    member this.Item
        with get (k1f, k2f, k3, k4f, k5f) =
            let keyFilter = keyFilterBuilder k1f k2f (Equals k3) k4f k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k2, k4, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap4

    member this.Item
        with get (k1f, k2f, k3f, k4, k5f) =
            let keyFilter = keyFilterBuilder k1f k2f k3f (Equals k4) k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k2, k3, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap4

    member this.Item
        with get (k1f, k2f, k3f, k4f, k5) =
            let keyFilter = keyFilterBuilder k1f k2f k3f k4f (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k2, k3, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap4


    // 3D
    member this.Item
        with get (k1, k2, k3f, k4f, k5f) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) k3f k4f k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k3, k4, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1, k2f, k3, k4f, k5f) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f (Equals k3) k4f k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k2, k4, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3


    member this.Item
        with get (k1, k2f, k3f, k4, k5f) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f k3f (Equals k4) k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k2, k3, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1, k2f, k3f, k4f, k5) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f k3f k4f (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k2, k3, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2, k3, k4f, k5f) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) (Equals k3) k4f k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k4, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2, k3f, k4, k5f) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) k3f (Equals k4) k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k3, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2, k3f, k4f, k5) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) k3f k4f (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k3, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2f, k3, k4, k5f) =
            let keyFilter = keyFilterBuilder k1f k2f (Equals k3) (Equals k4) k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k2, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2f, k3, k4f, k5) =
            let keyFilter = keyFilterBuilder k1f k2f (Equals k3) k4f (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k2, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2f, k3f, k4, k5) =
            let keyFilter = keyFilterBuilder k1f k2f k3f (Equals k4) (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k2, k3)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3


    // 2D
    member this.Item
        with get (k1, k2, k3, k4f, k5f) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) (Equals k3) k4f k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k4, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1, k2, k3f, k4, k5f) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) k3f (Equals k4) k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k3, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1, k2, k3f, k4f, k5) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) k3f k4f (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k3, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1f, k2, k3, k4, k5f) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) (Equals k3) (Equals k4) k5f
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k5)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1f, k2, k3, k4f, k5) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) (Equals k3) k4f (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1f, k2f, k3, k4, k5) =
            let keyFilter = keyFilterBuilder k1f k2f (Equals k3) (Equals k4) (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = struct (k1, k2)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    // 1D
    member this.Item
        with get (k1, k2, k3, k4, k5f) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) (Equals k3) (Equals k4) k5f
            let reKey struct (k1, k2, k3, k4, k5) = k5
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1, k2, k3, k4f, k5) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) (Equals k3) k4f (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = k4
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1, k2, k3f, k4, k5) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) k3f (Equals k4) (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = k3
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1, k2f, k3, k4, k5) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f (Equals k3) (Equals k4) (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = k2
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1f, k2, k3, k4, k5) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) (Equals k3) (Equals k4) (Equals k5)
            let reKey struct (k1, k2, k3, k4, k5) = k1
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3, k4, k5) =
            SliceData.getItem compare (struct (k1 ,k2, k3, k4, k5)) (keys, values)

    // Operators
    static member inline (*) (coefficient, sm:SMap5<_,_,_,_,_,_>) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap5 (sm.Keys, newValues)

    static member inline (*) (sm:SMap5<_,_,_,_,_,_>, coefficient) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap5 (sm.Keys, newValues)

    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap5<_,_,_,_,_,_>) =
        SliceData.hadamardProduct a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap5

    static member inline (.*) (sm5:SMap5<_,_,_,_,_,_>, sm4:SMap4<_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = struct (k2, k3, k4, k5)
        SliceData.projectHadamardProduct keyMapper sm4.Comparer (sm5.Keys, sm5.Values) (sm4.Keys, sm4.Values)
        |> SMap5

    static member inline (.*) (sm4:SMap4<_,_,_,_,_>, sm5:SMap5<_,_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = struct (k1, k2, k3, k4)
        SliceData.projectHadamardProduct keyMapper sm4.Comparer (sm5.Keys, sm5.Values) (sm4.Keys, sm4.Values)
        |> SMap5

    static member inline (.*) (sm5:SMap5<_,_,_,_,_,_>, sm3:SMap3<_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = struct (k3, k4, k5)
        SliceData.projectHadamardProduct keyMapper sm3.Comparer (sm5.Keys, sm5.Values) (sm3.Keys, sm3.Values)
        |> SMap5

    static member inline (.*) (sm3:SMap3<_,_,_,_>, sm5:SMap5<_,_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = struct (k1, k2, k3)
        SliceData.projectHadamardProduct keyMapper sm3.Comparer (sm5.Keys, sm5.Values) (sm3.Keys, sm3.Values)
        |> SMap5

    static member inline (.*) (sm5:SMap5<_,_,_,_,_,_>, sm2:SMap2<_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = struct (k4, k5)
        SliceData.projectHadamardProduct keyMapper sm2.Comparer (sm5.Keys, sm5.Values) (sm2.Keys, sm2.Values)
        |> SMap5

    static member inline (.*) (sm2:SMap2<_,_,_>, sm5:SMap5<_,_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = struct (k1, k2)
        SliceData.projectHadamardProduct keyMapper sm2.Comparer (sm5.Keys, sm5.Values) (sm2.Keys, sm2.Values)
        |> SMap5

    static member inline (.*) (sm5:SMap5<_,_,_,_,_,_>, sm:SMap<_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = k5
        SliceData.projectHadamardProduct keyMapper sm.Comparer (sm5.Keys, sm5.Values) (sm.Keys, sm.Values)
        |> SMap5

    static member inline (.*) (sm:SMap<_,_>, sm5:SMap5<_,_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4, k5) = k1
        SliceData.projectHadamardProduct keyMapper sm.Comparer (sm5.Keys, sm5.Values) (sm.Keys, sm.Values)
        |> SMap5


    static member inline (+) (a:SMap5<_,_,_,_,_,_>, b:SMap5<_,_,_,_,_,_>) =
        SliceData.add a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap5

    static member inline Sum (m:SMap5<_,_,_,_,_,_>) =
        SliceData.sum m.Values

//    static member inline Sum (m:SMap5<_,_,_,_,_,Flips.Types.Decision>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap5<_,_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap5 =

    let ofSeq (s:seq<('Key1 * 'Key2 * 'Key3 * 'Key4 *'Key5) * 'Value>) =
        s |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> struct (k1, k2, k3, k4, k5), v ) |> SliceData.ofSeq |> SMap5

    let toSeq (m:SMap5<_,_,_,_,_,_>) =
        SliceData.toSeq (m.Keys, m.Values)
        |> Seq.map (fun (struct (k1, k2, k3, k4, k5), v) -> (k1, k2, k3, k4, k5), v)

    let ofMap m =
        m |> Map.toSeq |> ofSeq

    let toMap (m:SMap5<_,_,_,_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> List.toSeq |> ofSeq

    let toList (m:SMap5<_,_,_,_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Array.toSeq |> ofSeq

    let toArray (m:SMap5<_,_,_,_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap5<_,_,_,_,_,_>) =
        SliceData.contains m.Comparer k m.Keys