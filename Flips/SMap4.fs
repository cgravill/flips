namespace Flips.SliceMap

open System

type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Value : equality> (keys:Memory<struct ('Key1 * 'Key2 * 'Key3 * 'Key4)>, values:Memory<'Value>) =
    let k1Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key1>.Compare
    let k2Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key2>.Compare
    let k3Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key3>.Compare
    let k4Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key4>.Compare

    let compare ((ak1, ak2, ak3, ak4):struct ('Key1 * 'Key2 * 'Key3 * 'Key4), (bk1, bk2, bk3, bk4):struct ('Key1 * 'Key2 * 'Key3 * 'Key4)) =
        let c1 = k1Compare (ak1, bk1)
        let c2 = k2Compare (ak2, bk2)
        let c3 = k3Compare (ak3, bk3)
        let c4 = k4Compare (ak4, bk4)

        if c1 = 0 then
            if c2 = 0 then
                if c3 = 0 then
                    c4
                else
                    c3
            else
                c2
        else
            c1


    let keyFilterBuilder k1f k2f k3f k4f =
        let k1Filter = Utilities.SliceFilterBuilder k1Compare k1f
        let k2Filter = Utilities.SliceFilterBuilder k2Compare k2f
        let k3Filter = Utilities.SliceFilterBuilder k3Compare k3f
        let k4Filter = Utilities.SliceFilterBuilder k4Compare k4f
        let keyFilter struct (k1, k2, k3, k4) = k1Filter k1 && k2Filter k2 && k3Filter k3 && k4Filter k4
        keyFilter

    let keys = keys
    let values = values

    member _.Comparer = compare
    member _.Keys = keys
    member _.Values = values

    new(m:Map<_,_>) =
      let (keys, values) = m |> Map.toSeq |> Seq.map (fun ((k1, k2, k3, k4), v) -> struct (k1, k2, k3, k4), v ) |> SliceData.ofSeq
      SMap4 (keys, values)

    override this.ToString() =
        sprintf "SMap4 %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> as s -> SliceData.equals this.Comparer (this.Keys, this.Values) (s.Keys, s.Values)
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
    // 4D
    member this.Item
        with get (k1f, k2f, k3f, k4f) =
            let keyFilter = keyFilterBuilder k1f k2f k3f k4f
            let reKey = id
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap4

    // 3D
    member this.Item
        with get (k1, k2f, k3f, k4f) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f k3f k4f
            let reKey struct (k1, k2, k3, k4) = struct (k2, k3, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2, k3f, k4f) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) k3f k4f
            let reKey struct (k1, k2, k3, k4) = struct (k1, k3, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2f, k3, k4f) =
            let keyFilter = keyFilterBuilder k1f k2f (Equals k3) k4f
            let reKey struct (k1, k2, k3, k4) = struct (k1, k2, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    member this.Item
        with get (k1f, k2f, k3f, k4) =
            let keyFilter = keyFilterBuilder k1f k2f k3f (Equals k4) 
            let reKey struct (k1, k2, k3, k4) = struct (k1, k2, k3)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap3

    // 2D
    member this.Item
        with get (k1, k2, k3f, k4f) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) k3f k4f 
            let reKey struct (k1, k2, k3, k4) = struct (k3, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1, k2f, k3, k4f) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f (Equals k3) k4f 
            let reKey struct (k1, k2, k3, k4) = struct (k2, k4)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1, k2f, k3f, k4) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f k3f (Equals k4)  
            let reKey struct (k1, k2, k3, k4) = struct (k2, k3)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1f, k2, k3f, k4) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) k3f (Equals k4)  
            let reKey struct (k1, k2, k3, k4) = struct (k1, k3)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2

    member this.Item
        with get (k1f, k2f, k3, k4) =
            let keyFilter = keyFilterBuilder k1f k2f (Equals k3) (Equals k4)  
            let reKey struct (k1, k2, k3, k4) = struct (k1, k2)
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap2


    // 1D
    member this.Item
        with get (k1, k2, k3, k4f) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) (Equals k3) k4f  
            let reKey struct (k1, k2, k3, k4) = k4
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1, k2, k3f, k4) =
            let keyFilter = keyFilterBuilder (Equals k1) (Equals k2) k3f (Equals k4)  
            let reKey struct (k1, k2, k3, k4) = k3
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1, k2f, k3, k4) =
            let keyFilter = keyFilterBuilder (Equals k1) k2f (Equals k3) (Equals k4)  
            let reKey struct (k1, k2, k3, k4) = k2
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    member this.Item
        with get (k1f, k2, k3, k4) =
            let keyFilter = keyFilterBuilder k1f (Equals k2) (Equals k3) (Equals k4)  
            let reKey struct (k1, k2, k3, k4) = k1
            SliceData.filterByKey keyFilter reKey (this.Keys, this.Values)
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3, k4) =
            SliceData.getItem compare (struct (k1 ,k2, k3, k4)) (keys, values)

    // Operators
    static member inline (*) (coefficient, sm:SMap4<_,_,_,_,_>) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap4 (sm.Keys, newValues)

    static member inline (*) (sm:SMap4<_,_,_,_,_>, coefficient) =
        let newValues = SliceData.scale coefficient sm.Values
        SMap4 (sm.Keys, newValues)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        SliceData.hadamardProduct a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap4

    static member inline (.*) (sm4:SMap4<_,_,_,_,_>, sm3:SMap3<_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4) = struct (k2, k3, k4)
        SliceData.projectHadamardProduct keyMapper sm3.Comparer (sm4.Keys, sm4.Values) (sm3.Keys, sm3.Values)
        |> SMap4

    static member inline (.*) (sm3:SMap3<_,_,_,_>, sm4:SMap4<_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4) = struct (k1, k2, k3)
        SliceData.projectHadamardProduct keyMapper sm3.Comparer (sm4.Keys, sm4.Values) (sm3.Keys, sm3.Values)
        |> SMap4

    static member inline (.*) (sm4:SMap4<_,_,_,_,_>, sm2:SMap2<_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4) = struct (k3, k4)
        SliceData.projectHadamardProduct keyMapper sm2.Comparer (sm4.Keys, sm4.Values) (sm2.Keys, sm2.Values)
        |> SMap4

    static member inline (.*) (sm2:SMap2<_,_,_>, sm4:SMap4<_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4) = struct (k1, k2)
        SliceData.projectHadamardProduct keyMapper sm2.Comparer (sm4.Keys, sm4.Values) (sm2.Keys, sm2.Values)
        |> SMap4

    static member inline (.*) (sm4:SMap4<_,_,_,_,_>, sm:SMap<_,_>) =
        let keyMapper struct (k1, k2, k3, k4) = k4
        SliceData.projectHadamardProduct keyMapper sm.Comparer (sm4.Keys, sm4.Values) (sm.Keys, sm.Values)
        |> SMap4

    static member inline (.*) (sm:SMap<_,_>, sm4:SMap4<_,_,_,_,_>) =
        let keyMapper struct (k1, k2, k3, k4) = k1
        SliceData.projectHadamardProduct keyMapper sm.Comparer (sm4.Keys, sm4.Values) (sm.Keys, sm.Values)
        |> SMap4

    static member inline (+) (a:SMap4<_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        SliceData.add a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap4

    static member inline Sum (m:SMap4<_,_,_,_,_>) =
        SliceData.sum m.Values

    //static member inline Sum (m:SMap4<_,_,_,_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap4<_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap4 =

    let ofSeq (s:seq<('Key1 * 'Key2 * 'Key3 * 'Key4) * 'Value>) =
        s |> Seq.map (fun ((k1, k2, k3, k4), v) -> struct (k1, k2, k3, k4), v ) |> SliceData.ofSeq |> SMap4

    let toSeq (m:SMap4<_,_,_,_,_>) =
        SliceData.toSeq (m.Keys, m.Values)
        |> Seq.map (fun (struct (k1, k2, k3, k4), v) -> (k1, k2, k3, k4), v)

    let ofMap m =
        m |> Map.toSeq |> ofSeq

    let toMap (m:SMap4<_,_,_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> List.toSeq |> ofSeq

    let toList (m:SMap4<_,_,_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Array.toSeq |> ofSeq

    let toArray (m:SMap4<_,_,_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap4<_,_,_,_,_>) =
        SliceData.contains m.Comparer k m.Keys
