namespace Flips.SliceMap

open System

type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Value : equality> (keys:Memory<struct ('Key1 * 'Key2 * 'Key3 * 'Key4)>, values:Memory<'Value>) =
    let k1Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key1>
    let k2Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key2>
    let k3Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key3>
    let k4Compare = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key4>

    let compare ((ak1, ak2, ak3, ak4):struct ('Key1 * 'Key2 * 'Key3 * 'Key4), (bk1, bk2, bk3, bk4):struct ('Key1 * 'Key2 * 'Key3 * 'Key4)) =
        let c1 = k1Compare.Compare(ak1, bk1)
        let c2 = k2Compare.Compare(ak2, bk2)
        let c3 = k3Compare.Compare(ak3, bk3)
        let c4 = k4Compare.Compare(ak4, bk4)

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


    let keys = keys
    let values = values

    member _.Compare = compare
    member _.Keys = keys
    member _.Values = values

    new(m:seq<('Key1 * 'Key2 * 'Key3 * 'Key4) * 'Value>) =
        let keys =
            m 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> struct (k1, k2, k3, k4)) 
            |> Array.ofSeq
            |> fun x -> x.AsMemory()

        let values =
            m 
            |> Seq.map snd
            |> Array.ofSeq
            |> fun x -> x.AsMemory()
        SMap4 (keys, values)

    override this.ToString() =
        sprintf "SMap4 %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> as s ->
            if this.Keys.Length <> s.Keys.Length then
                false
            else
                let mutable idx = 0
                let mutable result = true

                while idx < this.Keys.Length && result do
                    let c = compare(this.Keys.Span.[idx], s.Keys.Span.[idx])
                    if c <> 0 || this.Values.Span.[idx] <> s.Values.Span.[idx] then
                        result <- false

                result
            
        | _ -> false

    override this.GetHashCode () =
        hash this.Values

    //member _.ContainsKey k =
        //Map.containsKey k this.Values

    //member this.AsMap =
    //    this.Values

    //// Filter Values
    //member private this.FilterValues k1f k2f k3f k4f =
    //    let k1Filter = SliceFilterBuilder k1f
    //    let k2Filter = SliceFilterBuilder k2f
    //    let k3Filter = SliceFilterBuilder k3f
    //    let k4Filter = SliceFilterBuilder k4f
        
    //    this.Values
    //    |> Map.filter (fun struct (k1, k2, k3, k4) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3 && k4Filter k4)
    //    |> Map.toSeq

    //// Slices
    //// 4D
    //member this.Item
    //    with get (k1f, k2f, k3f, k4f) =
    //        this.FilterValues k1f k2f k3f k4f |> Map.ofSeq |> SMap4

    //// 3D
    //member this.Item
    //    with get (k1, k2f, k3f, k4f) =
    //        this.FilterValues (Equals k1) k2f k3f k4f
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k2, k3, k4), v) 
    //        |> Map.ofSeq 
    //        |> SMap3

    //member this.Item
    //    with get (k1f, k2, k3f, k4f) =
    //        this.FilterValues k1f (Equals k2) k3f k4f
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k3, k4), v) 
    //        |> Map.ofSeq 
    //        |> SMap3

    //member this.Item
    //    with get (k1f, k2f, k3, k4f) =
    //        this.FilterValues k1f k2f (Equals k3) k4f
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k4), v) 
    //        |> Map.ofSeq 
    //        |> SMap3

    //member this.Item
    //    with get (k1f, k2f, k3f, k4) =
    //        this.FilterValues k1f k2f k3f (Equals k4) 
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k3), v) 
    //        |> Map.ofSeq 
    //        |> SMap3

    //// 2D
    //member this.Item
    //    with get (k1, k2, k3f, k4f) =
    //        this.FilterValues (Equals k1) (Equals k2) k3f k4f
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) 
    //        |> Map.ofSeq 
    //        |> SMap2

    //member this.Item
    //    with get (k1, k2f, k3, k4f) =
    //        this.FilterValues (Equals k1) k2f (Equals k3) k4f
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k2, k4), v) 
    //        |> Map.ofSeq 
    //        |> SMap2

    //member this.Item
    //    with get (k1, k2f, k3f, k4) =
    //        this.FilterValues (Equals k1) k2f k3f (Equals k4) 
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) 
    //        |> Map.ofSeq 
    //        |> SMap2

    //member this.Item
    //    with get (k1f, k2, k3f, k4) =
    //        this.FilterValues k1f (Equals k2) k3f (Equals k4) 
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k3), v) 
    //        |> Map.ofSeq 
    //        |> SMap2

    //member this.Item
    //    with get (k1f, k2f, k3, k4) =
    //        this.FilterValues k1f k2f (Equals k3) (Equals k4) 
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2), v) 
    //        |> Map.ofSeq 
    //        |> SMap2

    //// 1D
    //member this.Item
    //    with get (k1, k2, k3, k4f) =
    //        this.FilterValues (Equals k1) (Equals k2) (Equals k3) k4f
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> k4, v) 
    //        |> Map.ofSeq 
    //        |> SMap

    //member this.Item
    //    with get (k1, k2, k3f, k4) =
    //        this.FilterValues (Equals k1) (Equals k2) k3f (Equals k4) 
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> k3, v) 
    //        |> Map.ofSeq 
    //        |> SMap

    //member this.Item
    //    with get (k1, k2f, k3, k4) =
    //        this.FilterValues (Equals k1) k2f (Equals k3) (Equals k4) 
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> k2, v) 
    //        |> Map.ofSeq 
    //        |> SMap

    //member this.Item
    //    with get (k1f, k2, k3, k4) =
    //        this.FilterValues k1f (Equals k2) (Equals k3) (Equals k4) 
    //        |> Seq.map (fun ((k1, k2, k3, k4), v) -> k1, v) 
    //        |> Map.ofSeq 
    //        |> SMap

    //// 0D (aka GetItem)
    //member this.Item
    //    with get(k1, k2, k3, k4) =
    //        this.Values.[k1, k2, k3, k4] 

    //// Operators
    //static member inline (*) (lhs, rhs:SMap4<_,_,_,_,_>) =
    //    rhs.Values
    //    |> Map.map (fun k v -> lhs * v)
    //    |> SMap4

    //static member inline (*) (lhs:SMap4<_,_,_,_,_>, rhs) =
    //    lhs.Values
    //    |> Map.map (fun k v -> rhs * v)
    //    |> SMap4

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =

        let newKeys = Array.zeroCreate(Math.Min(a.Keys.Length, b.Keys.Length))
        let newValues = Array.zeroCreate(Math.Min(a.Values.Length, b.Values.Length))
        let mutable aIdx = 0
        let mutable bIdx = 0
        let mutable outIdx = 0

        while (aIdx < a.Values.Length && bIdx < b.Values.Length) do
            
            let c = a.Compare (a.Keys.Span.[aIdx], b.Keys.Span.[bIdx])
            if c = 0 then
                newKeys.[outIdx] <- a.Keys.Span.[aIdx]
                newValues.[outIdx] <- a.Values.Span.[aIdx] * b.Values.Span.[bIdx]
                aIdx <- aIdx + 1
                bIdx <- bIdx + 1
                outIdx <- outIdx + 1
            elif c < 0 then
                aIdx <- aIdx + 1
            else
                bIdx <- bIdx + 1

        SMap4(newKeys.AsMemory(0, outIdx), newValues.AsMemory(0, outIdx))


    //static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap3<_,_,_,_>) =
    //    a.Values
    //    |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k2, k3, k4))
    //    |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k2, k3, k4])
    //    |> SMap4

    //static member inline (.*) (b:SMap3<_,_,_,_>, a:SMap4<_,_,_,_,_>) =
    //    a.Values
    //    |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k1, k2, k3))
    //    |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k1, k2, k3])
    //    |> SMap4

    //static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap2<_,_,_>) =
    //    a.Values
    //    |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k3, k4))
    //    |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k3, k4])
    //    |> SMap4

    //static member inline (.*) (b:SMap2<_,_,_>, a:SMap4<_,_,_,_,_>) =
    //    a.Values
    //    |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k1, k2))
    //    |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k1, k2])
    //    |> SMap4

    //static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap<_,_>) =
    //    a.Values
    //    |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey k4)
    //    |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k4])
    //    |> SMap4

    //static member inline (.*) (b:SMap<_,_>, a:SMap4<_,_,_,_,_>) =
    //    a.Values
    //    |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey k1)
    //    |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k1])
    //    |> SMap4

    static member inline (+) (a:SMap4<_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        //match Map.count lhs.Values > Map.count rhs.Values with
        //| true ->  mergeAddition lhs.Values rhs.Values
        //| false -> mergeAddition rhs.Values lhs.Values
        //|> SMap4

        let newKeys = Array.zeroCreate(a.Keys.Length + b.Keys.Length)
        let newValues = Array.zeroCreate(a.Values.Length + b.Values.Length)

        let mutable aIdx = 0
        let mutable bIdx = 0
        let mutable outIdx = 0

        while (aIdx < a.Values.Length && bIdx < b.Values.Length) do
            
            let c = a.Compare (a.Keys.Span.[aIdx], b.Keys.Span.[bIdx])

            if c < 0 then
                newKeys.[outIdx] <- a.Keys.Span.[aIdx]
                newValues.[outIdx] <- a.Values.Span.[aIdx]
                aIdx <- aIdx + 1
                outIdx <- outIdx + 1
            elif c = 0 then
                newKeys.[outIdx] <- a.Keys.Span.[aIdx]
                newValues.[outIdx] <- a.Values.Span.[aIdx] + b.Values.Span.[bIdx]
                aIdx <- aIdx + 1
                bIdx <- bIdx + 1
                outIdx <- outIdx + 1
            else
                newKeys.[outIdx] <- b.Keys.Span.[bIdx]
                newValues.[outIdx] <- b.Values.Span.[bIdx]
                bIdx <- bIdx + 1
                outIdx <- outIdx + 1

        while aIdx < a.Values.Length do
            newValues.[outIdx] <- a.Values.Span.[aIdx]
            aIdx <- aIdx + 1
            outIdx <- outIdx + 1

        while bIdx < b.Values.Length do
            newValues.[outIdx] <- b.Values.Span.[bIdx]
            bIdx <- bIdx + 1
            outIdx <- outIdx + 1

        SMap4(newKeys.AsMemory(0, outIdx), newValues.AsMemory(0, outIdx))

    //static member inline Sum (m:SMap4<_,_,_,_,_>) =
    //    m.Values |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap4<_,_,_,_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap4<_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


//module SMap4 =

//    let ofMap m =
//        m |> SMap4

//    let toMap (m:SMap4<_,_,_,_,_>) =
//        m.Values

//    let ofList m =
//        m |> Map.ofList |> SMap4

//    let toList (m:SMap4<_,_,_,_,_>) =
//        m.Values |> Map.toList

//    let ofSeq m =
//        m |> Map.ofSeq |> SMap4

//    let toSeq (m:SMap4<_,_,_,_,_>) =
//        m.Values |> Map.toSeq

//    let ofArray m =
//        m |> Map.ofArray |> SMap4

//    let toArray (m:SMap4<_,_,_,_,_>) =
//        m.Values |> Map.toArray

//    let containsKey k (m:SMap4<_,_,_,_,_>) =
//        Map.containsKey k m.Values

//    let reKey f m =
//        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq
