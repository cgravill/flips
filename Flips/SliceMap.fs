namespace Flips.SliceMap

open System
open System.Collections.Generic


type SliceType<'a when 'a : comparison> =
    | All
    | Equals of 'a
    | GreaterThan of 'a
    | GreaterOrEqual of 'a
    | LessThan of 'a
    | LessOrEqual of 'a
    | Between of 'a * 'a
    | In of Set<'a>
    | NotIn of Set<'a>
    | Where of ('a -> bool)

[<AutoOpen>]
module Utilities =

    // Declared here so it can be used by any of the MapXD types
    let inline internal getKeyCheck lb ub =
        match lb, ub with
        | Some lb, Some ub -> fun k1 -> k1 >= lb && k1 <= ub
        | Some lb, None -> fun k1 -> k1 >= lb
        | None, Some ub -> fun k1 -> k1 <= ub
        | None, None -> fun _ -> true


    let inline internal mergeAddition (lhs:Map<_,_>) (rhs:Map<_,_>) =
        /// The assumption is that the LHS Map has more entries than the RHS Map
        let newRhsValues = rhs |> Map.filter (fun k _ -> not (lhs.ContainsKey k)) |> Map.toSeq

        lhs
        |> Map.map (fun k lhsV -> match Map.tryFind k rhs with 
                                  | Some rhsV -> lhsV + rhsV 
                                  | None -> lhsV)
        |> fun newLhs -> Seq.fold (fun m (k, v) -> Map.add k v m) newLhs newRhsValues


    let internal SliceFilterBuilder<'a when 'a : comparison> (comparer:IComparer<_>) (f:SliceType<'a>) =
        match f with
        | All -> fun _ -> true
        | Equals x -> fun k -> comparer.Compare(x, k) = 0
        | GreaterThan x -> fun k -> comparer.Compare(x, k) > 0
        | GreaterOrEqual x -> fun k -> comparer.Compare(x, k) >= 0
        | LessThan x -> fun k -> comparer.Compare(x, k) < 0
        | LessOrEqual x -> fun k -> comparer.Compare(x, k) <= 0
        | Between (lowerBound, upperBound) -> fun k -> comparer.Compare(lowerBound, k) >= 0 && comparer.Compare(upperBound, k) <= 0
        | In set -> fun k -> Set.contains k set
        | NotIn set -> fun k -> not (Set.contains k set)
        | Where f -> f

    let inline sum< ^a, ^b when ^a: (static member Sum: ^a -> ^b)> (k1: ^a) = 
        ((^a) : (static member Sum: ^a -> ^b) k1)

    let inline sumAll< ^a, ^b when ^a: (static member Sum: ^a -> ^b) 
                              and ^a: (static member (+): ^a * ^a -> ^a)
                              and ^a: (static member Zero: ^a)> (k1: ^a seq) = 
        let r = Seq.sum k1
        ((^a) : (static member Sum: ^a -> ^b) r)

module internal Memory =

    let findIndexOf (comparer:IComparer<_>) startingLowerBound x (values:Memory<_>) =
        let mutable lowerBound = startingLowerBound
        let mutable upperBound = values.Length - 1
        let mutable idx = (lowerBound + upperBound) / 2

        while lowerBound <= upperBound do
            let x = comparer.Compare(values.Span.[idx], x)
            if x <= 0 then
                lowerBound <- idx + 1
                idx <- (lowerBound + upperBound) / 2
            else
                upperBound <- idx - 1
                idx <- (lowerBound + upperBound) / 2

        idx

    let contains (comparer:IComparer<_>) x (values:Memory<_>) =
        let mutable idx = 0
        let mutable doesContain = false

        while (idx < values.Length && not doesContain) do
            let c = comparer.Compare(values.Span.[idx], x)
            if c = 0 then
                doesContain <- true
            idx <- idx + 1

        doesContain

    let getItem (comparer:IComparer<_>) k (keys:Memory<_>, values:Memory<_>) =
        let idx = findIndexOf comparer 0 k keys

        if keys.Span.[idx] = k then
            values.Span.[idx]
        else
            invalidArg "Key" "Key not in SliceMap"

    let filterByKey f (keys:Memory<_>, values:Memory<_>) =
        let newValues = Array.zeroCreate(values.Length)
        let newKeys = Array.zeroCreate(keys.Length)

        let mutable idx = 0
        let mutable outIdx = 0

        while idx < keys.Length do
            if f keys.Span.[idx] then
                newKeys.[outIdx] <- keys.Span.[idx]
                newValues.[outIdx] <- values.Span.[idx]
                outIdx <- outIdx + 1
            
            idx <- idx + 1

        newKeys.AsMemory(0, outIdx), newValues.AsMemory(0, outIdx)

    let scale coefficient (values:Memory<_>) =
        let newValues = Array.zeroCreate(values.Length)
        let mutable idx = 0

        while idx < values.Length do
            newValues.[idx] <- values.Span.[idx] * coefficient

        newValues.AsMemory()


    let hadamardProduct (comparer:IComparer<_>) (aKeys:Memory<_>, aValues:Memory<_>) (bKeys:Memory<_>, bValues:Memory<_>) =
            let newKeys = Array.zeroCreate(Math.Min(aKeys.Length, bKeys.Length))
            let newValues = Array.zeroCreate(Math.Min(aValues.Length, bValues.Length))
            let mutable aIdx = 0
            let mutable bIdx = 0
            let mutable outIdx = 0

            while (aIdx < aKeys.Length && bIdx < bKeys.Length) do
        
                let c = comparer.Compare (aKeys.Span.[aIdx], bKeys.Span.[bIdx])
                if c = 0 then
                    newKeys.[outIdx] <- aKeys.Span.[aIdx]
                    newValues.[outIdx] <- aValues.Span.[aIdx] * bValues.Span.[bIdx]
                    aIdx <- aIdx + 1
                    bIdx <- bIdx + 1
                    outIdx <- outIdx + 1
                elif c < 0 then
                    aIdx <- aIdx + 1
                else
                    bIdx <- bIdx + 1

            newKeys.AsMemory(0, outIdx), newValues.AsMemory(0, outIdx)

    let add (comparer:IComparer<_>) (aKeys:Memory<_>, aValues:Memory<_>) (bKeys:Memory<_>, bValues:Memory<_>) =
        let newKeys = Array.zeroCreate(aKeys.Length + bKeys.Length)
        let newValues = Array.zeroCreate(aValues.Length + bValues.Length)

        let mutable aIdx = 0
        let mutable bIdx = 0
        let mutable outIdx = 0

        while (aIdx < aValues.Length && bIdx < bValues.Length) do
        
            let c = comparer.Compare(aValues.Span.[aIdx], bValues.Span.[bIdx])

            if c < 0 then
                newKeys.[outIdx] <- aKeys.Span.[aIdx]
                newValues.[outIdx] <- aValues.Span.[aIdx]
                aIdx <- aIdx + 1
                outIdx <- outIdx + 1
            elif c = 0 then
                newKeys.[outIdx] <- aKeys.Span.[aIdx]
                newValues.[outIdx] <- aValues.Span.[aIdx]
                aIdx <- aIdx + 1
                bIdx <- bIdx + 1
                outIdx <- outIdx + 1
            else
                newKeys.[outIdx] <- bKeys.Span.[bIdx]
                newValues.[outIdx] <- bValues.Span.[bIdx]
                bIdx <- bIdx + 1
                outIdx <- outIdx + 1

        while aIdx < aValues.Length do
            newKeys.[outIdx] <- aKeys.Span.[aIdx]
            newValues.[outIdx] <- aValues.Span.[aIdx]
            aIdx <- aIdx + 1
            outIdx <- outIdx + 1

        while bIdx < bValues.Length do
            newKeys.[outIdx] <- bKeys.Span.[bIdx]
            newValues.[outIdx] <- bValues.Span.[bIdx]
            bIdx <- bIdx + 1
            outIdx <- outIdx + 1

        newKeys.AsMemory(0, outIdx), newValues.AsMemory(0, outIdx)

    let inline sum (values:Memory<_>) =
        let mutable idx = 0
        let mutable acc = FSharp.Core.LanguagePrimitives.GenericZero

        while idx < values.Length do
            acc <- acc + values.Span.[idx]

        acc



type SMap<'Key, 'Value when 'Key : comparison and 'Value : equality> (keys:Memory<'Key>, values:Memory<'Value>) =
    let kComparer = FSharp.Core.LanguagePrimitives.FastGenericComparer<'Key>

    let keys = keys
    let values = values

    member _.Comparer = kComparer
    member _.Keys = keys
    member _.Values = values

    new(m:seq<'Key * 'Value>) =
        let keys =
            m 
            |> Seq.map fst
            |> Array.ofSeq
            |> fun x -> x.AsMemory()

        let values =
            m 
            |> Seq.map snd
            |> Array.ofSeq
            |> fun x -> x.AsMemory()
        SMap (keys, values)

    override this.ToString() =
        sprintf "SMap %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap<'Key, 'Value> as s -> this.Values = s.Values
        | _ -> false

    override this.GetHashCode () =
        hash this.Values

    member this.ContainsKey k =
        Memory.contains this.Comparer k this.Keys

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
            Memory.filterByKey filter (this.Keys, this.Values)
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k) =
            Memory.getItem this.Comparer k (this.Keys, this.Values)

    // Operators
    static member inline (*) (coefficient, sm:SMap<_,_>) =
        let newValues = Memory.scale coefficient sm.Values
        SMap (sm.Keys, newValues)

    static member inline (*) (sm:SMap<_,_>, coefficient) =
        let newValues = Memory.scale coefficient sm.Values
        SMap (sm.Keys, newValues)

    static member inline (.*) (a:SMap<_,_>, b:SMap<_,_>) =
        Memory.hadamardProduct a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap

    static member inline (+) (a:SMap<_,_>, b:SMap<_,_>) =
        Memory.add a.Comparer (a.Keys, a.Values) (b.Keys, b.Values)
        |> SMap

    static member inline Sum (m:SMap<_,_>) =
        Memory.sum m.Values

    //static member inline Sum (m:SMap<_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap<_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap =

    let ofMap m =
        m |> SMap

    let toMap (m:SMap<_,_>) =
        m.Values

    let ofList m =
        m |> Map.ofList |> SMap

    let toList (m:SMap<_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> SMap

    let toSeq (m:SMap<_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> SMap

    let toArray (m:SMap<_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:SMap<_,_>) =
        Map.containsKey k m.Values


type SMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Value : equality> (m:Map<('Key1 * 'Key2),'Value>) =

    member this.Values = m

    override this.ToString () = 
        sprintf "SMap2 %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap2<'Key1, 'Key2, 'Value > as s -> this.Values = s.Values
        | _ -> false

    override this.GetHashCode () =
        hash this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.AsMap =
        this.Values

    // Filter Values
    member private this.FilterValues k1f k2f =
        let k1Filter = SliceFilterBuilder k1f
        let k2Filter = SliceFilterBuilder k2f
        
        this.Values
        |> Map.filter (fun (k1, k2) _ -> k1Filter k1 && k2Filter k2)
        |> Map.toSeq

    // Slices
    // 2D
    member this.Item
        with get (k1f, k2f) =
            this.FilterValues k1f k2f |> Map.ofSeq |> SMap2

    // 1D
    member this.Item
        with get (k1, k2f) =
            this.FilterValues (Equals k1) k2f 
            |> Seq.map (fun ((_, k2), v) -> k2, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1f, k2) =
            this.FilterValues k1f (Equals k2)
            |> Seq.map (fun ((k1, _), v) -> k1, v)
            |> Map.ofSeq
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2) =
            this.Values.[(k1, k2)]

    // Operators
    static member inline (*) (lhs, rhs:SMap2<_,_,_>) =
        rhs.Values
        |> Map.map (fun _ v -> v * lhs)
        |> SMap2

    static member inline (*) (lhs:SMap2<_,_,_>, rhs) =
        lhs.Values
        |> Map.map (fun _ v -> v * rhs)
        |> SMap2

    static member inline (.*) (lhs:SMap2<_,_,_>, rhs:SMap2<_,_,_>) =
        let rhs = rhs.Values
        lhs.Values
        |> Map.filter (fun (k1, k2) _ -> rhs.ContainsKey (k1, k2))
        |> Map.map (fun (k1, k2) v -> v * rhs.[(k1, k2)])
        |> SMap2

    static member inline (.*) (lhs:SMap2<_,_,_>, rhs:SMap<_,_>) =
        let rhs = rhs.Values
        lhs.Values
        |> Map.filter (fun (k1, k2) _ -> rhs.ContainsKey k2)
        |> Map.map (fun (k1, k2) v -> v * rhs.[k2])
        |> SMap2

    static member inline (.*) (lhs:SMap<_,_>, rhs:SMap2<_,_,_>) =
        let rlhs = lhs.Values
        rhs.Values
        |> Map.filter (fun (k1, k2) _ -> lhs.ContainsKey k1)
        |> Map.map (fun (k1, k2) v -> v * lhs.[k1])
        |> SMap2

    static member inline (+) (lhs:SMap2<_,_,_>, rhs:SMap2<_,_,_>) =
        match Map.count lhs.Values > Map.count rhs.Values with
        | true ->  mergeAddition lhs.Values rhs.Values
        | false -> mergeAddition rhs.Values lhs.Values
        |> SMap2

    static member inline Sum (m:SMap2<_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd

    static member inline Sum (m:SMap2<_,_,Flips.Types.Decision>) =
        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    static member inline Sum (m:SMap2<_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap2 =

    let ofMap m =
        m |> SMap2

    let toMap (m:SMap2<_,_,_>) =
        m.Values

    let ofList m =
        m |> Map.ofList |> SMap2

    let toList (m:SMap2<_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> SMap2

    let toSeq (m:SMap2<_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> SMap2

    let toArray (m:SMap2<_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:SMap2<_,_,_>) =
        Map.containsKey k m.Values

    let inline reKey f m =
        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq


type SMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Value : equality> (m:Map<('Key1 * 'Key2 * 'Key3),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "SMap3 %O" this.Values

    override this.Equals(obj) =
        match obj with
        | :? SMap3<'Key1, 'Key2, 'Key3, 'Value> as s -> this.Values = s.Values
        | _ -> false

    override this.GetHashCode () =
        hash this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.AsMap =
        this.Values

    // Filter Values
    member private this.FilterValues k1f k2f k3f =
        let k1Filter = SliceFilterBuilder k1f
        let k2Filter = SliceFilterBuilder k2f
        let k3Filter = SliceFilterBuilder k3f
        
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3)
        |> Map.toSeq

    // Slices
    // 3D
    member this.Item
        with get (k1f, k2f, k3f) =
            this.FilterValues k1f k2f k3f |> Map.ofSeq |> SMap3

    // 2D
    member this.Item
        with get (k1, k2f, k3f) =
            this.FilterValues (Equals k1) k2f k3f
            |> Seq.map (fun ((k1, k2, k3), v) -> (k2, k3), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1f, k2, k3f) =
            this.FilterValues k1f (Equals k2) k3f
            |> Seq.map (fun ((k1, k2, k3), v) -> (k1, k3), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1f, k2f, k3) =
            this.FilterValues k1f k2f (Equals k3)
            |> Seq.map (fun ((k1, k2, k3), v) -> (k1, k2), v) 
            |> Map.ofSeq 
            |> SMap2

    // 1D
    member this.Item
        with get (k1, k2, k3f) =
            this.FilterValues (Equals k1) (Equals k2) k3f
            |> Seq.map (fun ((k1, k2, k3), v) -> k3, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1, k2f, k3) =
            this.FilterValues (Equals k1) k2f (Equals k3)
            |> Seq.map (fun ((k1, k2, k3), v) -> k2, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1f, k2, k3) =
            this.FilterValues k1f (Equals k2) (Equals k3)
            |> Seq.map (fun ((k1, k2, k3), v) -> k1, v) 
            |> Map.ofSeq 
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3) =
            this.Values.[(k1, k2, k3)] 

    // Operators
    static member inline (*) (lhs, rhs:SMap3<_,_,_,_>) =
        rhs.Values
        |> Map.map (fun k v -> lhs * v)
        |> SMap3

    static member inline (*) (lhs:SMap3<_,_,_,_>, rhs) =
        lhs.Values
        |> Map.map (fun k v -> rhs * v)
        |> SMap3

    static member inline (.*) (lhs:SMap3<_,_,_,_>, rhs:SMap3<_,_,_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun (k1, k2, k3) v -> v * rhs.[k1, k2, k3])
        |> SMap3

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap2<_,_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey (k2, k3))
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k2, k3])
        |> SMap3

    static member inline (.*) (b:SMap2<_,_,_>, a:SMap3<_,_,_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey (k1, k2))
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k1, k2])
        |> SMap3

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap<_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey k3)
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k3])
        |> SMap3

    static member inline (.*) (b:SMap<_,_>, a:SMap3<_,_,_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey k1)
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k1])
        |> SMap3

    static member inline (+) (lhs:SMap3<_,_,_,_>, rhs:SMap3<_,_,_,_>) =
        match Map.count lhs.Values > Map.count rhs.Values with
        | true ->  mergeAddition lhs.Values rhs.Values
        | false -> mergeAddition rhs.Values lhs.Values
        |> SMap3

    static member inline Sum (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd

    static member inline Sum (m:SMap3<_,_,_,Flips.Types.Decision>) =
        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    static member inline Sum (m:SMap3<_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap3 =

    let ofMap m =
        m |> SMap3

    let toMap (m:SMap3<_,_,_,_>) =
        m.Values

    let ofList m =
        m |> Map.ofList |> SMap3

    let toList (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> SMap3

    let toSeq (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> SMap3

    let toArray (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:SMap3<_,_,_,_>) =
        Map.containsKey k m.Values

    let inline reKey f m =
        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq


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

    member _.ContainsKey k =
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


//type SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Key5 : comparison and 'Value : equality> (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5),'Value>) =

//    member this.Values = m

//    override this.ToString() =
//        sprintf "SMap5 %O" this.Values

//    override this.Equals(obj) =
//        match obj with
//        | :? SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> as s -> this.Values = s.Values
//        | _ -> false

//    override this.GetHashCode () =
//        hash this.Values

//    member this.ContainsKey k =
//        Map.containsKey k this.Values

//    member this.AsMap =
//        this.Values

//    // Filter Values
//    member private this.FilterValues k1f k2f k3f k4f k5f =
//        let k1Filter = SliceFilterBuilder k1f
//        let k2Filter = SliceFilterBuilder k2f
//        let k3Filter = SliceFilterBuilder k3f
//        let k4Filter = SliceFilterBuilder k4f
//        let k5Filter = SliceFilterBuilder k5f
        
//        this.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3 && k4Filter k4 && k5Filter k5)
//        |> Map.toSeq

//    // Slices
//    // 5D
//    member this.Item
//        with get (k1f, k2f, k3f, k4f, k5f) =
//            this.FilterValues k1f k2f k3f k4f k5f |> Map.ofSeq |> SMap5

//    // 4D
//    member this.Item
//        with get (k1, k2f, k3f, k4f, k5f) =
//            this.FilterValues (Equals k1) k2f k3f k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k3, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2, k3f, k4f, k5f) =
//            this.FilterValues k1f (Equals k2) k3f k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k3, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2f, k3, k4f, k5f) =
//            this.FilterValues k1f k2f (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2f, k3f, k4, k5f) =
//            this.FilterValues k1f k2f k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2f, k3f, k4f, k5) =
//            this.FilterValues k1f k2f k3f k4f (Equals k5) 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap4


//    // 3D
//    member this.Item
//        with get (k1, k2, k3f, k4f, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) k3f k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1, k2f, k3, k4f, k5f) =
//            this.FilterValues (Equals k1) k2f (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1, k2f, k3f, k4, k5f) =
//            this.FilterValues (Equals k1) k2f k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1, k2f, k3f, k4f, k5) =
//            this.FilterValues (Equals k1) k2f k3f k4f (Equals k5) 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2, k3, k4f, k5f) =
//            this.FilterValues k1f (Equals k2) (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2, k3f, k4, k5f) =
//            this.FilterValues k1f (Equals k2) k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2, k3f, k4f, k5) =
//            this.FilterValues k1f (Equals k2) k3f k4f (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3, k4, k5f) =
//            this.FilterValues k1f k2f (Equals k3) (Equals k4) k5f 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3, k4f, k5) =
//            this.FilterValues k1f k2f (Equals k3) k4f (Equals k5)  
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3f, k4, k5) =
//            this.FilterValues k1f k2f k3f (Equals k4) (Equals k5)  
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3), v) 
//            |> Map.ofSeq 
//            |> SMap3


//    // 2D
//    member this.Item
//        with get (k1, k2, k3, k4f, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1, k2, k3f, k4, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1, k2, k3f, k4f, k5) =
//            this.FilterValues (Equals k1) (Equals k2) k3f k4f (Equals k5) 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2, k3, k4, k5f) =
//            this.FilterValues k1f (Equals k2) (Equals k3) (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k5), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2, k3, k4f, k5) =
//            this.FilterValues k1f (Equals k2) (Equals k3) k4f (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k4), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2f, k3, k4, k5) =
//            this.FilterValues k1f k2f (Equals k3) (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    // 1D
//    member this.Item
//        with get (k1, k2, k3, k4, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) (Equals k3) (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k5), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2, k3, k4f, k5) =
//            this.FilterValues (Equals k1) (Equals k2) (Equals k3) k4f (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k4), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2, k3f, k4, k5) =
//            this.FilterValues (Equals k1) (Equals k2) k3f (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2f, k3, k4, k5) =
//            this.FilterValues (Equals k1) k2f (Equals k3) (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1f, k2, k3, k4, k5) =
//            this.FilterValues k1f (Equals k2) (Equals k3) (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1), v) 
//            |> Map.ofSeq 
//            |> SMap

//    // 0D (aka GetItem)
//    member this.Item
//        with get(k1, k2, k3, k4, k5) =
//            this.Values.[k1, k2, k3, k4, k5] 

//    // Operators
//    static member inline (*) (lhs, rhs:SMap5<_,_,_,_,_,_>) =
//        rhs.Values
//        |> Map.map (fun k v -> lhs * v)
//        |> SMap5

//    static member inline (*) (lhs:SMap5<_,_,_,_,_,_>, rhs) =
//        lhs.Values
//        |> Map.map (fun k v -> rhs * v)
//        |> SMap5

//    static member inline (.*) (lhs:SMap5<_,_,_,_,_,_>, rhs:SMap5<_,_,_,_,_,_>) =
//        lhs.Values
//        |> Map.filter (fun k _ -> rhs.ContainsKey k)
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * rhs.[k1, k2, k3, k4, k5])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k2, k3, k4, k5))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k2, k3, k4, k5])
//        |> SMap5

//    static member inline (.*) (b:SMap4<_,_,_,_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k1, k2, k3, k4))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1, k2, k3, k4])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap3<_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k3, k4, k5))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k3, k4, k5])
//        |> SMap5

//    static member inline (.*) (b:SMap3<_,_,_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k1, k2, k3))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1, k2, k3])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap2<_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k4, k5))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k4, k5])
//        |> SMap5

//    static member inline (.*) (b:SMap2<_,_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k1, k2))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1, k2])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap<_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey k5)
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k5])
//        |> SMap5

//    static member inline (.*) (b:SMap<_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey k1)
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1])
//        |> SMap5


//    static member inline (+) (lhs:SMap5<_,_,_,_,_,_>, rhs:SMap5<_,_,_,_,_,_>) =
//        match Map.count lhs.Values > Map.count rhs.Values with
//        | true ->  mergeAddition lhs.Values rhs.Values
//        | false -> mergeAddition rhs.Values lhs.Values
//        |> SMap5

//    static member inline Sum (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap5<_,_,_,_,_,Flips.Types.Decision>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap5<_,_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


//module SMap5 =

//    let ofMap m =
//        m |> SMap5

//    let toMap (m:SMap5<_,_,_,_,_,_>) =
//        m.Values

//    let ofList m =
//        m |> Map.ofList |> SMap5

//    let toList (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toList

//    let ofSeq m =
//        m |> Map.ofSeq |> SMap5

//    let toSeq (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toSeq

//    let ofArray m =
//        m |> Map.ofArray |> SMap5

//    let toArray (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toArray

//    let containsKey k (m:SMap5<_,_,_,_,_,_>) =
//        Map.containsKey k m.Values

//    let reKey f m =
//        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq