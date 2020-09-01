namespace Flips.SliceMap

open System


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


    let internal SliceFilterBuilder<'a when 'a : comparison> (compare:('a * 'a) -> int) (f:SliceType<'a>) =
        match f with
        | All -> fun _ -> true
        | Equals x -> fun k -> compare (x, k) = 0
        | GreaterThan x -> fun k -> compare (x, k) > 0
        | GreaterOrEqual x -> fun k -> compare (x, k) >= 0
        | LessThan x -> fun k -> compare (x, k) < 0
        | LessOrEqual x -> fun k -> compare (x, k) <= 0
        | Between (lowerBound, upperBound) -> fun k -> compare (lowerBound, k) >= 0 && compare (upperBound, k) <= 0
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


module SliceData =

    let equals (compare:('a * 'a) -> int) (aKeys:Memory<'a>, aValues:Memory<'v>) (bKeys:Memory<'a>, bValues:Memory<'v>) =
        if aKeys.Length <> bKeys.Length then
            false
        else
            let mutable idx = 0
            let mutable result = true

            while idx < aKeys.Length && result do
                let c = compare(aKeys.Span.[idx], bKeys.Span.[idx])
                if c <> 0 || aValues.Span.[idx] <> bValues.Span.[idx] then
                    result <- false
                idx <- idx + 1

            result

    let findIndexOf (compare:('a * 'a) -> int) startingLowerBound x (values:Memory<_>) =
        let mutable lowerBound = startingLowerBound
        let mutable upperBound = values.Length - 1
        let mutable idx = (lowerBound + upperBound) / 2

        while lowerBound <= upperBound do
            let x = compare (values.Span.[idx], x)
            if x <= 0 then
                lowerBound <- idx + 1
                idx <- (lowerBound + upperBound) / 2
            else
                upperBound <- idx - 1
                idx <- (lowerBound + upperBound) / 2

        idx

    let contains (compare:('a * 'a) -> int) x (values:Memory<_>) =
        let mutable idx = 0
        let mutable doesContain = false

        while (idx < values.Length && not doesContain) do
            let c = compare (values.Span.[idx], x)
            if c = 0 then
                doesContain <- true
            idx <- idx + 1

        doesContain

    let getItem (compare:('a * 'a) -> int) k (keys:Memory<_>, values:Memory<_>) =
        let idx = findIndexOf compare 0 k keys

        if keys.Span.[idx] = k then
            values.Span.[idx]
        else
            invalidArg "Key" "Key not in SliceMap"

    let filterByKey f (reKey:('OldKey -> 'NewKey)) (keys:Memory<_>, values:Memory<_>) =
        let newValues = Array.zeroCreate(values.Length)
        let newKeys = Array.zeroCreate(keys.Length)

        let mutable idx = 0
        let mutable outIdx = 0

        while idx < keys.Length do
            if f keys.Span.[idx] then
                newKeys.[outIdx] <- reKey keys.Span.[idx]
                newValues.[outIdx] <- values.Span.[idx]
                outIdx <- outIdx + 1
            
            idx <- idx + 1

        newKeys.AsMemory(0, outIdx), newValues.AsMemory(0, outIdx)

    let inline scale coefficient (values:Memory<_>) =
        let newValues = Array.zeroCreate(values.Length)
        let mutable idx = 0

        while idx < values.Length do
            newValues.[idx] <- values.Span.[idx] * coefficient
            idx <- idx + 1

        newValues.AsMemory()

    let inline hadamardProduct (compare:('a * 'a) -> int) (aKeys:Memory<_>, aValues:Memory<_>) (bKeys:Memory<_>, bValues:Memory<_>) =
            let newKeys = Array.zeroCreate(Math.Min(aKeys.Length, bKeys.Length))
            let newValues = Array.zeroCreate(Math.Min(aValues.Length, bValues.Length))
            let mutable aIdx = 0
            let mutable bIdx = 0
            let mutable outIdx = 0

            while (aIdx < aKeys.Length && bIdx < bKeys.Length) do
        
                let c = compare (aKeys.Span.[aIdx], bKeys.Span.[bIdx])
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

    let inline projectHadamardProduct (keyMapper:('a -> 'b)) (compare:('b * 'b) -> int) (aKeys:Memory<'a>, aValues:Memory<_>) (bKeys:Memory<'b>, bValues:Memory<_>) =
        let newKeys = Array.zeroCreate(Math.Max(aKeys.Length, bKeys.Length))
        let newValues = Array.zeroCreate(Math.Max(aValues.Length, bValues.Length))
        let mutable aIdx = 0
        let mutable bIdx = 0
        let mutable outIdx = 0

        if newKeys.Length > 0 then
            while (aIdx < aKeys.Length) do
                let bLookupKey = keyMapper aKeys.Span.[aIdx]
                bIdx <- findIndexOf compare 0 bLookupKey bKeys
                if bIdx < bKeys.Length then
                    let c = compare (bLookupKey, bKeys.Span.[bIdx])

                    if c = 0 then
                        newKeys.[outIdx] <- aKeys.Span.[aIdx]
                        newValues.[outIdx] <- aValues.Span.[aIdx] * bValues.Span.[bIdx]
                        outIdx <- outIdx + 1

                aIdx <- aIdx + 1

        newKeys.AsMemory(0, outIdx), newValues.AsMemory(0, outIdx)

    let inline add (compare:('a * 'a) -> int) (aKeys:Memory<'a>, aValues:Memory<'v>) (bKeys:Memory<'a>, bValues:Memory<'v>) =
        let newKeys = Array.zeroCreate(aKeys.Length + bKeys.Length)
        let newValues = Array.zeroCreate(aValues.Length + bValues.Length)

        let mutable aIdx = 0
        let mutable bIdx = 0
        let mutable outIdx = 0

        while (aIdx < aValues.Length && bIdx < bValues.Length) do
        
            let c = compare (aKeys.Span.[aIdx], bKeys.Span.[bIdx])

            if c < 0 then
                newKeys.[outIdx] <- aKeys.Span.[aIdx]
                newValues.[outIdx] <- aValues.Span.[aIdx]
                aIdx <- aIdx + 1
            elif c = 0 then
                newKeys.[outIdx] <- aKeys.Span.[aIdx]
                newValues.[outIdx] <- aValues.Span.[aIdx] + bValues.Span.[bIdx]
                aIdx <- aIdx + 1
                bIdx <- bIdx + 1
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

    let toSeq (keys:Memory<_>, values:Memory<_>) =
        if keys.Length <> values.Length then
            invalidArg "Values" "The number of Values must match the number of Keys"

        seq {for i in 0..keys.Length -> keys.Span.[i], values.Span.[i]}

    let toMap (keys:Memory<_>, values:Memory<_>) =
        if keys.Length <> values.Length then
            invalidArg "Values" "The number of Values must match the number of Keys"

        seq {for i in 0..keys.Length -> keys.Span.[i], values.Span.[i]}
        |> Map.ofSeq

    let ofSeq (s:seq<'Key*'Value>) =
        let sorted = s |> Seq.distinctBy fst |> Seq.sortBy fst

        let keys =
            sorted
            |> Seq.map fst
            |> Array.ofSeq
            |> fun x -> x.AsMemory()

        let values =
            sorted
            |> Seq.map snd
            |> Array.ofSeq
            |> fun x -> x.AsMemory()

        keys, values

    let ofMap (m:Map<_,_>) =
        m |> Map.toSeq |> ofSeq






