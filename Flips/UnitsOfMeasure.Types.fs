module Flips.UnitsOfMeasure.Types

open Flips

type Decision<[<Measure>] 'Measure> =
    | Value of Types.Decision
with

    member this.Name =
        let (Value d) = this
        d.Name

and LinearExpression<[<Measure>] 'Measure> =
    | Value of Types.LinearExpression
    with

        static member inline Zero =
            let expr = Types.LinearExpression (Set.empty, Map.empty, Map.empty, Types.Scalar.Zero)
            LinearExpression<'Measure>.Value expr

type AsLinearExpressionOverloads() =
    static member inline AsLinearExpression<[<Measure>] 'Measure>(f: float<'Measure>) : LinearExpression<'Measure> = 
        let f = float f
        let expr = Flips.Types.AsLinearExpressionOverloads.AsLinearExpression(f)
        LinearExpression<'Measure>.Value expr

    static member inline AsLinearExpression<[<Measure>] 'Measure>(Decision.Value d:Decision<'Measure>) : LinearExpression<'Measure> =
        let expr = Flips.Types.AsLinearExpressionOverloads.AsLinearExpression(d)
        LinearExpression<'Measure>.Value expr

    static member inline AsLinearExpression<[<Measure>] 'Measure>(l:LinearExpression<'Measure>) : LinearExpression<'Measure> = l
   
    static member inline Invoke<[<Measure>] 'Measure>(input: ^LinearExpressionLike) : LinearExpression<'Measure> =
        let inline call (mthd: ^M, input: ^I) = ((^M or ^I) : (static member AsLinearExpression<[<Measure>] 'Measure> : _ -> LinearExpression<'Measure>) input)
        call(Unchecked.defaultof<AsLinearExpressionOverloads>, input)

let inline (|AsLinearExpression|) x : LinearExpression<'Measure> = AsLinearExpressionOverloads.Invoke(x)

type LinearExpression with

    static member private Merge (l:LinearExpression, r:LinearExpression) =
        // Assume the Left LinearExpression is larget than the right
        let nameOverlap = Set.intersect l.Names r.Names
    
        for n in nameOverlap do
            if l.Decisions.[n].Type <> r.Decisions.[n].Type then
                let (DecisionName name) = n
                invalidArg name "Cannot have mismatched DecisionTypes for same DecisionName"

        let newNames = l.Names + r.Names

        let newDecs = (l.Decisions, (r.Names - l.Names)) ||> Set.fold (fun m k -> Map.add k r.Decisions.[k] m)

        let newCoefs =
            (l.Coefficients, nameOverlap)
            ||> Set.fold (fun m k -> Map.add k (l.Coefficients.[k] + r.Coefficients.[k]) m)
            |> fun updatedCoefs -> Set.fold (fun m n -> Map.add n r.Coefficients.[n] m) updatedCoefs (r.Names - l.Names)

        LinearExpression (newNames, newCoefs, newDecs, l.Offset + r.Offset)

    static member inline (+) (AsLinearExpression l, AsLinearExpression r) =
        let lSize = Set.count l.Names
        let rSize = Set.count r.Names

        if lSize > rSize then
            LinearExpression.Merge (l, r)
        else
            LinearExpression.Merge (r, l)

    static member inline (*) (AsLinearExpression expr, AsFloat f) =
        let newCoefs = Map.map (fun k v -> v * f) expr.Coefficients
        LinearExpression (expr.Names, newCoefs, expr.Decisions, expr.Offset * f)

    static member inline (*) (AsFloat f, AsLinearExpression expr) =
        expr * f

    static member inline (-) (AsLinearExpression l, AsLinearExpression r) =
        l + (-1.0 * r)

    static member inline (<==) (AsLinearExpression l, AsLinearExpression r) =
        Inequality (l, LessOrEqual, r)

    static member inline (==) (AsLinearExpression l, AsLinearExpression r) =
        Equality (l, r)

    static member inline (>==) (AsLinearExpression l, AsLinearExpression r) =
        Inequality (l, GreaterOrEqual, r)

type DecisionType<[<Measure>] 'Measure> =
    | Boolean
    | Integer of LowerBound:float<'Measure> * UpperBound:float<'Measure>
    | Continuous of LowerBound:float<'Measure> * UpperBound:float<'Measure>