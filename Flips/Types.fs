module Flips.Types


[<CustomEquality; CustomComparison>]
type Scalar = Value of float with

    static member private NearlyEquals (Value a:Scalar) (Value b:Scalar) : bool =
        let aValue = System.BitConverter.DoubleToInt64Bits a
        let bValue = System.BitConverter.DoubleToInt64Bits b
        if (aValue >>> 63) <> (bValue >>> 63) then
            a = b
        else
            System.Math.Abs(aValue - bValue) <= 10_000L

    static member Zero = Value 0.0

    override this.GetHashCode () =
        let (Value v) = this
        hash v

    override this.Equals(obj) =
        match obj with
        | :? Scalar as s -> Scalar.NearlyEquals this s 
        | _ -> false

    interface System.IComparable with
        member this.CompareTo yObj =
            match yObj with
            | :? Scalar as s -> compare this s
            | _ -> invalidArg "yObj" "Cannot compare values of different types"

type AsFloatOverloads() =
    static member inline AsFloat((Scalar.Value f)) : float = f 
    static member inline AsFloat(f: float) : float = f
   
    static member inline Invoke(input: ^floatLike) : float =
        let inline call (mthd: ^M, input: ^I) = ((^M or ^I) : (static member AsFloat : _ -> float) input)
        call(Unchecked.defaultof<AsFloatOverloads>, input)

let inline (|AsFloat|) x : float = AsFloatOverloads.Invoke(x)

type Scalar with

    static member inline (+) (AsFloat f1, AsFloat f2) =
        Scalar.Value (f1 + f2)

    static member inline (*) (AsFloat f1, AsFloat f2) =
        Scalar.Value (f1 * f2)

    static member inline (-) (AsFloat f1, AsFloat f2) =
        Scalar.Value (f1 - f2)

    static member inline (/) (AsFloat f1, AsFloat f2) =
        Scalar.Value (f1 / f2)

type DecisionType =
    | Boolean
    | Integer of LowerBound:float * UpperBound:float
    | Continuous of LowerBound:float * UpperBound:float

type DecisionName = DecisionName of string

type Decision = {
    Name : DecisionName
    Type : DecisionType
}

and LinearExpression (names:Set<DecisionName>, coefficients : Map<DecisionName, Scalar>, decisions : Map<DecisionName, Decision>, offset:Scalar) =
    member this.Names = names
    member this.Coefficients = coefficients
    member this.Decisions = decisions
    member this.Offset = offset

    static member private Equivalent (lExpr:LinearExpression) (rExpr:LinearExpression) =
        let isEqualOffset = (lExpr.Offset = rExpr.Offset)
        let leftOnlyNames = lExpr.Names - rExpr.Names
        let rightOnlyNames = rExpr.Names - lExpr.Names
        let overlapNames = Set.intersect lExpr.Names rExpr.Names

        let leftOnlyNamesAreZero = 
            leftOnlyNames
            |> Set.forall (fun n -> lExpr.Coefficients.[n] = Scalar.Zero)

        let rightOnlyNamesAreZero =
            rightOnlyNames
            |> Set.forall (fun n -> rExpr.Coefficients.[n] = Scalar.Zero)

        let overlapNamesMatch =
            overlapNames
            |> Set.forall (fun n -> lExpr.Coefficients.[n] = rExpr.Coefficients.[n])

        isEqualOffset && leftOnlyNamesAreZero && rightOnlyNamesAreZero && overlapNamesMatch

    override this.GetHashCode () =
        hash this

    override this.Equals(obj) =
        match obj with
        | :? LinearExpression as expr -> LinearExpression.Equivalent this expr
        | _ -> false

    static member GetDecisions (expr:LinearExpression) =
        expr.Decisions
        |> Map.toList
        |> List.map snd
        |> Set.ofList

    static member Zero =
        LinearExpression (Set.empty, Map.empty, Map.empty, Scalar.Zero)


and AsLinearExpressionOverloads() =
    static member inline AsLinearExpression(f: float) : LinearExpression = 
        LinearExpression (Set.empty, Map.empty, Map.empty, Value f)

    static member inline AsLinearExpression(s:Scalar) : LinearExpression = 
        LinearExpression (Set.empty, Map.empty, Map.empty, s)

    static member inline AsLinearExpression(d:Decision) : LinearExpression =
        let names = Set.ofList [d.Name]
        let coefs = Map.ofList [d.Name, Value 1.0]
        let decs = Map.ofList [d.Name, d]
        LinearExpression (names, coefs, decs, Value 0.0)

    static member inline AsLinearExpression(l:LinearExpression) : LinearExpression = l
   
    static member inline Invoke(input: ^LinearExpressionLike) : LinearExpression =
        let inline call (mthd: ^M, input: ^I) = ((^M or ^I) : (static member AsLinearExpression : _ -> LinearExpression) input)
        call(Unchecked.defaultof<AsLinearExpressionOverloads>, input)

let inline (|AsLinearExpression|) x : LinearExpression = AsLinearExpressionOverloads.Invoke(x)

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


and Inequality =
    | LessOrEqual
    | GreaterOrEqual

and ConstraintExpression = 
    | Inequality of LHS:LinearExpression * Inequality * RHS:LinearExpression
    | Equality of LHS:LinearExpression * RHS:LinearExpression

type ConstraintName = ConstraintName of string

type Constraint = {
    Name : ConstraintName
    Expression : ConstraintExpression
}

type ObjectiveSense =
    | Minimize
    | Maximize

type ObjectiveName = ObjectiveName of string

type Objective = {
    Name : ObjectiveName
    Sense : ObjectiveSense
    Expression : LinearExpression
}

type Solution = {
    DecisionResults : Map<Decision,float>
    ObjectiveResult : float
}

type SolverType = 
    | CBC
    | GLOP
    | Cplex128
    | Gurobi900


type SolverSettings = {
    SolverType : SolverType
    MaxDuration : int64
    WriteLPFile : Option<string>
}

type SolveResult =
    | Optimal of Solution
    | Suboptimal of string