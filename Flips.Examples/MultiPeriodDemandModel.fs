module Flips.Examples.MultiPeriodDemandModel

open Flips.Domain
open Flips.SliceMap
open Flips.Solve

type Product = Product of string
type Period = Period of int

type Data = {
    Products : List<Product>
    Periods : List<Period>
    Demand : SMap2<Product, Period, float>
    PurchaseCost : SMap2<Product, Period, float>
    SalePrice : SMap<Product, float>
    CarryingCost : SMap<Product, float>
    InitialInventory : SMap<Product, float>
}

let buildModel (data:Data) =
    
    let purchase = 
        [for product in data.Products do
            for period in data.Periods ->
                let name = sprintf "Purchase|%A_%A" product period
                (product, period), Decision.createContinuous name 0.0 infinity
        ] |> SMap2.ofList

    let inventory = 
        [for product in data.Products do
            for period in data.Periods ->
                let name = sprintf "Inventory|%A_%A" product period
                (product, period), Decision.createContinuous name 0.0 infinity
        ] |> SMap2.ofList

    let sales = 
        [for product in data.Products do
            for period in data.Periods ->
                let name = sprintf "Sales|%A_%A" product period
                (product, period), Decision.createContinuous name 0.0 infinity
        ] |> SMap2.ofList

    let salesLessThanDemandConstraints =
        ConstraintBuilder "SalesLessThanDemand" {
            for product in data.Products do
                for period in data.Periods ->
                    sales.[product, period] <== data.Demand.[product, period]
        }

    let salesLessThanInventoryConstraints =
        ConstraintBuilder "SalesLessThanInventory" {
            for product in data.Products do
                for period in data.Periods ->
                    sales.[product, period] <== inventory.[product, period]
        }

    let inventoryContinuityConstraints =
        ConstraintBuilder "InventoryContinuity" {
            for product in data.Products do
                for (Period p) in data.Periods.[1..] ->
                    inventory.[product, Period p] == inventory.[product, Period (p - 1)] + purchase.[product, Period (p - 1)] + (-1.0 * sales.[product, Period (p - 1)])
        }

    let initialInventoryConstraints =
        ConstraintBuilder "InitialInventory" {
            for product in data.Products ->
                inventory.[product, data.Periods.[0]] == data.InitialInventory.[product]
        }


    let objectiveExpression  = 
        sumAll [for p in data.Periods -> sales.[All, p] .* data.SalePrice] + 
        (-1.0 * sumAll [for p in data.Periods -> inventory.[All, p] .* data.CarryingCost]) + 
        (-1.0 * sum (purchase .* data.PurchaseCost))

    let alt =
        sum ( data.SalePrice .* sales - data.CarryingCost .* inventory - purchase .* data.PurchaseCost )

    let objective = Objective.create "MaximizeSales" Maximize objectiveExpression
    let m = 
        Model.create objective
        |> Model.addConstraints salesLessThanDemandConstraints
        |> Model.addConstraints salesLessThanInventoryConstraints
        |> Model.addConstraints inventoryContinuityConstraints
        |> Model.addConstraints initialInventoryConstraints

    m

let solveModel () =
    let products = ["Chicken"] |> List.map Product
    let periods = [0..2] |> List.map Period
    let demand = 
        [
            (Product "Chicken", Period 0), 10.0; (Product "Chicken", Period 1), 8.0; (Product "Chicken", Period 2), 12.0;
        ] |> SMap2.ofList

    let purchaseCost =
        [
            (Product "Chicken", Period 0), 1.0; (Product "Chicken", Period 1), 5.0; (Product "Chicken", Period 2), 2.0;
        ] |> SMap2.ofList

    let salesPrice =
        [
            (Product "Chicken"), 10.0
        ] |> SMap.ofList

    let carryingCost =
        [
            (Product "Chicken"), 0.10
        ] |> SMap.ofList

    let initialInventory =
        [
            (Product "Chicken"), 10.0
        ] |> SMap.ofList

    let data = {
        Products = products
        Periods = periods
        Demand = demand
        PurchaseCost = purchaseCost
        SalePrice = salesPrice
        CarryingCost = carryingCost
        InitialInventory = initialInventory
    }

    let m = buildModel data

    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    // Call the `solve` function in the Solve module to evaluate the model
    let result = solve settings m

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (DecisionName name, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %s\tValue: %f" name value