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
                for period in data.Periods.[2..] ->
                    sales.[product, period] <== inventory.[product, period]
        }


    let objectiveExpression  = sumAll [for p in data.Periods -> sales.[All, p] .* data.SalePrice]
    let objective = Objective.create "MaximizeSales" Maximize objectiveExpression
    let m = 
        Model.create objective
        |> Model.addConstraints salesLessThanDemandConstraints
        |> Model.addConstraints salesLessThanInventoryConstraints
        |> Model.addConstraints inventoryContinuityConstraints

    m

let solveModel () =
    let data = 
    let m = buildModel