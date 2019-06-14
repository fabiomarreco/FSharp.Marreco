module Model
open System

type Date = DateTime
type Currency = | BRL | USD
type Amount = decimal

module Money = 
    type Money = 
        private
        | Money of Currency * Amount
        | ConvertedMoney of Currency * Money

    let create currency amount = Money (currency, amount)
    let convert currency money = ConvertedMoney (currency, money)
    let rec map f = function 
                    | Money (c, a) -> Money (c, f a)
                    | ConvertedMoney (c, m) -> ConvertedMoney (c, map f m)

open Money


type Rate = decimal
type DiscountFactor = decimal

type DayCountConvention = Undefined
module DayCount =
    let datesToYearPeriod (_ : DayCountConvention) (initialDate:Date) (endDate:Date) 
        = (decimal <| endDate.Subtract(initialDate).TotalDays) / 365.0m //TODO 

type RateCompound = Undefined
module Compound = 
    let rateToDiscount (_: RateCompound) rate period =  
        decimal <| (1.0m + rate*period) // TODO

type CashFlowId = string
type FutureValue = FV of Date * Money

type CashFlowDescription = 
    | Interest of {| Rate: Rate; InitialDate : Date; EndDate : Date; Notional : Money |}
    | Amortization of Money

type CashFlowDefinition = { 
    Id            : CashFlowId
    SettlementDate: Date
    Description   : CashFlowDescription
}

type AnalyticAttribute() = 
    inherit Attribute()

//-----------------------------
type CashFlowsDefinitions = CashFlowDefinition list
type MtM = Money
type Yield = Rate
type CashFlowDiscountFactors = Map<CashFlowId, DiscountFactor>
type CashFlowPresentValues = Map<CashFlowId, Money>
type FutureCashFlows = Map<CashFlowId, FutureValue>
type Principal = Money
type PrincipalCurrency = Currency

//------------------------------
//virou uma discussao sobre currency.....
let futureCashFlows (cashFlows, dayCount, compound) : FutureCashFlows = 
    let toFutureCashFlow = 
        function 
        | Amortization value -> value
        
        | Interest desc -> //TODO estou ignorando a moeda.....
            let period = DayCount.datesToYearPeriod dayCount desc.InitialDate desc.EndDate
            let discount = Compound.rateToDiscount compound desc.Rate period
            let futureValue = desc.Notional |> Money.map ((*) (1m - 1m/discount))
            futureValue

    cashFlows 
        |> List.map 
            (fun c-> 
                let moneyValue = c.Description |> toFutureCashFlow
                c.Id, FV (c.SettlementDate, moneyValue))
        |> Map.ofList
    
