open Heap

type date = 
{ 
  year: int
  ; month: int
  ; day: int

}
type order = 
{
  order_id : string 
  ; security_id: string
  ; price : float
  ; dir : string
  ; market_participant: string
  ; timestamp: date
}

type limit = 
{
  mutable size: int
  ;limit_price: float
  ; orders: order array; 
}

let offer_comare l1 l2 = l1.limit_price < l2.limit_price
let bid_compare l1 l2 = l1.limit_price > l2.limit_price

let default_order = 
  {
  order_id = ""
  ; security_id = ""
  ; price = 0.0
  ; dir =""
  ; market_participant= ""
  ; timestamp = {year = 1970; month = 3; day = 1 }
  }
let default_bid_limit = {
  size = 0
  ; limit_price = 0.0
  ; orders = [|default_order|]}

let default_offer_limit = {
  size = 0
  ; limit_price = infinity
  ; orders = [|default_order|]}


let bids = create_heap 1000 default_bid_limit bid_compare
let offers = create_heap 1000 default_offer_limit offer_comare


`

let matching_engine (bids: limit heap) (offers: limit heap) = 
  if bids.size > 0 && offers.size > 0 then 
    let best_bid = top bids in 
    let best_offer = top offers in 
    (* CR-07-17-24 raghazadehtabriz: need to do matching logic *)
    Printf.printf "Matching bid at %f with offer at %f\n" best_bid.limit_price best_offer.limit_price

    