module Rooted (Primitive : Heap.Make) : Heap.Make = functor (Ord : PartialOrder) -> struct
  module H = Primitive(Ord)
  type elt = Ord.t
  type t = Empty | Root of elt * H.t

  let empty = Empty

  let is_empty h = h = Empty

  let find_min = function
    | Empty -> raise Not_found
    | Root (x, _) -> x
  let insert x = function
    | Empty -> Root (x, H.empty)
    | Root (y, h) when Ord.le x y -> Root (x, H.insert y h)
    | Root (y, h) -> Root (y, H.insert x h)

  let merge rh1 rh2 = match rh1, rh2 with
    | _, Empty -> rh1
    | Empty, _ -> rh2
    | Root (x1, h1), Root (x2, h2) when Ord.le x1 x2 ->
      Root (x1, H.insert x2 h1)
    | Root (x1, h1), Root (x2, h2) ->
      Root (x2, H.insert x1 h2)

  let delete_min = function
    | Empty -> raise Not_found
    | Root (x, h) ->
      if H.is_empty h then Empty
      else Root (H.find_min h, H.delete_min h)
end
