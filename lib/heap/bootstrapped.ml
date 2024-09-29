module Bootstrap (Primitive : Heap.Make) : Heap.Make = functor (Ord : PartialOrder) -> struct
  type elt = Ord.t

  module rec HOrd : PartialOrder with type t = elt * H.t = struct
    type t = elt * H.t
    let le (x1, h1) (x2, h2) = Ord.le x1 x2
  end
  and H : Heap with type elt = HOrd.t = Primitive(HOrd)

  type t = Empty | Root of H.elt

  let empty = Empty

  let is_empty h = h = Empty

  let find_min = function
    | Empty -> raise Not_found
    | Root (x, _) -> x

  let merge bh1 bh2 = match bh1, bh2 with
    | bh, Empty | Empty, bh -> bh
    | Root (x1, h1), Root (x2, h2) when Ord.le x1 x2 ->
      Root (x1, H.insert (x2, h2) h1)
    | Root (x1, h1), Root (x2, h2) ->
      Root (x2, H.insert (x1, h1) h2)

  let insert x h = merge (Root (x, H.empty)) h

  let delete_min = function
    | Empty -> raise Not_found
    | Root (x, h) ->
      if H.is_empty h then Empty else
        let (y, h1) = H.find_min h in
        let h2 = H.delete_min h in
        Root (y, H.merge h1 h2)
end
