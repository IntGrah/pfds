module Skew : Heap.Make = functor (Ord : PartialOrder) -> struct
  type elt = Ord.t
  type t = Leaf | Tree of Ord.t * t * t

  let empty = Leaf
  let is_empty h = h = Leaf

  let rec merge t1 t2 = match t1, t2 with
    | Leaf, t | t, Leaf -> t
    | Tree (x1, l1, r1), Tree (x2, l2, r2) ->
      if Ord.le x1 x2 then
        Tree (x1, merge t2 r1, l1)
      else
        Tree (x2, merge t1 r2, l2)

  let insert x = merge (Tree (x, Leaf, Leaf))

  let find_min = function
    | Leaf -> raise Not_found
    | Tree (x, _, _) -> x

  let delete_min = function
    | Leaf -> raise Not_found
    | Tree (_, l, r) -> merge l r
end
