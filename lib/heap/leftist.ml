module Leftist : Heap.Make = functor (Ord : PartialOrder) -> struct
  type elt = Ord.t
  type t = Leaf | Tree of elt * int * t * t

  let empty = Leaf

  let is_empty h = h = Leaf

  let s_value = function
    | Leaf -> 0
    | Tree (_, s, _, _) -> s

  let rec merge t1 t2 = match t1, t2 with
    | Leaf, t | t, Leaf -> t
    | Tree (x, _, l, r), Tree (y, _, _, _) when Ord.le x y ->
      let r' = merge r t2 in
      if l = Leaf then
        Tree (x, 1, r', Leaf)
      else if s_value l < s_value r' then
        Tree (x, s_value r' + 1, r', l)
      else
        Tree (x, s_value r' + 1, l, r')
    | _, _ -> merge t2 t1

  let insert x = merge (Tree (x, 1, Leaf, Leaf))

  let find_min = function
    | Leaf -> raise Not_found
    | Tree (x, _, _, _) -> x

  let delete_min = function
    | Leaf -> raise Not_found
    | Tree (_, _, l, r) -> merge l r
end
