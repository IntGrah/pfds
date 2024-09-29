module Pairing : Heap.Make = functor (Ord : PartialOrder) -> struct
  type elt = Ord.t
  type tree = Tree of elt * tree list
  type t = Empty | Root of tree

  let empty = Empty
  let is_empty h = h = Empty

  let link (Tree (x, xs) as l) (Tree (y, ys) as r) =
    if Ord.le x y then Tree (x, r::xs) else Tree (y, l::ys)

  let add t = function
    | Empty -> Root t
    | Root r -> Root (link t r)

  let rec merge_pairs = function
    | [] -> Empty
    | [x] -> Root x
    | x::y::xs -> add (link x y) (merge_pairs xs)

  let insert x = add (Tree (x, []))

  let merge h1 h2 = match h1, h2 with
    | Empty, h -> h
    | Root t, h -> add t h

  let delete_min = function
    | Empty -> raise Not_found
    | Root Tree (_, xs) -> merge_pairs xs

  let find_min = function
    | Empty -> raise Not_found
    | Root Tree (x, _) -> x
end