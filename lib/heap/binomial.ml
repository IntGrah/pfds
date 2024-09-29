module Binomial : Heap.Make = functor (Ord : PartialOrder) -> struct
  type elt = Ord.t
  type tree = Tree of elt * int * tree list
  type t = tree list

  let empty = []
  let is_empty h = h = []
  let rank (Tree (_, r, _)) = r

  let link (Tree (k1, r, c1) as t1) (Tree (k2, r, c2) as t2) =
    if Ord.le k1 k2 then
      Tree (k1, r + 1, t2 :: c1)
    else
      Tree (k2, r + 1, t1 :: c2)

  let rec add t1 = function
    | t2 :: ts when rank t1 = rank t2 -> add (link t1 t2) ts
    | ts -> t1 :: ts

  let merge h1 h2 = List.fold_right add h1 h2

  let insert x = add (Tree (x, 0, []))

  let rec find_min = function
    | [] -> raise Not_found
    | [Tree (x, _, _)] -> x
    | Tree (x, _, _) :: ts -> min x (find_min ts)

  let delete_min h =
    let rec get_min = function
      | [] -> raise Not_found
      | [t] -> t, []
      | Tree (x1, _, _) as t1 :: ts1 ->
        let (Tree (x2, _, _) as t2, ts2) = get_min ts1 in
        if Ord.le x1 x2 then t1, ts1 else t2, t1 :: ts2
    in
    let (Tree (_, _, c), h) = get_min h in
    merge (List.rev c) h
end
