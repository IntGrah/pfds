module SkewBinomial : Heap.Make = functor (Ord : PartialOrder) -> struct
  type elt = Ord.t
  type tree = Tree of elt * int * tree list
  type t = tree list

  let empty = []
  let is_empty h = h = []
  let rank (Tree (_, r, _)) = r

  let simple_link (Tree (k1, r, c1) as t1) (Tree (k2, r, c2) as t2) =
    if Ord.le k1 k2 then
      Tree (k1, r + 1, t2 :: c1)
    else
      Tree (k2, r + 1, t1 :: c2)

  let skew_link k1 (Tree (k2, r, c2) as t2) (Tree (k3, r, c3) as t3) =
    if Ord.le k1 k2 && Ord.le k1 k3 then (* type A *)
      Tree (k1, r + 1, [t2; t3])
    else (* type B *)
      let t1 = Tree (k1, 0, []) in
      if Ord.le k2 k3 then
        Tree (k2, r + 1, t1 :: t3 :: c2)
      else
        Tree (k3, r + 1, t1 :: t2 :: c3)

  let rec unique = function
    | t1 :: t2 :: ts when rank t1 = rank t2 ->
      unique (simple_link t1 t2 :: ts)
    | ts -> ts

  let rec merge_uniq h1 h2 = match h1, h2 with
    | h1, [] -> h1
    | [], h2 -> h2
    | t1 :: ts1, t2 :: ts2 ->
      if rank t1 < rank t2 then
        t1 :: merge_uniq ts1 h2
      else if rank t1 > rank t2 then
        t2 :: merge_uniq h1 ts2
      else
        unique (simple_link t1 t2 :: merge_uniq ts1 ts2)

  let merge h1 h2 = merge_uniq (unique h1) (unique h2)

  let insert x = function
    | t2 :: t3 :: ts when rank t2 = rank t3 -> skew_link x t2 t3 :: ts
    | ts -> Tree (x, 0, []) :: ts

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
    let (z, ts) = List.partition_map (function Tree (x, 0, _) -> Left x | t -> Right t) c in
    List.fold_right insert z (merge (List.rev ts) h)
end
