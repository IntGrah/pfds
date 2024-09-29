module RBT : SearchTree = struct
  type color = R | B
  type 'a t = E | N of color * 'a t * 'a * 'a t
  exception Invariant

  let empty = E

  let rec search x = function
    | E -> false
    | N (_, l, y, _) when x < y -> search x l
    | N (_, _, y, r) when x > y -> search x r
    | _ -> true

  let fix_red_red = function
    | N (B, N (R, N (R, a, x, b), y, c), z, d)
    | N (B, N (R, a, x, N (R, b, y, c)), z, d)
    | N (B, a, x, N (R, N (R, b, y, c), z, d))
    | N (B, a, x, N (R, b, y, N (R, c, z, d))) ->
      N (R, N (B, a, x, b), y, N (B, c, z, d))
    | n -> n

  let set_black = function E -> E | N (_, l, x, r) -> N (B, l, x, r)
  let set_red = function E -> E | N (_, l, x, r) -> N (R, l, x, r)
  let is_black = function N (R, _, _, _) -> false | _ -> true
  let is_red = function N (R, _, _, _) -> true | _ -> false

  let insert x t =
    let rec ins = function
      | E -> N (R, E, x, E)
      | N (c, l, y, r) when x < y -> fix_red_red @@ N (c, (ins l), y, r)
      | N (c, l, y, r) when x > y -> fix_red_red @@ N (c, l, y, (ins r))
      | n -> n
    in set_black (ins t)

  let bal_left l x r = match l, x, r with
    | l, x, r when is_red l -> N (R, set_black l, x, r)
    | l, x, r when is_black r -> fix_red_red @@ N (B, l, x, set_red r)
    | l, x, N (R, N (B, a, y, b), z, c) ->
      N (R, N (B, l, x, a), y, fix_red_red @@ N (B, b, z, set_red c))
    | _ -> raise Invariant

  let bal_right l x r = match l, x, r with
    | l, x, r when is_red r -> N (R, l, x, set_black r)
    | l, x, r when is_black l -> fix_red_red @@ N (B, set_red l, x, r)
    | N (R, a, x, N (B, b, y, c)), z, r ->
      N (R, fix_red_red @@ N (B, set_red a, x, b), y, N (B, c, z, r))
    | _ -> raise Invariant

  let rec merge l r = match l, r with
    | E, s | s, E -> s
    | N (r, a, x, b), N (r', c, y, d) when r = r' ->
      (match merge b c with
       | N (R, b', z, c') -> N (R, N (r, a, x, b'), z, N (r, c', y, d))
       | bc -> if r = R then N (R, a, x, N (R, c, y, d))
         else bal_left a x @@ N (B, bc, y, d))
    | a, N (R, b, x, c) -> N (R, merge a b, x, c)
    | N (R, a, x, b), c -> N (R, a, x, merge b c)

  let delete x t =
    let rec del = function
      | E -> E
      | N (_, l, y, r) when x < y ->
        if is_black l then bal_left (del l) y r
        else N (R, del l, y, r)
      | N (_, l, y, r) when x > y ->
        if is_black r then bal_right l y (del r)
        else N (R, l, y, del r)
      | N (_, l, y, r) -> merge l r
    in set_black (del t)
end
