module type FooType = sig
  type t
end

module type BarType = sig
  type elt
  type t
end

module Functor (FooM : FooType) : BarType with type elt = FooM.t = struct
  type elt = FooM.t
  type t = elt list
end


module rec Foo : FooType with type t = int * Bar.t = struct
  type t = int * Bar.t
end
and Bar : BarType with type elt = Foo.t = Functor(Foo)


type 'a seq = Nil | Cons of 'a * ('a * 'a) seq

let rec size : type a . a seq -> int = function
  | Nil -> 0
  | Cons (_, s) -> 1 + 2 * size s


module type Queue = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a t -> 'a -> 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
  val concat : 'a t -> 'a t -> 'a t
end

module CatQueue (Q : Queue) : Queue = struct
  type 'a cat = Cat of 'a * 'a cat Lazy.t Q.t
  type 'a t = 'a cat option

  let empty = None
  let is_empty q = q = None
  let link (Cat (x, q)) s = Cat (x, Q.snoc q s)

  let concat q1 q2 = match q1, q2 with
    | None, q | q, None -> q
    | Some s1, Some s2 -> Some (link s1 @@ lazy s2)

  let cons x xs = concat (Some (Cat (x, Q.empty))) xs
  let snoc xs x = concat xs (Some (Cat (x, Q.empty)))

  let head = function
    | None -> raise Not_found
    | Some (Cat (x, _)) -> x

  let rec link_all q =
    let lazy t = Q.head q in
    let q' = Q.tail q in
    if Q.is_empty q' then t
    else link t @@ lazy (link_all q')

  let tail = function
    | None -> raise Not_found
    | Some Cat (_, q) -> 
      if Q.is_empty q then None
      else Some (link_all q)
end
