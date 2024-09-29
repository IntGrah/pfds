module type PartialOrder = sig
  type t
  val le : t -> t -> bool
end

module type Heap = sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val merge : t -> t -> t
  val find_min : t -> elt
  val delete_min : t -> t
end

module Heap = struct
  module type Make = functor (Ord : PartialOrder) -> Heap with type elt = Ord.t
end
