module type SearchTree = sig
  type 'a t
  val empty : 'a t
  val search : 'a -> 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val delete : 'a -> 'a t -> 'a t
  val to_string : ('a -> string) -> 'a t -> string
  val to_list : 'a t -> 'a list
end
