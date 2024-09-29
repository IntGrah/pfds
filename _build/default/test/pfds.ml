open Leftist

module PQ = Bootstrap(Leftist) (struct
    type t = int
    let le = (<=)
  end)

let rec to_list h =
  if PQ.is_empty h then []
  else PQ.find_min h :: to_list (PQ.delete_min h)
let heap_sort xs = List.fold_right PQ.insert xs PQ.empty |> to_list

let h1 = List.fold_right PQ.insert [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5] PQ.empty
let () = heap_sort [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5] |> List.iter print_int


module type Functor = functor (H : Heap.Make) -> sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val find_min : 'a t -> 'a option
  val delete_min : 'a t -> 'a t
end



let t = List.fold_right RBT.insert [5; 1; 4; 3; 6; 9; 2; 8; 0; 7] RBT.empty
let () = print_endline @@ RBT.to_string string_of_int t;;

let t = RBT.delete 3 t;;
let t = RBT.delete 4 t;;
let t = RBT.delete 9 t;;
let t = RBT.delete 8 t;;
let t = RBT.delete 7 t;;
List.iter print_int @@ RBT.to_list t;;
print_endline "";;
let () = print_endline @@ RBT.to_string string_of_int t;;
