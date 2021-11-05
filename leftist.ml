(*     Autor: Jonasz Aleszkiewicz *)
(* Recenzent: Łukasz Smoliński    *)

(* Kolejka priorytetowa jest drzewem składającym się z wierzchołków
   pamiętających swoją wartość (będącą priorytetem w kolejce), swoje
   wierzchołki-dzieci i "wysokość" -- długość idącej tylko w prawo w dół ścieżki
   do liścia. [End] to wierzchołek jedną krawędź za liściem, czyli nie
   wierzchołek. Pusta kolejka to samotne [End]. *)

type 'a queue = End | Node of 'a node

and 'a node = { left : 'a queue; value : 'a; right : 'a queue; height : int }

exception Empty

let empty = End

let is_empty q = q = End

(** Zwraca wysokość drzewa; fikcyjna wysokość wierzchołka za liściem wynosi -1. *)
let height = function
  | End -> -1
  | Node q -> q.height

let rec join p q =
  match p, q with
  | Node p, Node q ->
      (* W korzeniu wyniku musi być najmniejszy priorytet z drzew [p], [q].
         Zróbmy, że jest on w [p]. *)
      let p, q = if p.value <= q.value then p, q else q, p in
      (* Chcemy nowe drzewo z korzenia [p], lewego poddrzewa [p], i połączenia
         [q] z [p]. *)
      let x = p.left and y = join p.right (Node q) in
      (* W tym nowym drzewie zachowujemy warunek, że wysokość prawego poddrzewa
         nie jest większa niż lewego.*)
      let x, y = if height x >= height y then x, y else y, x in
      Node { p with left = x; right = y; height = height y + 1 }
  | Node _, _ -> p
  | _, _ -> q

let add x q = join (Node { left = End; value = x; right = End; height = 0 }) q

let delete_min = function
  | End -> raise Empty
  | Node q -> q.value, join q.left q.right
