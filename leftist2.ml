let print_queue q =
  let rec f q indent =
    match q with
    | End -> "#"
    | Node q ->
        let label = Printf.sprintf " %+4F (%i) " q.value q.height in
        let indent = indent ^ String.make (String.length label + 1) ' ' in
        Printf.sprintf "%s─┬─%s\n%s└─%s" label
          (f q.left (indent ^ "│ "))
          indent
          (f q.right (indent ^ "  "))
  in
  print_endline (f q "")

(** Jeśli asserty są włączone, przechodzi po drzewie i patrzy czy struktura
    danych jest ok. *)
let rec assert_invariants q =
  assert (
    (match q with
    | End -> ()
    | Node q ->
        Printf.printf "%i\n" q.height;
        assert (height (Node q) = height q.right + 1);
        assert (height q.left >= height q.right);
        List.iter
          (function
            | End -> ()
            | Node x -> assert (q.value <= x.value))
          [ q.left; q.right ];
        assert_invariants q.left;
        assert_invariants q.right);
    true)