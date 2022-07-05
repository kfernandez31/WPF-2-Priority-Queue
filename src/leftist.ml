(*
    Autor:     Kacper Kramarz-Fernandez (indeks: 429629, gr. IV)
    Reviewer:  ??? (gr. IV)
    Zadanie 2: Drzewa lewicowe
*)

(** Typ złączalnej kolejki priorytetowej: null lub węzeł z dwoma krawędziami i npl 
    (dł. ścieżki do najbliższego null'a) 
*)
type 'a queue = 
| EmptyQueue
| Node of ('a queue) * 'a * ('a queue) * int
;;

(** Pusta kolejka priorytetowa *)
let empty = EmptyQueue
;;

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty
;;

(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join q1 q2 =
    match q1, q2 with
    | EmptyQueue, t | t, EmptyQueue          -> t
    | Node(l1,x1,r1, npl1), Node(_,x2,r2, _) -> 
        if (x1 > x2) then
            join q2 q1
        else
            let merge = join r1 q2 in
            match l1, merge with
            | EmptyQueue, t | t, EmptyQueue                 -> Node(t,x1,EmptyQueue, 0)
            | Node(_,_,_, left_npl), Node(_,_,_, merge_npl) ->
                if (left_npl <= merge_npl) then
                    Node(merge, x1, l1, left_npl + 1)
                else
                    Node(l1, x1, merge, merge_npl + 1)
;;

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *) 
let add e q = 
    join q (Node(EmptyQueue, e, EmptyQueue, 0))
;;

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q = 
    match q with
    | EmptyQueue       -> raise Empty
    | Node(l,x,r, npl) -> (x, join l r)
;;

(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty q =
    q = empty
;;
