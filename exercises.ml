open Base

(* Tail of a List *)
let rec last = function
  | [] -> None
  | x :: [] -> Some x
  | _ :: xs -> last xs
;;

let input_1 = [ "a"; "b"; "c"; "d" ]

let%test_unit "Write a function that returns the last element of a list" =
  [%test_eq: string option] (last input_1) (Some "d")
;;

let%test_unit "Write a function that returns the last element of a list" =
  [%test_eq: string option] (last []) None
;;

(* Last Two Elements of a List *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: xs -> last_two xs
;;

let%test_unit "Find the last but one (last and penultimate) elements of a list" =
  [%test_eq: (string * string) option] (last_two input_1) (Some ("c", "d"))
;;

let%test_unit "Find the last but one (last and penultimate) elements of a list" =
  [%test_eq: (string * string) option] (last_two [ "a" ]) None
;;

(* N'th Element of a List *)
let rec nth l n =
  match l, n with
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: rest, n -> nth rest (n - 1)
;;

let input_2 = List.concat [ input_1; [ "e" ] ]

let%test_unit "Find the N'th element of a list" =
  [%test_eq: string option] (nth input_2 2) (Some "c")
;;

let%test_unit "Find the N'th element of a list" =
  [%test_eq: string option] (nth [ "a" ] 2) None
;;

(* Length of a List *)
let length l =
  let rec loop i = function
    | [] -> i
    | _ :: xs -> loop (i + 1) xs
  in
  loop 0 l
;;

let%test_unit "Find the number of elements of a list" =
  [%test_eq: int] (length [ "a"; "b"; "c" ]) 3
;;

let%test_unit "Find the number of elements of a list" = [%test_eq: int] (length []) 0

(* Reverse a List *)
let rev xs =
  let rec loop ys = function
    | [] -> ys
    | x :: xs -> loop (x :: ys) xs
  in
  loop [] xs
;;

let%test_unit "Reverse a list" =
  [%test_eq: string list] (rev [ "a"; "b"; "c" ]) [ "c"; "b"; "a" ]
;;

(* Palindrome *)
let is_palindrome xs =
  let rec eq xs ys =
    match xs, ys with
    | [], [] | _ :: _, [] | [], _ :: _ -> false
    | [ x ], [ y ] -> String.equal x y
    | x :: xs, y :: ys -> if String.equal x y then eq xs ys else false
  in
  eq xs (rev xs)
;;

let%test_unit "Find out whether a list is a palindrome" =
  [%test_eq: bool] (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]) true
;;

let%test_unit "Find out whether a list is a palindrome" =
  [%test_eq: bool] (is_palindrome [ "a"; "b" ]) false
;;

(* Flatten a List *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten nodes : string list =
  let rec loop res = function
    | [] -> res
    | One x :: nodes -> loop (x :: res) nodes
    | Many xs :: nodes -> loop (loop res xs) nodes
  in
  List.rev (loop [] nodes)
;;

let%test_unit "Flatten a nested list structure" =
  [%test_eq: string list]
    (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])
    [ "a"; "b"; "c"; "d"; "e" ]
;;

(* Eliminate Duplicates *)
let compress xs =
  let rec loop res = function
    | [] -> res
    | [ x ] -> x :: res
    | h1 :: (h2 :: _ as t) -> if phys_equal h1 h2 then loop res t else loop (h1 :: res) t
  in
  loop [] xs |> List.rev
;;

let%test_unit "Eliminate consecutive duplicates of list elements" =
  [%test_eq: string list]
    (compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

(* Pack Consecutive Duplicates *)
let pack xs : 'a list list =
  let rec loop xs ys zs =
    match xs, ys with
    | [], _ -> ys :: zs
    | x :: t, [] -> loop t [ x ] zs
    | x :: t, y :: _ ->
      if phys_equal x y then loop t (x :: ys) zs else loop t [ x ] (ys :: zs)
  in
  loop xs [] [] |> List.rev
;;

let%test_unit "Pack consecutive duplicates of list elements into sublists" =
  [%test_eq: string list list]
    (pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ])
    [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

(* Run-Length Encoding *)
let encode xs =
  let rec loop count acc = function
    | [] -> acc
    | [ x ] -> (count + 1, x) :: acc
    | x :: (y :: _ as t) ->
      if String.( = ) x y
      then loop (count + 1) acc t
      else loop 0 ((count + 1, x) :: acc) t
  in
  loop 0 [] xs |> List.rev
;;

let%test_unit "Run-Length Encoding" =
  [%test_eq: (int * string) list]
    (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
;;

(* Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
[@@deriving sexp, equal, compare]

let encode' xs : 'a rle list =
  let to_rle count char = if Int.( = ) count 1 then One char else Many (count, char) in
  let rec loop count acc = function
    | [] -> acc
    | [ x ] -> to_rle (count + 1) x :: acc
    | x :: (y :: _ as t) ->
      if String.( = ) x y
      then loop (count + 1) acc t
      else loop 0 (to_rle (count + 1) x :: acc) t
  in
  loop 0 [] xs |> List.rev
;;

let%test_unit "Modified Run-Length Encoding" =
  [%test_eq: string rle list]
    (encode' [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

(* Decode a Run-Length Encoded List *)
let decode xs =
  let rec repeat x res = function
    | 1 -> x :: res
    | n -> repeat x (x :: res) (n - 1)
  in
  let rec loop res = function
    | [] -> res
    | One x :: t -> loop (x :: res) t
    | Many (n, x) :: t -> loop (repeat x res n) t
  in
  loop [] xs |> List.rev
;;

let%test_unit "Given a run-length code list generated as specified in the previous \
               problem, construct its uncompressed version"
  =
  [%test_eq: string list]
    (decode
       [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ])
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

(* Run-Length Encoding of a List (Direct Solution) *)
let encode'' xs =
  let rle x = function
    | 1 -> One x
    | n -> Many (n, x)
  in
  let rec loop count res = function
    | [] -> res
    | [ x ] -> rle x (count + 1) :: res
    | x :: (y :: _ as t) ->
      if String.( = ) x y
      then loop (count + 1) res t
      else loop 0 (rle x (count + 1) :: res) t
  in
  loop 0 [] xs |> List.rev
;;

let%test_unit "Run-Length Encoding of a List (Direct Solution)" =
  [%test_eq: string rle list]
    (encode'' [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
    [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

(* Duplicate the Elements of a List *)
let duplicate xs =
  let rec loop ys = function
    | [] -> ys
    | x :: t -> loop (x :: x :: ys) t
  in
  loop [] xs |> List.rev
;;

let%test_unit "Replicate the elements of a list a given number of times" =
  [%test_eq: string list]
    (duplicate [ "a"; "b"; "c"; "c"; "d" ])
    [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
;;

(* Replicate the Elements of a List a Given Number of Times *)
let duplicate' xs n =
  let rec repeat x ys = function
    | 1 -> x :: ys
    | n -> repeat x (x :: ys) (n - 1)
  in
  let rec loop ys = function
    | [] -> ys
    | x :: t -> loop (repeat x ys n) t
  in
  loop [] xs |> List.rev
;;

let%test_unit "Replicate the elements of a list a given number of times" =
  [%test_eq: string list]
    (duplicate' [ "a"; "b"; "c" ] 3)
    [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
;;

(* Drop Every N'th Element From a List *)
let drop xs m =
  let rec loop n ys = function
    | [] -> ys
    | h :: t -> if n = 1 then loop m ys t else loop (n - 1) (h :: ys) t
  in
  loop m [] xs |> List.rev
;;

let%test_unit "Drop every N'th element from a list" =
  [%test_eq: string list]
    (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
;;

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split xs n =
  let rec loop i ys = function
    | [] -> List.rev ys, []
    | h :: t as xs -> if i = n then List.rev ys, xs else loop (i + 1) (h :: ys) t
  in
  loop 0 [] xs
;;

let%test_unit "Split a list into two parts; the length of the first part is given" =
  [%test_eq: string list * string list]
    (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
    ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
;;

let%test_unit "Split a list into two parts; the length of the first part is given" =
  [%test_eq: string list * string list]
    (split [ "a"; "b"; "c"; "d" ] 5)
    ([ "a"; "b"; "c"; "d" ], [])
;;

(* Extract a Slice From a List *)
let slice xs s e =
  (* let rec loop i k xs ys = *)
  (*   match xs, i, k with *)
  (*   | [], _, _ | _, 0, 1 -> ys *)
  (*   | h :: t, 0, k -> loop 0 (k - 1) t (h :: ys) *)
  (*   | _ :: t, i, k -> loop (i - 1) k t ys *)
  (* in *)
  (* loop s e xs [] |> List.rev *)
  let rec take n ys = function
    | [] -> ys
    | h :: t -> if n = 1 then ys else take (n - 1) (h :: ys) t
  in
  let rec drop n ys = function
    | [] -> ys
    | _ :: t -> if n = 1 then t else drop (n - 1) ys t
  in
  xs |> drop s [] |> take e [] |> List.rev
;;

let%test_unit "Extract a Slice From a List" =
  [%test_eq: string list]
    (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6)
    [ "c"; "d"; "e"; "f"; "g" ]
;;
