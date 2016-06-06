(* Trying to implment variable arity map in OCaml *)

let empty xs =
  match xs with
  | [] -> true
  | _ -> false

let rec ormap f xs =
  match xs with
  | [] -> true
  | x::xs -> (f x) || (ormap f xs)

(* Error *)
let rec apply f xs =
  match xs with
  | [] -> failwith "nope"
  | [x] -> f x
  | x::xs ->
    let f2 = f x in
    apply f2 xs

let rec mapN f xss =
  match xss with
  | [] -> []
  | _ ->
    if ormap empty xss
    then []
    else apply f (map List.hd xss) :: mapN f (map List.tl xss)

let _ =
  let a1 = mapN (fun x y -> x + y)
   [[1, 2, 3]
    [0, 4, 5]]
  in
  let a2 = mapN (fun x y z -> x + y + z)
   [[1, 2, 3]
    [9, 4, 2]
    [0, 4, 5]]
  in
  ()
