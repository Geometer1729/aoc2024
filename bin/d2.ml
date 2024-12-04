open Base


let rec inc xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x::y::rest -> y>x && y-x <= 3 && inc (y::rest)


let rec dec xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x::y::rest -> y<x && x-y <= 3 && dec (y::rest)


let f_2_1 = fun () ->
  let text = In_channel.input_all @@ In_channel.open_text "input_2" in
  let table =
      List.map  ~f:(fun x ->  List.filter ~f:(fun y -> not @@ String.equal y  "") (String.split ~on:' ' x) )
      (String.split_lines text)
  in
  let parsed : int list list = List.map ~f:(List.map ~f:(Int.of_string)) table in
  let safe = parsed
      |> List.filter ~f:(fun xs -> dec xs || inc xs)
      |> List.length
  in
  Stdio.print_endline @@ Int.to_string safe

let rec sorta_inc_2 xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x::y::rest -> (y>x && y-x <= 3 && sorta_inc_2 (y::rest)) || (inc (x::rest))

let sorta_inc xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x::rest -> sorta_inc_2 (x::rest) || (inc rest)

let rec sorta_dec_2 xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x::y::rest -> (x>y && x-y <= 3 && sorta_dec_2 (y::rest)) || (dec (x::rest))

let sorta_dec xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x::rest -> sorta_dec_2 (x::rest) || (dec rest)

let f_2_2 = fun () ->
  let text = In_channel.input_all @@ In_channel.open_text "input_2" in
  let table =
      List.map  ~f:(fun x ->  List.filter ~f:(fun y -> not @@ String.equal y  "") (String.split ~on:' ' x) )
      (String.split_lines text)
  in
  let parsed : int list list = List.map ~f:(List.map ~f:(Int.of_string)) table in
  let safe = parsed
      |> List.filter ~f:(fun xs -> sorta_dec xs || sorta_inc xs)
      |> List.length
  in
  Stdio.print_endline @@ Int.to_string safe

let () = f_2_1 ()
let () = f_2_2 ()
