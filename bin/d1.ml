open Base

let f_1_1 = fun () ->
  let text = In_channel.input_all @@ In_channel.open_text "input_1" in
  let table =
      List.map  ~f:(fun x ->  List.filter ~f:(fun y -> not @@ String.equal y  "") (String.split ~on:' ' x) )
      (String.split_lines text)
  in
  let parsed = List.map ~f:(List.map ~f:(Int.of_string)) table in
  let t2 = Option.value_exn  @@ List.transpose parsed in
  let t3 = List.map ~f:Int.(List.sort ~compare) t2 in
  let t4 = List.zip_exn (Option.value_exn @@ List.nth t3 0) (Option.value_exn @@ List.nth t3 1) in
  let t5 = List.map ~f:(fun (a,b) -> max (a-b) (b-a)) t4 in
  Stdio.print_endline @@ Int.to_string @@ Option.value_exn @@ List.reduce ~f:(+) t5

let f_1_2 = fun () ->
  let text = In_channel.input_all @@ In_channel.open_text "input_1" in
  let table =
      List.map  ~f:(fun x ->  List.filter ~f:(fun y -> not @@ String.equal y  "") (String.split ~on:' ' x) )
      (String.split_lines text)
  in
  let parsed = List.map ~f:(List.map ~f:(Int.of_string)) table in
  let t2 = Option.value_exn  @@ List.transpose parsed in
  let t3 = List.map ~f:Int.(List.sort ~compare) t2 in
  let l = Option.value_exn @@ List.nth t3 0 in
  let r = Option.value_exn @@ List.nth t3 1 in
  let count_list = fun xs ->
      List.map ~f:(fun x -> (List.hd_exn x,List.length x))
      (List.sort_and_group ~compare:Int.compare xs)
  in
  let l2 = count_list l in
  let r2 = count_list r in
  let score = List.map ~f:(fun (x,n) ->
      x * n * (Option.value ~default:0 @@ Option.map ~f:snd @@ List.find r2 ~f:(fun (y,_) -> phys_equal x y))
    ) l2
  in
  Stdio.print_endline @@ Int.to_string @@ Option.value_exn @@ List.reduce ~f:(+) score

let () = f_1_1 ()
let () = f_1_2 ()
