open Base

let f_3_1 () =
  let text = In_channel.input_all @@ In_channel.open_text "input_3"
  in Str.full_split (Str.regexp "mul([0-9]*,[0-9]*)") text
    |> List.filter_map ~f:(fun  x ->
      (match x with
      | Str.Text _w  -> None
      | Str.Delim w  -> Some w
      ))
    |> List.map ~f:(fun w ->
      match Str.split (Str.regexp "[(,)]") w with
      | (_::l::r::_) -> (Int.of_string l,Int.of_string r)
      | _ -> Option.value_exn None (* unreachable I hope *)
                    )
    |> List.map ~f:(fun (x,y) -> x*y)
    |> List.fold ~init:0 ~f:(+)
    |> Stdio.printf "%d\n"

let f_3_2 () =
  let text = In_channel.input_all @@ In_channel.open_text "input_3"
  in Str.full_split (Str.regexp "mul([0-9]*,[0-9]*)\\|do()\\|don't()") text
    |> List.filter_map ~f:(fun  x ->
      (match x with
      | Str.Text _  -> None
      | Str.Delim w  -> Some w
      ))
    |> List.map ~f:(fun i ->
      match i with
      | "do()" -> Error true
      | "don't()" -> Error false
      | w ->
      match Str.split (Str.regexp "[(,)]") w with
      | (_::l::r::_) -> Ok (Int.of_string l,Int.of_string r)
      | _ -> Option.value_exn None (* unreachable I hope *)
                    )
    |> List.map ~f:(fun res ->
        match res with
        | Error t -> Error t
        | Ok (x,y) -> Ok (x*y)
      )
    |> List.fold ~init:(0,true) ~f:(fun (sum,enabled) instruction ->
        match instruction with
        | Error new_enabled -> (sum,new_enabled)
        | Ok r -> if enabled then (sum+r,enabled) else (sum,enabled)
        )
    |> fst
    |> Stdio.printf "%d\n"

let () = f_3_1 ()
let () = f_3_2 ()
