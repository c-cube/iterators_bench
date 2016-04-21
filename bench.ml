
module G = struct
  type 'a t = unit -> 'a option

  let (--) i j =
    let r = ref i in
    fun () ->
      if !r > j then None
      else (let x = !r in incr r; Some x)

  let map f g =
    fun () -> match g() with
      | None -> None
      | Some x -> Some (f x)

  let rec filter f g () = match g() with
    | None -> None
    | Some x when f x -> Some x
    | Some _ -> filter f g ()

  type 'a state =
    | Start
    | Cur of 'a
    | Stop

  let flat_map f g =
    let state = ref Start in
    let rec aux () = match !state with
      | Start -> next_gen(); aux ()
      | Stop -> None
      | Cur g' ->
        match g'() with
          | None -> next_gen(); aux ()
          | Some _ as res -> res
    and next_gen() = match g() with
      | None -> state := Stop
      | Some x -> state := Cur (f x)
      | exception e -> state := Stop; raise e
    in
    aux

  let rec fold f acc g = match g () with
    | None -> acc
    | Some x -> fold f (f acc x) g
end

let f_gen () =
  let open Gen in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold (+) 0

let f_gen_noptim () =
  let open Gen in
  1 -- 100_000
  |> Sys.opaque_identity map (fun x -> x+1)
  |> Sys.opaque_identity filter (fun x -> x mod 2 = 0)
  |> Sys.opaque_identity flat_map (fun x -> x -- (x+30))
  |> Sys.opaque_identity fold (+) 0

let f_g () =
  let open G in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold (+) 0

let f_seq () =
  let open Sequence in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold (+) 0

let f_core () =
  let open Core_kernel.Sequence in
  range ~start:`inclusive ~stop:`inclusive 1 100_000
  |> map ~f:(fun x -> x+1)
  |> filter ~f:(fun x -> x mod 2 = 0)
  |> concat_map ~f:(fun x -> range ~start:`inclusive ~stop:`inclusive x (x+30))
  |> fold ~f:(+) ~init:0

let () =
  assert (f_gen_noptim () = f_gen());
  assert (f_g () = f_gen());
  assert (f_seq () = f_gen());
  assert (f_core () = f_gen());
  ()

let () =
  let res =
    (Sys.opaque_identity Benchmark.throughputN) ~repeat:2 3
    [ "gen", Sys.opaque_identity f_gen, ()
    ; "gen_no_optim", Sys.opaque_identity f_gen_noptim, ()
    ; "g", Sys.opaque_identity f_g, ()
    ; "core.sequence", Sys.opaque_identity f_core, ()
    ; "sequence", Sys.opaque_identity f_seq, ()
    ]
  in
  Benchmark.tabulate res

(* ocamlfind opt -O3 -package gen -package sequence -package benchmark -linkpkg -unbox-closures -inline-call-cost 200 truc.ml -o truc *)
