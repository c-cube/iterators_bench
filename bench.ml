
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

module CPS = struct
  type (+'a, 'state) unfold = {
    unfold: 'r.
      'state ->
      on_done:(unit -> 'r) ->
      on_skip:('state -> 'r) ->
      on_yield:('state -> 'a -> 'r) ->
      'r
  }

  type +_ t = CPS : 'state * ('a, 'state) unfold -> 'a t

  let return x = CPS ((), {
    unfold=(fun () ~on_done:_ ~on_skip:_ ~on_yield -> on_yield () x)
  })

  let map f (CPS (st, u)) = CPS (st, {
    unfold=(fun st ~on_done ~on_skip ~on_yield ->
      u.unfold
        st
        ~on_done
        ~on_skip
        ~on_yield:(fun st x -> on_yield st (f x))
    );
  })

  let fold f acc (CPS (st,u)) =
    let rec loop st acc =
      u.unfold
        st
        ~on_done:(fun _ -> acc)
        ~on_skip:(fun _ -> acc)
        ~on_yield:(fun st' x -> let acc = f acc x in loop st' acc)
    in
    loop st acc

  let to_list_rev iter = fold (fun acc x -> x::acc) [] iter

  let of_list l = CPS (l, {
    unfold=(fun l ~on_done ~on_skip:_ ~on_yield -> match l with
      | [] -> on_done ()
      | x :: tail -> on_yield tail x
    );
  })

  let (--) i j = CPS (i, {
    unfold=(fun i ~on_done ~on_skip:_ ~on_yield ->
      if i>j then on_done ()
      else on_yield (i+1) i
    );
  })

  let filter f (CPS (st, u1)) =
    let rec u2 = {
      unfold=(fun st ~on_done ~on_skip ~on_yield ->
        u1.unfold st ~on_done ~on_skip
          ~on_yield:(fun st' x ->
            if f x then on_yield st' x
            else u2.unfold st' ~on_done ~on_skip ~on_yield
          )
      );
    } in
    CPS (st, u2)

  type ('a, 'b) run =
    | Start of 'a
    | Run of 'b
    | Stop

  let flat_map : type a b. (a -> b t) -> a t -> b t
  = fun f (CPS (st1, u1)) ->
    (* obtain next element of u1 *)
    let rec iter_main st1 ~on_done ~on_skip ~on_yield =
      u1.unfold st1
        ~on_done
        ~on_skip:(fun st1 -> iter_main st1 ~on_skip ~on_done ~on_yield)
        ~on_yield:(fun st1 x1 ->
          let sub2 = f x1 in
          iter_sub st1 sub2 ~on_done ~on_skip ~on_yield
        )
    (* iterate on sub-sequence *)
    and iter_sub st1 (CPS (sub_st2, sub2)) ~on_done ~on_skip ~on_yield =
      sub2.unfold sub_st2
        ~on_done:(fun () -> iter_main st1 ~on_done ~on_skip ~on_yield)
        ~on_skip:(fun sub_st2 -> iter_sub st1 (CPS (sub_st2, sub2)) ~on_done ~on_skip ~on_yield)
        ~on_yield:(fun sub_st2 x2 -> on_yield (Run (st1, CPS (sub_st2, sub2))) x2)
    in
    CPS (Start st1, {
      unfold=(fun st1 ~on_done ~on_skip ~on_yield -> match st1 with
        | Stop -> on_done ()
        | Start st1 -> iter_main st1 ~on_done ~on_skip ~on_yield
        | Run (st1, sub2) ->
          iter_sub st1 sub2 ~on_done ~on_skip ~on_yield
      );
    })
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

let f_cps () =
  let open CPS in
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
    ; "cps", Sys.opaque_identity f_cps, ()
    ; "sequence", Sys.opaque_identity f_seq, ()
    ]
  in
  Benchmark.tabulate res

(* ocamlfind opt -O3 -package gen -package sequence -package benchmark -linkpkg -unbox-closures -inline-call-cost 200 truc.ml -o truc *)
