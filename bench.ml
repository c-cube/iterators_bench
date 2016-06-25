
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

  let empty = CPS ((), {unfold=(fun () ~on_done ~on_skip:_ ~on_yield:_ -> on_done ())})

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
        ~on_skip:(fun st' -> loop st' acc)
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
    CPS (st, {
      unfold=(fun st ~on_done ~on_skip ~on_yield ->
        u1.unfold st ~on_done ~on_skip
          ~on_yield:(fun st' x ->
            if f x then on_yield st' x
            else on_skip st'
          )
      );
    })

  let flat_map : type a b. (a -> b t) -> a t -> b t
  = fun f (CPS (st1, u1)) ->
    (* obtain next element of u1 *)
    let u = {
      unfold=(fun (st1, CPS (sub_st2, sub2))
          ~on_done ~on_skip ~on_yield ->
        let done_ () =
          u1.unfold st1
            ~on_done
            ~on_skip:(fun st1' -> on_skip (st1', empty))
            ~on_yield:(fun st1' x1 -> on_skip (st1', f x1))
        in
        let skip sub_st2 = on_skip (st1, CPS (sub_st2, sub2)) in
        let yield_ sub_st2 x2 = on_yield (st1, CPS (sub_st2,sub2)) x2 in
        sub2.unfold sub_st2 ~on_done:done_ ~on_skip:skip ~on_yield:yield_
      );
    }
    in
    CPS ((st1, empty), u)
end

module CPS2 = struct
  type +'a t =
    | CPS : { state : 's
            ; unfold : 'r.
                         's
                -> on_done:(unit -> 'r)
                -> on_skip:('s -> 'r)
                -> on_yield:('s -> 'a -> 'r)
                -> 'r
            } -> 'a t

  let empty =
    CPS { state = (); unfold=(fun () ~on_done ~on_skip:_ ~on_yield:_ -> on_done ()) }

  let return x =
    CPS { state = ();
          unfold=(fun () ~on_done:_ ~on_skip:_ ~on_yield -> on_yield () x)
        }

  let map f (CPS { state = st; unfold = u }) =
    CPS { state = st;
          unfold=(fun st ~on_done ~on_skip ~on_yield ->
            u
              st
              ~on_done
              ~on_skip
              ~on_yield:(fun st x -> on_yield st (f x))
          );
        }

  let fold f acc (CPS { state = st; unfold = u }) =
    let rec loop st acc =
      u
        st
        ~on_done:(fun _ -> acc)
        ~on_skip:(fun st' -> loop st' acc)
        ~on_yield:(fun st' x -> let acc = f acc x in loop st' acc)
    in
    loop st acc

  let to_list_rev iter = fold (fun acc x -> x::acc) [] iter

  let of_list l = CPS { state = l;
                        unfold=(fun l ~on_done ~on_skip:_ ~on_yield -> match l with
                          | [] -> on_done ()
                          | x :: tail -> on_yield tail x
                        );
                      }

  let (--) i j = CPS { state = i;
                       unfold=(fun i ~on_done ~on_skip:_ ~on_yield ->
                         if i>j then on_done ()
                         else on_yield (i+1) i
                       );
                     }

  let filter f (CPS { state = st; unfold = u1 }) =
    CPS { state = st;
          unfold=(fun st ~on_done ~on_skip ~on_yield ->
            u1 st ~on_done ~on_skip
              ~on_yield:(fun st' x ->
                if f x then on_yield st' x
                else on_skip st'
              )
          );
        }

  let flat_map : type a b. (a -> b t) -> a t -> b t
    = fun f (CPS { state = st1; unfold = u1 }) ->
      (* obtain next element of u1 *)
      let u = (fun (st1, CPS { state = sub_st2; unfold = sub2 })
                ~on_done ~on_skip ~on_yield ->
                let done_ () =
                  u1 st1
                    ~on_done
                    ~on_skip:(fun st1' -> on_skip (st1', empty))
                    ~on_yield:(fun st1' x1 -> on_skip (st1', f x1))
                in
                let skip sub_st2 = on_skip (st1, CPS { state = sub_st2; unfold = sub2 }) in
                let yield_ sub_st2 x2 =
                  on_yield (st1, CPS { state = sub_st2; unfold = sub2 }) x2
                in
                sub2 sub_st2 ~on_done:done_ ~on_skip:skip ~on_yield:yield_
              );
      in
      CPS { state = (st1, empty); unfold = u }
end

module Fold = struct
  type _ t =
    | Fold : {fold: 'b. 's -> init:'b -> f:('b -> 'a -> 'b) -> 'b;
              s: 's } -> 'a t

  let map (Fold{fold;s}) ~f:m =
    let fold s ~init ~f =
      fold s ~init ~f:(fun b a -> f b (m a))
    in
    Fold{fold;s}

  let filter (Fold{fold;s}) ~f:p =
    let fold s ~init ~f =
      fold s ~init ~f:(fun b a -> if p a then  f b a else b)
    in
    Fold{fold;s}

  let fold (Fold{fold;s}) = fold s

  let flat_map (Fold{fold=fold1;s=s1}) ~f:m =
    let fold _s2 ~init ~f =
      fold1 s1
        ~init
        ~f:(fun acc x1 ->
          let (Fold{fold=fold2;s=s2}) = m x1 in
          fold2 s2 ~init:acc ~f)
    in
    Fold {fold; s=s1}

  let (--) i j =
    let rec fold s ~init ~f =
      if s>j then init
      else fold (s+1) ~init:(f init s) ~f
    in
    Fold {fold; s=i}
end

module LList = struct
  type 'a t = 'a node lazy_t
  and 'a node = Nil | Cons of 'a * 'a t

  let rec (--) i j =
    lazy (
      if i>j then  Nil
      else Cons (i, i+1 -- j)
    )

  let rec map f l =
    lazy (match l with
      | lazy Nil -> Nil
      | lazy (Cons (x,tail)) -> Cons (f x, map f tail)
    )

  let filter f l =
    let rec aux f l = match l with
      | lazy Nil -> Nil
      | lazy (Cons (x,tl)) when f x -> Cons (x, lazy (aux f tl))
      | lazy (Cons (_, tl)) ->  aux f tl
    in
    lazy (aux f l)

  let rec append a b =
    lazy (
      match a with
        | lazy Nil -> Lazy.force b
        | lazy (Cons (x,tl)) -> Cons (x, append tl b)
    )

  let rec flat_map f l =
    lazy (
      match l with
        | lazy Nil -> Nil
        | lazy (Cons (x,tl)) ->
          let res = append (f x) (flat_map f tl) in
          Lazy.force res
    )

  let rec fold f acc = function
    | lazy Nil -> acc
    | lazy (Cons (x,tl)) -> fold f (f acc x) tl
end

module UList = struct
  type 'a t = unit -> 'a node
  and 'a node = Nil | Cons of 'a * 'a t

  let rec (--) i j () =
    if i>j then Nil
    else Cons (i, i+1 -- j)

  let rec map f l () =
    match l () with
      | Nil -> Nil
      | Cons (x,tail) -> Cons (f x, map f tail)

  let filter f l =
    let rec aux f l () = match l () with
      | Nil -> Nil
      | Cons (x,tl) when f x -> Cons (x, aux f tl)
      | Cons (_, tl) -> aux f tl ()
    in
    aux f l

  let rec append a b () =
    match a () with
      | Nil -> b ()
      | Cons (x,tl) -> Cons (x, append tl b)

  let rec flat_map f l () =
    match l() with
      | Nil -> Nil
      | Cons (x,tl) ->
        let res = append (f x) (flat_map f tl) in
        res ()

  let rec fold f acc l = match l() with
    | Nil -> acc
    | Cons (x,tl) -> fold f (f acc x) tl
end

(* the "gen" library *)
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

(* sequence library *)
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

let f_cps2 () =
  let open CPS2 in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold (+) 0

let f_fold () =
  let open Fold in
  1 -- 100_000
  |> map ~f:(fun x -> x +1)
  |> filter ~f:(fun i -> i mod 2 = 0)
  |> flat_map ~f:(fun x -> x -- (x+30))
  |> fold ~init:0 ~f:(+)

let f_list () =
  let open CCList in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> List.fold_left (+) 0

let f_llist () =
  let open LList in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold (+) 0

let f_ulist () =
  let open UList in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold (+) 0

(* Core library *)
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
  assert (f_fold () = f_gen());
  ()

let () =
  let res =
    (Sys.opaque_identity Benchmark.throughputN) ~repeat:2 3
    [ "gen", Sys.opaque_identity f_gen, ()
    ; "gen_no_optim", Sys.opaque_identity f_gen_noptim, ()
    ; "g", Sys.opaque_identity f_g, ()
    ; "core.sequence", Sys.opaque_identity f_core, ()
    ; "cps", Sys.opaque_identity f_cps, ()
    ; "cps2", Sys.opaque_identity f_cps2, ()
    ; "fold", Sys.opaque_identity f_fold, ()
    ; "sequence", Sys.opaque_identity f_seq, ()
    ; "list", Sys.opaque_identity f_list, ()
    ; "lazy_list", Sys.opaque_identity f_llist, ()
    ; "ulist", Sys.opaque_identity f_ulist, ()
    ]
  in
  Benchmark.tabulate res

(* ocamlfind opt -O3 -package gen -package sequence -package benchmark -linkpkg -unbox-closures -inline-call-cost 200 truc.ml -o truc *)
