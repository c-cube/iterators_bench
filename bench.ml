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

module G_exn = struct
  exception End

  type 'a t = unit -> 'a
  (** @raise End when gen is exhausted *)

  let (--) i j =
    let r = ref i in
    fun () ->
      if !r > j then raise End
      else (let x = !r in incr r; x)

  let map f g =
    fun () ->
      let x = g() in
      f x

  let rec filter f g () =
    let x = g() in
    if f x then x else filter f g ()

  type 'a state =
    | Start
    | Cur of 'a
    | Stop

  let flat_map f g =
    let state = ref Start in
    let rec aux () = match !state with
      | Start -> next_gen(); aux ()
      | Stop -> raise End
      | Cur g' ->
        match g'() with
        | x -> x
        | exception End -> next_gen(); aux ()
    and next_gen() = match g() with
      | x -> state := Cur (f x)
      | exception e -> state := Stop; raise e
    in
    aux

  let rec fold f acc g = match g () with
    | x -> fold f (f acc x) g
    | exception End -> acc
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

module UnCons = struct
  type +'a t = T : {state: 'st; next: 'st -> ('a * 'st) option} -> 'a t

  let empty = T { state=(); next=(fun _ -> None) }

  let (--) i j =
    let next i = if i>j then None else Some (i, i+1) in
    T {state=i; next}

  let map f (T {state; next}) =
    let next' s = match next s with
      | None -> None
      | Some (x, s') -> Some (f x, s')
    in
    T {state; next=next'}

  let filter f (T {state; next}) =
    let rec next' s = match next s with
      | None -> None
      | Some (x, _) as res when f x -> res
      | Some (_, s') -> next' s'
    in
    T {state; next=next'}

  type ('a, 'b, 'top_st) flat_map_state =
      FMS :
        { top: 'top_st;
          sub: 'sub_st;
          sub_next: 'sub_st -> ('b * 'sub_st) option
        } -> ('a, 'b, 'top_st) flat_map_state

  let flat_map f (T {state; next}) =
    let rec next' (FMS { top; sub; sub_next}) =
      match sub_next sub with
      | None ->
        begin match next top with
          | None -> None
          | Some (x, top') ->
            let T{next=sub_next; state=sub} = f x in
            next' (FMS { top=top'; sub; sub_next; })
        end
      | Some (x, sub') ->
        Some (x, FMS {top; sub=sub'; sub_next})
    in
    T {
      state=FMS {top=state; sub=(); sub_next=(fun _ -> None) };
      next=next';
    }

  let fold f acc (T{state; next}) =
    let rec aux f acc state = match next state with
      | None -> acc
      | Some (x, state') ->
        let acc = f acc x in
        aux f acc state'
    in
    aux f acc state
end

module Co = struct
  type 'a kont =
    | K_nil
    | K_return of 'a * 'a kont
    | K_tailcall of 'a t * 'a kont
    | K_map : {
        f: 'a -> 'b;
        from: 'a kont; (* used to generate new ['a] *)
        k: 'b kont;
      } -> 'b kont
    | K_bind : {
        f: 'a -> 'b t;
        from: 'a kont; (* used to generate new ['a] *)
        k: 'b kont;
      } -> 'b kont
    | K_list of {
        l: 'a list;
        k: 'a kont;
      }

  (* yield some ['a] by adding them to the continuation stack *)
  and 'a t = 'a kont -> 'a kont

  let[@inline] nil k = k

  let[@inline] append (x:'a t) (y:'a t) : 'a t =
    fun k-> x (y k)

  let[@inline] return (x:'a) : 'a t = fun k -> K_return (x, k)

  let[@inline] yield (x:'a) (co:'a t) : 'a t = fun k -> K_return (x, co k)

  let[@inline] flat_map (f: 'a -> 'b t) (x:'a t) : 'b t =
    fun k -> K_bind {f; from=x K_nil; k}

  let[@inline] map (f: 'a -> 'b) (x:'a t) : 'b t =
    function
    | K_map r -> K_map {r with f=(fun x -> f (r.f x))} (* compose *)
    | k -> K_map {f;from=x K_nil;k}

  let[@inline] filter (f:'a -> bool) (x:'a t) : 'a t =
    flat_map (fun x -> if f x then return x else nil) x

  let[@inline] of_list  l : _ t = fun k -> K_list {l;k}

  let (--) i j : int t =
    let rec aux r k =
      if r > j then k
      else yield r (aux (r+1)) k
    in aux i

  (* generator *)
  let rec next_k
    : type a. a kont -> a kont * a option
    = fun k -> match k with
      | K_nil -> K_nil, None
      | K_tailcall (co,k) -> next_k (co k)
      | K_return (x, k') -> k', Some x
      | K_map ({f; from; _} as r) ->
        begin match next_k from with
          | _, None -> next_k r.k
          | from', Some x -> K_map {r with from=from'}, Some (f x)
        end
      | K_bind ({f; from; _} as r) ->
        begin match next_k from with
          | _, None -> next_k r.k
          | from', Some x ->
            next_k (f x @@ K_bind {r with from=from'})
        end
      | K_list r ->
        begin match r.l with
          | [] -> next_k r.k
          | x :: tl -> K_list {r with l=tl}, Some x
        end

  let fold f acc (l:_ t) =
    let rec aux f acc (k:_ kont) =
      match next_k k with
      | _, None -> acc
      | k', Some x -> aux f (f acc x) k'
    in
    aux f acc (l K_nil)
end

(* the "Stdlib_Seq" library *)
let f_std_seq () =
  let open Seq in 

  let init n f =
    let rec aux i () =
      if i = n then
        Nil
      else
        Cons(f i, aux (i + 1))
    in
    if n < 0 then
      invalid_arg "Stdlib_Seq.init"
    else
      aux 0

  in
  
  let ( -- ) a b =
    if b < a then
      empty
    else
      init (b - a + 1) (fun x -> a + x)

  in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold_left (+) 0

(* the "BatSeq" library *)
let f_batseq () =
  let open BatSeq in
  let rec flat_map f seq () = match seq () with
    | Nil -> Nil
    | Cons (x, next) ->
      flat_map_app f (f x) next ()

  (* this is [append seq (flat_map f tail)] *)
  and flat_map_app f seq tail () = match seq () with
    | Nil -> flat_map f tail ()
    | Cons (x, next) ->
      Cons (x, flat_map_app f next tail)
  in 
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold_left (+) 0

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

let f_g_exn () =
  let open G_exn in
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

let f_uncons () =
  let open UnCons in
  1 -- 100_000
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 2 = 0)
  |> flat_map (fun x -> x -- (x+30))
  |> fold (+) 0

let f_co () =
  let open Co in
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

(* Base library *)
let f_base () =
  let open Base.Sequence in
  range ~start:`inclusive ~stop:`inclusive 1 100_000
  |> map ~f:(fun x -> x+1)
  |> filter ~f:(fun x -> x mod 2 = 0)
  |> concat_map ~f:(fun x -> range ~start:`inclusive ~stop:`inclusive x (x+30))
  |> fold ~f:(+) ~init:0

let () =
  assert (f_gen_noptim () = f_gen());
  assert (f_g () = f_gen());
  assert (f_g_exn () = f_gen());
  assert (f_seq () = f_gen());
  assert (f_core () = f_gen());
  assert (f_base () = f_gen());
  assert (f_fold () = f_gen());
  assert (f_uncons () = f_gen());
  assert (f_co () = f_gen());
  assert (f_std_seq () = f_gen());
  ()

let () =
  let res =
    (Sys.opaque_identity Benchmark.throughputN) ~repeat:2 3
      [ "gen", Sys.opaque_identity f_gen, ()
      ; "gen_no_optim", Sys.opaque_identity f_gen_noptim, ()
      ; "g", Sys.opaque_identity f_g, ()
      ; "g_exn", Sys.opaque_identity f_g_exn, ()
      ; "core.sequence", Sys.opaque_identity f_core, ()
      ; "base.sequence", Sys.opaque_identity f_base, ()
      ; "cps", Sys.opaque_identity f_cps, ()
      ; "cps2", Sys.opaque_identity f_cps2, ()
      ; "fold", Sys.opaque_identity f_fold, ()
      ; "sequence", Sys.opaque_identity f_seq, ()
      ; "list", Sys.opaque_identity f_list, ()
      ; "lazy_list", Sys.opaque_identity f_llist, ()
      ; "ulist", Sys.opaque_identity f_ulist, ()
      ; "uncons", Sys.opaque_identity f_uncons, ()
      ; "coroutine", Sys.opaque_identity f_co, ()
      ; "batseq", Sys.opaque_identity f_batseq, ()
      ; "std_seq", Sys.opaque_identity f_std_seq, ()
      ]
  in
  Benchmark.tabulate res

(* ocamlfind opt -O3 -unbox-closures -unbox-closures-factor 20 -package sequence -package gen -package core_kernel -package base -package batteries -package benchmark -package containers -linkpkg bench.ml -o bench.native *)
