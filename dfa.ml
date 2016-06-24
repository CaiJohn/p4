open Bvset
open Disassembler

type dir_type =
  | D_Forward
  | D_Backward

type may_must_type =
  | K_May
  | K_Must

(* Do not change the following type *)
type 'a dfa =
    {
      instrs : lua_ops array;
      dir : dir_type; (* direction *)
      may_must : may_must_type; (* may or must *)
      gen_kill : int -> ('a bvset * 'a bvset); (* (gen, kill) *)
      entry_or_exit_facts : 'a bvset; (* facts assumed at program entry (fwd analysis) or exit (bkwd analysis) *)
      top : 'a bvset; (* initial sets of facts at all other program points *)
    }

      
(* let my_dump_res f r = *)
(*   let rec output_set o = function *)
(*     | [] -> () *)
(*     | [x] -> f o x *)
(*     | x::xs -> Printf.fprintf o "%a, %a" f x output_set xs *)
(*   in *)
(*     Array.iteri (fun i s -> Printf.printf "%d: {%a}\n" i output_set (Bvset.mems s)) r *)

(* ;; *)

let string_of_list lst pr =
  List.fold_left (fun r item -> r^" "^(pr item)) "" lst
;;
  
(**************************************************************************)

type kind_uop = U_Minus | U_Not
type kind_binop = B_Add | B_Sub | B_Mul | B_Div | B_Mod | B_Pow | B_Test
type expr =
  | Uop of kind_uop * rk_source
  | Binop of kind_binop * rk_source * rk_source

let output_rk_source o = function
  | `L_String s -> Printf.fprintf o "\"%s\"" s
  | `L_Bool b -> Printf.fprintf o "%b" b
  | `L_Double d -> Printf.fprintf o "%f" d
  | `L_Nill -> Printf.fprintf o "nil"
  | `Register r -> Printf.fprintf o "R(%d)" r

let output_kind_uop o = function
  | U_Minus -> Printf.fprintf o "-"
  | U_Not -> Printf.fprintf o "!"

let output_kind_binop o = function
  | B_Add -> Printf.fprintf o "+"
  | B_Sub -> Printf.fprintf o "-"
  | B_Mul -> Printf.fprintf o "*"
  | B_Div -> Printf.fprintf o "/"
  | B_Mod -> Printf.fprintf o "%%"
  | B_Pow -> Printf.fprintf o "^"
  | B_Test -> Printf.fprintf o "!="

let output_expr o = function
  | Uop (u, s) -> Printf.fprintf o "%a%a" output_kind_uop u output_rk_source s
  | Binop (b, s1, s2) -> Printf.fprintf o "%a %a %a" output_rk_source s1 output_kind_binop b output_rk_source s2

(* Edit the following four definitions as appropraite *)

let reach_defs (instrs:lua_ops array):int dfa =
  let bvs = mkempty
              (let rec full index len res=
                 if index<len
                 then full (index+1) len (index::res)
                 else res
               in
               full 0 (Array.length instrs) [])
  in
  {
    instrs = instrs;
    dir = D_Forward;
    may_must = K_May;
    gen_kill =
      (
        let collect_def reg instrs=
          (* collect all the definition line number *)
          let rec helper index len res=
            if index<len
            then
              (match Array.get instrs index with
               | Load_Const (dst,_) ->
                  if dst = reg then helper (index+1) len (insert index res) else helper (index+1) len res
               | Unary_Minus (dst,_) ->
                  if dst = reg then helper (index+1) len (insert index res) else helper (index+1) len res
               | Unary_Not (dst,_)  ->
                  if dst = reg then helper (index+1) len (insert index res) else helper (index+1) len res
               | Test_Set (dst,_,_) ->
                  if dst = reg then helper (index+1) len (insert index res) else helper (index+1) len res
               | Move (dst,_) ->
                  if dst = reg then helper (index+1) len (insert index res) else helper (index+1) len res
               | Arith aop  ->
                  if aop.dest = reg then helper (index+1) len (insert index res) else helper (index+1) len res
               | _ -> helper (index+1) len res)
            else
              res
          in
          helper 0 (Array.length instrs) bvs
        in
        let collect_reg instrs=
          (* Collect the set of registers in the input instructions *)
          Array.fold_left
            (fun res elem ->
              match elem with
              | Load_Const (dst,_) | Unary_Minus (dst,_)
                | Unary_Not (dst,_) | Test_Set (dst,_,_)
                | Move (dst,_) ->
                 if List.exists (fun d -> d=dst) res
                 then res
                 else dst::res
              | Arith aop  ->
                 if List.exists (fun d -> d=aop.dest) res
                 then res
                 else aop.dest::res
              | _ -> res
            ) [] instrs
        in
        let reg_def =
          List.fold_left
            (fun res reg ->
              (reg, collect_def reg instrs)::res)
            []
            (collect_reg instrs)
        in
        let find_reg_def ref_def reg=
          let (_,def) =
            List.find
              (fun (nreg,ndef) -> nreg = reg)
              ref_def
          in
          def
        in
        (* The body of gen and kill *)
        (fun index ->
          match Array.get instrs index with
          | Load_Const (dst,_) ->
             ((insert index bvs),(remove index (find_reg_def reg_def dst)))
          | Load_Nil (f,t) ->
             if f=t
             then ((insert f bvs),(remove f (find_reg_def reg_def f)))
             else failwith "Load_Nil: write to multiple registers"
          | Load_Bool (dst,_,_) ->
             ((insert index bvs),(remove index (find_reg_def reg_def dst)))
          | Unary_Minus (dst,_) ->
             ((insert index bvs),(remove index (find_reg_def reg_def dst)))
          | Unary_Not (dst,_) ->
             ((insert index bvs),(remove index (find_reg_def reg_def dst)))
          | Jump _ ->
             (bvs,bvs)
          | Cmp _ ->
             (bvs,bvs)
          | Test _ ->
             (bvs,bvs)
          | Test_Set (dst,_,_) ->
             ((insert index bvs),bvs)
          | Move (dst,_) ->
             ((insert index bvs),(remove index (find_reg_def reg_def dst)))
          | Arith aop->
             let dst = aop.dest in
             ((insert index bvs),(remove index (find_reg_def reg_def dst)))
          | Return _ ->
             (bvs, bvs)
          | _ ->
             failwith ((string_of_int index)^" instruction not defined")
        )
      );
      entry_or_exit_facts = bvs;
      top = bvs;
  }

let avail_exprs (instrs:lua_ops array):expr dfa =
  let bvs = mkempty [] in
    {
      instrs = instrs;
      dir = D_Forward;
      may_must = K_Must;
      gen_kill = (fun _ -> (bvs, bvs));
      entry_or_exit_facts = bvs;
      top = bvs;
    }

let live_vars (instrs:lua_ops array):int dfa =
  let bvs = mkempty [] in
    {
      instrs = instrs;
      dir = D_Backward;
      may_must = K_May;
      gen_kill = (fun _ -> (bvs, bvs));
      entry_or_exit_facts = bvs;
      top = bvs;
    }

let very_busy_exprs (instrs:lua_ops array):expr dfa =
  let bvs = mkempty [] in
    {
      instrs = instrs;
      dir = D_Backward;
      may_must = K_Must;
      gen_kill = (fun _ -> (bvs, bvs));
      entry_or_exit_facts = bvs;
      top = bvs;
    }

(**************************************************************************)

type 'a dfa_result = 'a bvset array

let pred (instrs : lua_ops array) (i:int):int list =
  let visit (res,pc) item =
    match item with
    | Jump offset ->
       if pc+offset = i
       then (pc::res,pc+1)
       else (res,pc+1)
    | _ ->
       if pc = i-1
       then (pc::res,pc+1)
       else (res,pc+1)
  in
  let (res,_)=
    Array.fold_left visit ([],0) instrs in
  res
;;
  
let succ (instrs : lua_ops array) (i:int):int list =
  let visit (res,pc) item =
    match item with
    | Jump offset ->
       ((pc+offset)::res,pc+1)
    | _ ->
       if pc = i+1
       then (pc::res,pc+1)
       else (res,pc+1)
  in
  let (res,_)=
    Array.fold_left visit ([],0) instrs in
  res
;;

let do_dfa (d : 'a dfa) : 'a dfa_result =
  match d with
  | {
      instrs=instrs;
      dir=dir;
      may_must=may_must;
      gen_kill=gen_kill;
      entry_or_exit_facts=fact;
      top=top;
    } ->
     (* ********** *)
     let calc old dir may_must gen_kill=
       let combine = match may_must with K_Must -> inter | K_May -> union in
       let (source,offset) = match dir with D_Forward -> (pred,1) | D_Backward -> (succ,0) in
       let res = Array.copy old in
       let () =
         Array.iteri
           (fun index instr ->
             (* combine information from all the pred/succ *)
             let sum = List.fold_left
                         (fun res item ->
                           combine res (Array.get old (item+offset)))
                         top
                         (source instrs index) 
             in
             let (gen,kill) = gen_kill index in
             Array.set res (index+offset) (diff (union sum gen) kill)
           )
           instrs
       in
       res
     in
     (* check whether the result stablize *)
     let check_eq r1 r2 =
       let rec helper r1 r2 index =
         if Array.length r1 = index
         then true
         else
           if equal (Array.get r1 index) (Array.get r2 index)
           then helper r1 r2 (index+1)
           else false
       in
       helper r1 r2 0
     in
     let rec iter (old:'a bvset array) =
       let res = calc old dir may_must gen_kill in
       (* let () = my_dump_res (fun o n -> Printf.fprintf o "%d" n) res in *)
       if check_eq res old
       then res
       else iter res
     in
     (* ********** *)
     let init = Array.make ((Array.length instrs)+1) top in
     iter init
;;
     


(* forward *)
(* 0     1      2       3 *)
(*    0 ----> 1 ----> 2    *)
