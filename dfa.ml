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
  let bvs = mkempty [] in
    {
      instrs = instrs;
      dir = D_Forward;
      may_must = K_May;
      gen_kill = (fun _ -> (bvs, bvs));
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

let pred (instrs : lua_ops array) (i:int):int list = []

let succ (instrs : lua_ops array) (i:int):int list = []

let do_dfa (d : 'a dfa) : 'a dfa_result =
  (* Implement me! *)
  [||]
