open Disassembler
open Dfa

let dump_res f r =
  let rec output_set o = function
    | [] -> ()
    | [x] -> f o x
    | x::xs -> Printf.fprintf o "%a, %a" f x output_set xs
  in
    Array.iteri (fun i s -> Printf.printf "%d: {%a}\n" i output_set (Bvset.mems s)) r

;;

let input_chan = open_in_bin Sys.argv.(2) in
let func_unit = disassemble input_chan in
let instrs = Array.of_list func_unit.instructions in
let _ = close_in input_chan in
  match Sys.argv.(1) with
    | "dump" -> dump_function 0 func_unit
    | "rd" ->
	let r = do_dfa (reach_defs instrs) in
	  Printf.printf "Reaching definitions\n";
	  dump_res (fun o n -> Printf.fprintf o "%d" n) r
    | "ae" ->
        let r = do_dfa (avail_exprs instrs) in
          Printf.printf "Available expressions\n";
          dump_res output_expr r
    | "lv" ->
        let r = do_dfa (live_vars instrs) in
          Printf.printf "Live variables\n";
          dump_res (fun o n -> Printf.fprintf o "%d" n) r
    | "vb" ->
        let r = do_dfa (very_busy_exprs instrs) in
          Printf.printf "Very busy expressions\n";
          dump_res output_expr r
    | _ -> failwith "Unknown command"
