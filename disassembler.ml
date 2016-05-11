(* This code is copyright 2012 by John Toman. Permission is granted for use in the instruction of CMSC430 at the University of Maryland *)

let int_size = ref 0;;
let size_t_size = ref 0;;

type instruction_type = 
  | I_ABC
  | I_ABx
  | I_AsBx;;

type instruction = {
  a: int;
  b: int;
  c: int;
  opcode: int;
};;

module type GENINT = sig
  type t
  val to_int: t -> int
  val logand: t -> t -> t
  val logor: t -> t -> t
  val shift_right: t -> int -> t
  val shift_left: t -> int -> t
  val of_int: int -> t
  val size: int
  val zero: t
end


type lua_const = [
  | `L_String of string (** A lua string *)
  | `L_Double of float (** A "number", which is a double on most platforms *)
  | `L_Bool of bool (** A boolean value *)
  | `L_Nill (** The nil primitive *)
];;
(** Primitive values supported by the Lua VM *)

type rk_source = [ 
  lua_const
  | `Register of int
];;
(** Some instructions allow for direct references to constants to appear in the source operands along with registers.
    In the "No Frills" guide, an instruction uses an rk_source when when the operands in the RTL representation of the instructions use RK(A) or RK(B)
*)

(** {6 Lua Instructions} *)


(** {7 Arithmetic Operations} *)

type arith_op_types = Arith_Add | Arith_Sub | Arith_Mul | Arith_Div | Arith_Mod | Arith_Pow;;

(** The type of arithmetic operation for an arithmetic operation. The names are self explanatory. *)

type arith_op = {
  op_type: arith_op_types; (** The operator used in this operation. *)
  left_src: rk_source; (** The left operand, that is, the value on the left of the operator *)
  right_src: rk_source; (** The right operand, that is, the value on the right of the operator *)
  dest: int (** The destination register in which to store the result of this arithmetic operation *)
};;
(** A common representation of an arithmetic expression. *)

(** {7 Supported Instructions} *)

type return_val = 
  | No_Value (** Return no value from the function *)
  | Return_One of int (** Return one value from the function: the contents of the specified register *)
  | Return_Many of int * int (** [Return_Many (r,n)] return [n] values, starting at register [r] *)
  | Return_All of int 
(** An abstraction away from the complexities of returning values from a function (variable number of return values is not supported). *)

type comparison_type = 
    Comp_Eq
  | Comp_Lt
  | Comp_Le

type comparison_op = {
  skip_if_not: bool;
  right_operand: rk_source;
  left_operand: rk_source;
  comp_type: comparison_type
}

type lua_ops = 
  | Load_Const of int * lua_const (** Loads the specified constant into the named register *)
  | Get_Global of int * string  (** Loads the global variable identified by the string into the specified register *)
  | Set_Global of string * int
  | Load_Nil of int * int
  | Load_Bool of int * bool * bool
  | Get_Upvalue of int * int
  | Set_Upvalue of int * int
  | Unary_Minus of int * int
  | Unary_Not of int * int
  | Length of int * int
  | Concat of int * int * int
  | Jump of int
  | Tail_Call of int * int
  | Vararg of int * int
  (* [dst] * [src table] * [key] *)
  | Self of int * int * rk_source
  (* [expected] != [src1] OP [src2] *)
  | Cmp of comparison_op
  | Test of int * bool
  | Test_Set of int * int * bool
  (* [a] * [displacement] *)
  | For_Prep of int * int
  (* [a] * [displacement] *)
  | For_Loop of int * int
  | T_For_Loop of int * int
  | Close of int
  | Move of int * int  (** [Move (dst,src)] sets the contents of register [dst] to that of register [src] *)
  | Arith of arith_op (** An arithmetic operation *)
  | Call of int * int * int (** [Call (fn,args,ret)] calls the function loaded into register [fn]. There are [args-1] arguments passed to this function. These arguments reside in registers [fn+1] to [fn+(args-1)]. There are [ret-1] return values saved. These return value(s) function are stored in [fn] to [fn + (ret - 2)]. *)
  (* [destination register] * [function index] *)
  | Closure of int * int (** [Closure (dst,function_index)] loads the closure at [function_index] the current function unit's child functions into [dst]. See {!Assembler.function_unit} for details on child functions. *)
  (* [dest register] * [table reg] * [table key] *)
  | Load_Table of int * int * rk_source (** [Load_Table (dst,table,key)] loads value associated with [key] in the table whose reference is stored in the register [table] into the register [dst]. [key] may be a number (for array lookups), or a string (for hash tables). *)
  (* [table reg] * [table key] * [table_value] *)
  | Set_Table of int * rk_source * rk_source (** [Set_Table (table,k,v)] associates the key [k] with the value [v] in the table whose reference is stored in the register [table]. [k] may be an integer or a string, and [v] may be any value, including a register whose contents is a reference to another table, in which case [k] is associated with a reference to the table. *)
  | Set_List of int * int * int (** [Set_List (table,page,n)] sets a range of numerical indices in the table to values in the registers [table+1] to [table+n] inclusive. The numerical indices set in the table are [(page-1)*50] to [(page-1)*50+n]. Note that page must be offset by 1, a [page] value of 0 will not work as expected.*)
  | Make_Table of int * int * int (** [Make_Table (dst,n_array,n_hash)] initializes a new table in the register [dst] that has [n_array] array slots pre-allocated and [n_hash]  hash slots pre-allocated. These values are hints, and using more hash slots than specified in [n_hash] or using array indices greater than [n_array] is not an error *)
  | Return of return_val (** Returns a value from the function. *)

type vararg_flags = VarArg_HasArg | VarArg_IsVarArg | VarArg_NeedsArg;;

type function_unit = {
  instructions: lua_ops list;
  num_params: int;
  vararg: vararg_flags list;
  num_upvalues: int;
  child_functions: function_unit list;
}

module IntReader(I : GENINT) : sig
  val read: in_channel -> I.t
end = struct
  let rec read_loop (accum : I.t) (index : int) (channel : in_channel) = 
    if index = I.size then
      accum
    else
      let (in_byte : I.t) = I.of_int (input_byte channel) in
      read_loop (I.logor accum (I.shift_left in_byte (index * 8)))
	(index+1) channel
  let read in_chan = read_loop I.zero 0 in_chan
end;;

module Int64Reader = IntReader(struct include Int64 let size = 8 end);;
module Int32Reader = IntReader(struct include Int32 let size = 4 end);;

let read_int64 channel = Int64Reader.read channel;;
let read_int32 channel = Int32Reader.read channel;;

let read_float channel = 
  Int64.float_of_bits (read_int64 channel)

let read_size_t chan = 
  if (!size_t_size) == 4 then 
    Int32.to_int (read_int32 chan) 
  else 
    Int64.to_int (read_int64 chan);;

let (read_integer : in_channel -> int) = fun chan ->
  if (!int_size) == 4 then 
    Int32.to_int (read_int32 chan)
  else 
    Int64.to_int (read_int64 chan);;

let read_string channel = 
(*  Printf.printf "Reading string at %08X\n" (pos_in channel); *)
  let string_size = read_size_t channel in
  if string_size == 0  then ""
  else
    let to_return = String.create (string_size - 1) in
    for i = 0 to string_size - 2 do
      to_return.[i] <- (Char.chr (input_byte channel))
    done;
(*    Printf.printf "read string %s\n" to_return; *)
    ignore (input_byte channel); to_return;;

let mask_of_int i = 
  let rec loop index accum =
    if index = i then accum else
      loop (index+1) (Int32.logor accum (Int32.shift_left Int32.one index)) in
  loop 0 Int32.zero;;

let get_field n start width = 
  let mask = mask_of_int width in
  Int32.to_int (Int32.logand (Int32.shift_right n start) mask);;

let get_opcode n = get_field n 0 6;;

let type_of_opcode = [|
I_ABC; (*  MOVE *)
I_ABx; (*LOADK*)
I_ABC; (*LOADBOOL*)
I_ABC; (*LOADNIL*)
I_ABC; (*GETUPVAL*)
I_ABx; (*GETGLOBAL*)
I_ABC; (* GETTABLE*)
I_ABx; (*SETGLOBAL*)
I_ABC; (*SETUPVAL*)
I_ABC; (*SETTABLE*)
I_ABC; (* NEWTABLE *)
I_ABC; (* SELF *)
I_ABC; (* ADD *)
I_ABC; (* SUB *)
I_ABC; (* MUL *)
I_ABC; (* DIV *)
I_ABC; (* MOD *)
I_ABC; (* POW *)
I_ABC; (* UNM *)
I_ABC; (* NOT *)
I_ABC; (* LEN *)
I_ABC; (* CONCAT *)
I_AsBx; (* JMP *)
I_ABC; (* EQ *)
I_ABC; (* LT *)
I_ABC; (* LE *)
I_ABC; (* TEST *)
I_ABC; (* TESTSET *)
I_ABC; (* CALL *)
I_ABC; (* TAILCALL *)
I_ABC; (* RETURN *)
I_AsBx; (* FORLOOP *)
I_AsBx; (* FORPREP *)
I_ABC; (* TFORLOOP *)
I_ABC; (* SETLIST *)
I_ABC; (* CLOSE *)
I_ABx; (* CLOSURE *)
I_ABC; (* VARARG *)
|];;

let unblit_instruction word = 
  let opcode = get_opcode word in
  let a_field = get_field word 6 8 in
  match type_of_opcode.(opcode) with
    | I_ABC ->
	{
	  a = a_field;
	  c = get_field word 14 9;
	  b = get_field word 23 9;
	  opcode = opcode;
	}
    | I_ABx -> 
      {
	a = a_field;
	b = get_field word 14 18;
	c = 0;
	opcode = opcode
      }
    | I_AsBx ->
      {
	a = a_field;
	b = (get_field word 14 18) - 131071;
	c = 0;
	opcode = opcode
      }  

let dump_raw_instruction instr = 
  Printf.printf "opcode: %d\n" instr.opcode;
  Printf.printf "a field: %d\n" instr.a;
  Printf.printf "b field: %d\n" instr.b;
  Printf.printf "c field: %d\n" instr.c

let decompile_instruction word (constant_list : lua_const array) = 
  let instr = unblit_instruction word in
  let field_to_rk f = if f < 256 then (`Register f :> rk_source) else (constant_list.(f - 256) :> rk_source) in
  match instr.opcode with
    | 0 -> Move (instr.a,instr.b)
    | 1 -> Load_Const (instr.a, constant_list.(instr.b))
    | 2 -> Load_Bool (instr.a, instr.b != 0, instr.c != 0)
    | 3 -> Load_Nil (instr.a, instr.b)
    | 4 -> Get_Upvalue (instr.a, instr.b)
    | 5 -> 
      begin
	match constant_list.(instr.b) with
	  | `L_String s -> Get_Global(instr.a, s)
	  | _ -> failwith "attempt to use a non string constant as a global name (get)"
      end
    | 6 -> Load_Table (instr.a,instr.b,(field_to_rk instr.c))
    | 7 -> 
      begin
	match constant_list.(instr.b) with
	  | `L_String s -> Set_Global(s,instr.a)
	  | _ -> failwith "attempt to use a non-string constant as a global name (set)"
      end
    | 8 -> Set_Upvalue (instr.b, instr.a)
    | 9 -> Set_Table (instr.a,(field_to_rk instr.b),(field_to_rk instr.c))
    | 10 -> 
      let decode_byte b = 
	let exp = (b lsr 3) land 31 in
	let mantissa = (b land 7) in
	if exp == 0 then mantissa else
	(8 + mantissa) * (int_of_float (2.0 ** (float (exp - 1)))) in
      Make_Table (instr.a,(decode_byte instr.b),(decode_byte instr.c))
    | 11 ->
      Self (instr.a, instr.b, (field_to_rk instr.c))
    | 12 | 13 | 14 | 15 | 16 | 17 ->
      (* arithmetic
	 TODO: turn the below function into an array lookup a la
	 the opcode -> instruction type approach we're using now
      *)
      let get_op_of_a a = match a with
	| 12 -> Arith_Add
	| 13 -> Arith_Sub
	| 14 -> Arith_Mul
	| 15 -> Arith_Div
	| 16 -> Arith_Mod
	| 17 -> Arith_Pow
	| _ -> failwith "this is impossible"
      in 
      Arith {
	dest = instr.a;
	op_type = get_op_of_a instr.opcode;
	right_src = field_to_rk instr.c;
	left_src = field_to_rk instr.b
      }
    | 18 -> Unary_Minus (instr.a, instr.b)
    | 19 -> Unary_Not (instr.a, instr.b)
    | 20 -> Length (instr.a, instr.b)
    | 21 -> Concat (instr.a, instr.b, instr.c)
    | 22 -> Jump (instr.b)
    | 23 | 24 | 25 ->
      (* conditional tests *)
      Cmp {
	skip_if_not = instr.a != 0;
	right_operand = field_to_rk instr.c;
	left_operand = field_to_rk instr.b;
	comp_type = match instr.opcode with
	  | 23 -> Comp_Eq
	  | 24 -> Comp_Lt
	  | 25 -> Comp_Le
	  | _ -> failwith "this is impossible"
      }
    | 26 -> (* test *)
      Test (instr.a, instr.c != 0)
    | 27 -> (* test set *)
      Test_Set (instr.a, instr.b, instr.c != 0)
    | 28 -> (* call *)
      (* TODO: maybe make this nicer to handle a la the return type? *)
      Call (instr.a, instr.b, instr.c)
    | 29 ->
      Tail_Call (instr.a, instr.b)
    | 30 -> (* return *)
      if instr.b = 0 then
	Return (Return_All instr.a)
      else if instr.b = 1 then
	Return No_Value
      else if instr.b = 2 then
	Return (Return_One instr.a)
      else
	Return (Return_Many (instr.a,(instr.b - 1)))
    | 31 -> (* forloop *)
      For_Loop (instr.a, instr.b)
    | 32 -> (* forprep *)
      For_Prep (instr.a, instr.b)
    | 33 -> (* tforloop *)
      T_For_Loop (instr.a, instr.c)
    | 34 -> (* set list *)
      if instr.c == 0 then failwith "Casting to next instruction is not supported"
      else
	Set_List (instr.a, instr.c, instr.b)
    | 35 -> (* close *)
      Close instr.a
    | 36 -> Closure (instr.a,instr.b)
    | 37 -> Vararg (instr.a, instr.b)
    | _ -> failwith (Printf.sprintf "unrecognized opcode %d" instr.opcode)

let parse_header chan = 
  let magic_bytes = Int32.to_int (read_int32 chan) in
  let version = input_byte chan in
  let _ = input_byte chan in
  let endianess = input_byte chan in
  int_size := input_byte chan;
  size_t_size := input_byte chan;
  let instruction_size = input_byte chan in
  let lua_num_size = input_byte chan in
  let integral_flag = input_byte chan in
  if magic_bytes != 0x61754C1B || 
    version != 0x51 || endianess != 1 ||
    lua_num_size != 8 || integral_flag != 0 ||
    instruction_size != 4 then
    failwith "the bytecode format is not supported"

let rec decompile_function chan = 
  let function_pos = pos_in chan in
(*  Printf.printf "Function at offset %08X\n" (pos_in chan);*)
  let read_instructions () = 
    let num_instructions = read_integer chan in
    let instruction_arr = Array.make num_instructions Int32.zero in
    for i = 0 to num_instructions - 1 do
      instruction_arr.(i) <- (read_int32 chan);
    done;
    instruction_arr in
  let read_constant_list () = 
    let num_constants = read_integer chan in
    let constant_arr = Array.make num_constants `L_Nill in
    for i = 0 to num_constants - 1 do
      let constant_tag = input_byte chan in
      let constant_val = 
	match constant_tag with
	  | 0 -> `L_Nill
	  | 1 -> `L_Bool ((input_byte chan) != 0)
	  | 3 -> `L_Double (read_float chan)
	  | 4 -> `L_String (read_string chan)
	  | _ -> failwith (Printf.sprintf "Unrecognized constant tag %d" constant_tag)
      in
      constant_arr.(i) <- constant_val
    done;
    constant_arr in
  (* ignore the source file name *)
  let _ = read_string chan in
  (* ignore the line defined *)
  let _ = read_integer chan in
  (* ignore the last line defined *)
  let _ = read_integer chan in
  let num_upvalues = input_byte chan in
  let num_parameters = input_byte chan in
  let vararg_flags = input_byte chan in
  (* ignore the max stack size *)
  let _ = input_byte chan in
(*  Printf.printf "%08X: Reading instructions at %0X\n" function_pos (pos_in chan);*)
  let instructions = read_instructions () in
(*  Printf.printf "Read %d instructions\n" (Array.length instructions);
  Printf.printf "%08X: reading constants at %08X\n" function_pos (pos_in chan); *)
  let constants = read_constant_list () in
(*  Printf.printf "Read %d constants\n" (Array.length constants);*)
(*  let _ = Printf.printf "instruction size %d\n" (Array.length instructions) in
  let _ = Printf.printf "constant size %d\n" (Array.length constants) in*)
  let rec loop accum ind  = 
    if ind == -1 then accum
    else
      let decompiled_instr = decompile_instruction instructions.(ind) constants in
      loop (decompiled_instr::accum) (ind - 1) in
  let instruction_list = loop [] ((Array.length instructions) - 1) in
  (* Printf.printf "%08X: Reading children functions at %08X\n" function_pos (pos_in chan); *)
  let num_children = read_integer chan in
  (* Printf.printf "%d children \n" num_children; *)
  let rec f_loop accum ind = 
    if ind = num_children then accum
    else
      (Printf.printf "%08X: getting child function %d\n" function_pos ind;
      let child_f = decompile_function chan in
      f_loop (child_f::accum) (ind+1))
  in
  let child_functions = List.rev (f_loop [] 0) in
  (* read and ignore debug data *)
  (* Printf.printf "%08X: reading source line list at position %08X\n" function_pos (pos_in chan); *)
  let num_sline_list = read_integer chan in
  for i = 0 to num_sline_list -1 do
    ignore (read_integer chan)
  done;
  (* Printf.printf "%08X: reading locals at position %08X\n" function_pos (pos_in chan); *)
  let num_locals = read_integer chan in
  for i = 0 to num_locals - 1 do
    ignore (read_string chan);
    ignore (read_integer chan);
    ignore (read_integer chan);
  done;
  (* Printf.printf "%08X: reading upvalues at position %08X\n" function_pos (pos_in chan); *)
  let upval_list_size = read_integer chan in
  for i = 0 to upval_list_size - 1 do
    ignore (read_string chan);
  done;
  let vararg_list = List.fold_left
    (fun accum (tag,flag) ->
      if (vararg_flags land flag) != 0 then
	tag::accum
      else accum) [] [ (VarArg_HasArg,1); (VarArg_IsVarArg, 2); (VarArg_NeedsArg, 4)] in
  {
    instructions = instruction_list;
    num_params = num_parameters;
    vararg = vararg_list;
    num_upvalues = num_upvalues;
    child_functions = child_functions
  }

let string_of_reg r = 
  Printf.sprintf "R(%d)" r
  
let string_of_rk = function
  | `L_String s -> Printf.sprintf "\"%s\"" s
  | `L_Bool b -> Printf.sprintf "%b" b
  | `L_Double d -> Printf.sprintf "%f" d
  | `L_Nill -> "nil"
  | `Register r -> string_of_reg r

(* returns a string representation of an instruction. For debugging purposes only
 *)
let dump_instruction = 
  let mk_arg_string func_reg  param_reg = 
    if param_reg == 0 then
      Printf.sprintf "(%s,...)" (string_of_reg (func_reg + 1))
    else if param_reg == 1 then
      "()"
    else if param_reg == 2 then
      Printf.sprintf "(%s)" (string_of_reg (func_reg + 1))
    else if param_reg == 3 then
      Printf.sprintf "(%s,%s)" (string_of_reg (func_reg + 1)) (string_of_reg (func_reg + 2))
    else
      Printf.sprintf "(%s,...,%s)" (string_of_reg (func_reg + 1))
	(string_of_reg (func_reg + (param_reg - 1)))
  in
  function
  | Move (dst,src) ->
    Printf.sprintf "%s := %s" (string_of_reg dst) (string_of_reg src)
  | Load_Const (dst,const) ->
    Printf.sprintf "%s := %s" (string_of_reg dst) (string_of_rk (const :> rk_source))
  | Load_Bool (dst,value,skip_next) ->
    (Printf.sprintf "%s := %b" (string_of_reg dst) value) ^
      (if skip_next then "; PC++" else "")
  | Load_Nil (start_reg,end_reg) ->
    Printf.sprintf "%s ... %s := nil" (string_of_reg start_reg) (string_of_reg end_reg)
  | Get_Upvalue (dst_reg,upval_num) ->
    Printf.sprintf "%s := Up[%d]" (string_of_reg dst_reg) upval_num
  | Get_Global (dst, global_name) ->
    Printf.sprintf "%s := %s" (string_of_reg dst) global_name
  | Load_Table (dst, table, key) ->
    Printf.sprintf "%s := %s[%s]" (string_of_reg dst) (string_of_reg table) (string_of_rk key)
  | Set_Global (dst, src) ->
    Printf.sprintf "%s := %s" dst (string_of_reg src)
  | Set_Upvalue (dst,src) ->
    Printf.sprintf "Up[%d] := %s" dst (string_of_reg src)
  | Unary_Minus (dst,src) ->
    Printf.sprintf "%s := -%s" (string_of_reg dst) (string_of_reg src)
  | Unary_Not (dst, src) ->
    Printf.sprintf "%s := not(%s)" (string_of_reg dst) (string_of_reg src)
  | Length (dst, src) ->
    Printf.sprintf "%s := length(%s)" (string_of_reg dst) (string_of_reg src)
  | Concat (dst,range_start,range_end) ->
    Printf.sprintf "%s := %s + .. + %s" (string_of_reg dst) (string_of_reg range_start) (string_of_reg range_end)
  | Jump offset ->
    Printf.sprintf "PC += %d" offset
  | Tail_Call (func_reg,param_reg) ->
    let base_string = Printf.sprintf "return %s" (string_of_reg func_reg) in
    let arg_string = mk_arg_string func_reg param_reg in
    base_string ^ arg_string
  | Vararg (range_start,amount) ->
    Printf.printf "amount %d\n" amount;
    if amount == 0 then
      Printf.sprintf "%s,... = vararg" (string_of_reg range_start)
    else
      Printf.sprintf "%s,...,%s = vararg" (string_of_reg range_start)
	(string_of_reg (range_start + (amount - 1)))
  | Self (dst, table, key) ->
    Printf.sprintf "%s := %s; %s := %s[%s]"
      (string_of_reg (dst+1))
      (string_of_reg table)
      (string_of_reg dst)
      (string_of_reg table)
      (string_of_rk key)
  | Cmp c ->
    let string_of_cmp = function
      | Comp_Eq -> "=="
      | Comp_Lt -> "<"
      | Comp_Le -> "<="
    in
    let operator_string = string_of_cmp c.comp_type in
    Printf.sprintf "if((%s %s %s) != %b) { PC++; }"
      (string_of_rk c.right_operand)
      operator_string
      (string_of_rk c.left_operand)
      c.skip_if_not
  | Test (r_target,b_target) ->
    Printf.sprintf "if(not(%s != %b)) { PC++; }"
      (string_of_reg r_target)
      (b_target)
  | Test_Set (target,test_target,expected) ->
    let test_target_str = string_of_reg test_target in
    Printf.sprintf "if(%s != %b) { %s := %s } else { PC++; }"
      test_target_str
      expected
      (string_of_reg target)
      test_target_str
  | For_Prep (reg,displacement) ->
    Printf.sprintf "%s -= %s; PC += %d"
      (string_of_reg reg)
      (string_of_reg (reg + 2))
      displacement
  | For_Loop (reg,displacement) ->
    let reg_string = string_of_reg reg in
    Printf.sprintf "%s += %s; if (%s <?= %s) { PC += %d; %s := %s }"
      reg_string
      (string_of_reg (reg + 2))
      reg_string
      (string_of_reg (reg + 1))
      displacement
      (string_of_reg (reg + 3))
      reg_string
  | T_For_Loop (reg,save_amount) ->
    let r3 = string_of_reg (reg + 3) in
    let r2 = string_of_reg (reg + 2) in
    Printf.sprintf "%s,...,%s := %s(%s,%s); if(%s != nil) { %s := %s } else { PC++; }"
      r3
      (string_of_reg (reg + save_amount + 2))
      (string_of_reg reg)
      (string_of_reg (reg + 1))
      r2 r3 r2 r3
  | Close r ->
    Printf.sprintf "Close >=%s" (string_of_reg r)
  | Arith a ->
    let string_of_op = function
      | Arith_Add -> "+"
      | Arith_Sub -> "-"
      | Arith_Mul -> "*"
      | Arith_Div -> "/"
      | Arith_Mod -> "%"
      | Arith_Pow -> "^"
    in
    let op_string = string_of_op a.op_type in
    Printf.sprintf "%s := %s %s %s"
      (string_of_reg a.dest)
      (string_of_rk a.left_src)
      op_string
      (string_of_rk a.right_src)
  | Call (fn,args,save) ->
    let ret_string = 
      if save == 1 then ""
      else if save == 0 then 
	Printf.sprintf "%s,... := " (string_of_reg fn)
      else if save == 2 then
	Printf.sprintf "%s := " (string_of_reg fn)
      else if save == 3 then
	Printf.sprintf "%s,%s := " (string_of_reg fn)
	  (string_of_reg (fn+1))
      else
	Printf.sprintf "%s,...,%s := "
	  (string_of_reg fn)
	  (string_of_reg (fn + save - 2))
    in
    let arg_string = mk_arg_string fn args in
    Printf.sprintf "%s%s%s" ret_string
      (string_of_reg fn)
      arg_string
  | Closure (dst,fn_index) ->
    Printf.sprintf "%s := KPROTO[%d]" (string_of_reg dst) fn_index
  | Set_Table (table,key,value) ->
    Printf.sprintf "%s[%s] := %s"
      (string_of_reg table)
      (string_of_rk key)
      (string_of_rk value)
  | Set_List (table,page,amount) ->
    let table_string = (string_of_reg table) in
    if amount == 0 then
      Printf.sprintf "%s[%d],... := %s,..."
	table_string
	((page-1)*50 + 1)
	(string_of_reg (table + 1))
    else
      Printf.sprintf "%s[%d],...,%s[%d] := %s,...,%s"
	table_string
	((page-1) * 50 + 1)
	table_string
	((page-1) * 50 + amount)
	(string_of_reg (table+1))
	(string_of_reg (table+amount))
  | Make_Table (dst,size_array,size_hash) ->
    Printf.sprintf "%s := {} (hash = %d, array = %d)"
      (string_of_reg dst)
      size_hash size_array
  | Return r ->
    match r with
      | No_Value -> "return"
      | Return_One r -> Printf.sprintf "return %s" (string_of_reg r)
      | Return_Many (r,n) ->
	Printf.sprintf "return %s,...,%s"
	  (string_of_reg r)
	  (string_of_reg (r+n-1))
      | Return_All r ->
	Printf.sprintf "return %s,..." (string_of_reg r)

let rec dump_function level f = 
  let tab_string = String.make level '\t' in
  let printf fmt = print_string tab_string; Printf.printf (format_of_string fmt) in
  let print s = Printf.printf "%s%s\n" tab_string s in
  let rec instruction_loop = function
    | [] -> ()
    | h::t -> print (dump_instruction h); instruction_loop t
  in
  printf "args: %d, upvalues: %d\n" f.num_params f.num_upvalues;
  instruction_loop f.instructions;
  let rec child_loop i f_list = match f_list with
    |  [] -> ()
    | h::t -> printf "Child %d\n" i;
      dump_function (level+1) h;
      printf "*** End child %d ***\n" i;
      child_loop (i+1) t
  in
  child_loop 0 f.child_functions;;

let disassemble chan =
  let _ = parse_header chan in
  decompile_function chan;;
