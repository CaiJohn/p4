(* This code is copyright 2012 by John Toman. Permission is granted for use in the instruction of CMSC430 at the University of Maryland *)

(** A module for disassembling lua bytecode *)


type lua_const = [
  | `L_String of string (** A lua string *)
  | `L_Double of float (** A "number", which is a double on most platforms *)
  | `L_Bool of bool (** A boolean value *)
  | `L_Nill (** The nil primitive *)
];;
(** Primitive values supported by the Lua VM. "Nill" should be "Nil" but is kept "Nill" for consistency with the assembler. *)

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

(** The type of arithmetic operation for an arithmetic operation *)

type arith_op = {
  op_type: arith_op_types; (** The operator used in this operation. *)
  left_src: rk_source; (** The left operand, that is, the value on the left of the operator *)
  right_src: rk_source; (** The right operand, that is, the value on the right of the operator *)
  dest: int (** The destination register in which to store the result of this arithmetic operation *)
};;
(** A common representation of an arithmetic expression. *)

(** {7 Comparisons} *)

type comparison_type = 
    Comp_Eq
  | Comp_Lt
  | Comp_Le

(** The comparisons possible in a Comparison instruction. The names are self-explanatory *)


type comparison_op = {
  skip_if_not: bool; (** A comparison instruction will skip the next instruction if the result of the comparison is NOT equal to this field *)
  right_operand: rk_source; (** The operand on the right side of the comparison *)
  left_operand: rk_source; (** The operand of the left side of the comparison *)
  comp_type: comparison_type (** The type of comparison that this comparison_op encodes.*)
}

(** A common representation of comparison operations. See page 35 for details on how Lua encodes conditionals. *) 

(** {7 Instructions Representations} *)

type return_val = 
  | No_Value (** Return no value from the function *)
  | Return_One of int (** Return one value from the function: the contents of the specified register *)
  | Return_Many of int * int (** [Return_Many (r,n)] return [n] values, starting at register [r] *)
  | Return_All of int (** Returns all registers from the specified register to the top of the stack *)
(** An abstraction away from the complexities of returning values from a function. *)


type lua_ops = 
  | Load_Const of int * lua_const (** Loads the specified constant into the named register *)
  | Get_Global of int * string  (** Loads the global variable identified by the string into the specified register *)
  | Set_Global of string * int (** [Set_Global (global_name,src_reg)] sets the global variable with the name [global_name] to the value in register [src_reg] *)
  | Load_Nil of int * int (** [Load_Nil (start,end)] sets the registers between [start] and [end] (inclusive) to nil *)
  | Load_Bool of int * bool * bool (** [Load_Bool (dst,boolean_val,skip_next)] places the boolean value [boolean_val] in register [dst]. If [skip_next] is true, then the next instruction is skipped *)
  | Get_Upvalue of int * int (** [Get_Upvalue (dst,index)] gets the upvalue with index [index] and places it in register [dst]. It can also be used to make an upvalue into another for a function loaded with the Closure instruction. See page 52 of the no frills guide and the documentation for the closure function for details.  *)
  | Set_Upvalue of int * int (** [Set_Upvalue (dst_index,src_reg)] places the value in [src_reg] into the upvalue at index [dst_index] *)
  | Unary_Minus of int * int (** [Unary_Minus (a,b)] negates the value in [b] and places it in register [a] *)
  | Unary_Not of int * int (** [Unary_Plus (a,b)] performs a boolean not on the value in [b] and places the result in register [a] *)
  | Length of int * int (** [Length (dst,src)] finds the length (or size) or [src] and places it in [dst] *)
  | Concat of int * int * int (** [Concat (dst,start,end)] String concatenates a range of registers starting at [start] and ending with [end] and places the result in [dst] *)
  | Jump of int (** Performs an unconditional jump by adding the displacement (which may be negative) to the PC *)
  | Tail_Call of int * int (** Performs a tail call which is effectively a goto. The function resides in the register identified by the first argument, and the second argument encodes the parameters similarly to the Call instruction *)
  | Vararg of int * int (** Implements the vararg operator. The first element of the tuple encodes the A field, and the second element encodes the B field. *)
  | Self of int * int * rk_source (** [Self (dst,table,key)] is short-hand for R(dst) := R(table)[key]; R(dst+1) := R(table). It is used if OO in lua *)
  | Cmp of comparison_op (** Comparison instruction that incorporates the LT, EQ, and LE instructions. See {!Disassembler.comparison_op} for an explanation of the fields *)
  | Test of int * bool (** See page 37 of the no frills guide for details. The first tuple member is the a field and the second member is the C field coerced to a boolean *)
  | Test_Set of int * int * bool (** See page 37  of the no frills guide for details. The first and second tuple members are the A and B fields respectively. The third member is the C field coerced to a boolean *)
  | For_Prep of int * int (** See page 42 of the no frills guide for details. The first tuple member is the value of the A field, and the second element is the displacement *)
  (* [a] * [displacement] *)
  | For_Loop of int * int (** See page 42 of the no frills guide for details The first tuple member is the value of the A field, and the second element is the displacement. *)
  | T_For_Loop of int * int (** See page 45 of the no frills guide for details. The first tuple member is the value of the A field, and the second element is the displacement. *)
  | Close of int (** [Close a] Closes all local variables from register [a] to the top of the stack. See page 54 of the no frills guide for details. *)
  | Move of int * int  (** [Move (dst,src)] sets the contents of register [dst] to that of register [src]. It is also a pseudo-instruction that makes a register into an upvalue; see page 52 in the no frills guide for details. *)
  | Arith of arith_op (** An arithmetic operation *)
  | Call of int * int * int (** [Call (fn,args,ret)] calls the function loaded into register [fn]. There are [args-1] arguments passed to this function. These arguments reside in registers [fn+1] to [fn+(args-1)]. There are [ret-1] return values saved. These return value(s) function are stored in [fn] to [fn + (ret - 2)]. *)
  (* [destination register] * [function index] *)
  | Closure of int * int (** [Closure (dst,function_index)] loads the closure at [function_index] the current function unit's child functions into [dst]. See {!Disassembler.function_unit} for details on child functions. Following this instruction are [n] pseudo-instructions, where [n] is the number of upvalues required by the loaded function. These pseudo-instructions store a either a local register of an upvalue as an upvalue for the loaded function, depending on whether the pseudo-instruction is a Moive or a Get_Upvalue instruction respectively. The B field (src field) of these instructions identifies which register/upvalue should be used as an upvalue. See page 52 in the no frills guide for details. *)
  (* [dest register] * [table reg] * [table key] *)
  | Load_Table of int * int * rk_source (** [Load_Table (dst,table,key)] loads value associated with [key] in the table whose reference is stored in the register [table] into the register [dst]. [key] may be a number (for array lookups), or a string (for hash tables). *)
  (* [table reg] * [table key] * [table_value] *)
  | Set_Table of int * rk_source * rk_source (** [Set_Table (table,k,v)] associates the key [k] with the value [v] in the table whose reference is stored in the register [table]. [k] may be an integer or a string, and [v] may be any value, including a register whose contents is a reference to another table, in which case [k] is associated with a reference to the table. *)
  | Set_List of int * int * int (** [Set_List (table,page,n)] sets a range of numerical indices in the table to values in the registers [table+1] to [table+n] inclusive. The numerical indices set in the table are [(page-1)*50] to [(page-1)*50+n]. Note that page must be offset by 1, a [page] value of 0 will not work as expected.*)
  | Make_Table of int * int * int (** [Make_Table (dst,n_array,n_hash)] initializes a new table in the register [dst] that has [n_array] array slots pre-allocated and [n_hash]  hash slots pre-allocated. These values are hints, and using more hash slots than specified in [n_hash] or using array indices greater than [n_array] is not an error *)
  | Return of return_val (** Returns a value from the function. *)

(** The instructions supported by the Lua 5.1 virtual machine. The following are the general rules of thumb regarding the ordering of members in tags that have tuples as their arguments
{ul 
{- Pseudo-instructions are represented by records. These include the Arith and Cmp instructions}
{- The elements are ordered in the form (dst,src1,...). The order of the src elements is done by reading left to right in the RTL representation found accompanying each instruction in the no frill guide. {b Note:} this can lead to an order of elements that is inconsistent with the A B C order of fields found in the no frills guide. For instance, the Set_Upvalue instruction encodes the source value in the A (i.e. first) field, but this appears as the {e second} element of the tuple for the Set_Upvalue instruction. Instructions with this inconsistent ordering are: Set_Upvalue, Set_Global, and Set_List}
{- If there is no clear destination or source, and the instruction deals with some concept of "start" and "end" or "start" and "amount", start is the first element and the end (or "amount") is the second element. This includes the Vararg, Concat, and Load_Nil instructions}
{- if none of the above rules apply, then the elements' correspond to the position in the field list, that is, A B C, A Bx, or A sBx, depending on the instruction type }}
*)

(** {6 API} *)

type vararg_flags = VarArg_HasArg | VarArg_IsVarArg | VarArg_NeedsArg;;

(** Flags that control the vararg behavior of a function. See page 9 of the no frills guide for details *)

type function_unit = {
  instructions: lua_ops list; (** The list of instructions that compose the body of this function *)
  num_params: int; (** The number of formal parameters to this function *)
  vararg: vararg_flags list; (** The vararg flags for this function. See page 9 of the no frills guide for details. *)
  num_upvalues: int; (** The number of upvalues of used by this function. *)
  child_functions: function_unit list; (** The child functions accessible within this function. These functions are loaded via the closure instruction, and they are identified by the numeric index in this list. *)
}

(** Represents what the no frills guide calls a binary chunk (what is referred to as a function_unit in this documentation. *)


val disassemble: in_channel -> function_unit
(** Disassembles the lua bytecode file from the first argument. Returns the {!Disassembler.function_unit} for the top-level binary chunk *)
val dump_function: int -> function_unit -> unit
(** Pretty prints a function_unit on standard out. For debugging purposes. *)
val dump_instruction: lua_ops -> string
(** Returns a pretty printed representation of an instruction to standard out. The string representation resembles the RTL found in the no frills guide, but differs in some places and is non-standard and for debugging purposes {b only}. *)
