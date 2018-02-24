(**
 * Module for converting OCaml bytecode into grouped
 * instructions. We do this so we can calculate OCaml
 * offsets (given with respect to the program counter)
 * to EVM offsets (given as absolute program counter
 * values).
 *
 * This is an intermediate representation, following the
 * OCaml bytecode and preceding the EVM bytecode.
 *)
type t =
  | Evm of Evm.instr
  | Push_caml_code_offset of int

type 'a group = { instrs : 'a list; caml_len : int; }
type program = (t group * int) list

(* Convert from array of bytes representing OCaml bytecode program
 * to grouped EVM program *)
val convert : int array -> program

val to_string : program -> string
