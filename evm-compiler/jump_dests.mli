(**
 * Phase that inserts jump destinations, and replaces Caml_code_offset
 * values with the actual offsets in the ethereum program.
 *)

type t =
  | Evm of Evm.instr
  | Label of Label.t
  | Goto of Label.t

type program = (t Grouper.group * int) list

val to_string : program -> string

(** Perform both of the below transformations, in this order:
  * (1) insert gotos and labels, removing caml_code_offsets.
  * (2) replace labels with JUMP_DESTS and gotos with absolute code offsets.
  *
  * Then, the resulting groups are concatenated into a single program.
  *
  * process x  =  x |> insert_jump_dests |> remove_code_offsets |> List.concat
  *)
val process : Grouper.program -> Evm.program

(** Perform only (1) from above *)
val insert_labels : Grouper.program -> program

(** Perform only (2) from above *)
val remove_labels : program -> Evm.instr list list
