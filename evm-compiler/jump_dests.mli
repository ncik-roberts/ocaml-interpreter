(**
 * Phase that inserts jump destinations, and replaces Caml_code_offset
 * values with the actual offsets in the ethereum program.
 *)

(** Perform both of the below transformations, in this order:
  * (1) insert jump destinations
  * (2) remove caml_code_offsets
  *
  * Then, the resulting groups are concatenated into a single program.
  *
  * process x  =  x |> insert_jump_dests |> remove_code_offsets |> List.concat
  *)
val process : Grouper.program -> Evm.program

(** Perform only jump-destination insertion *)
val insert_jump_dests : Grouper.program -> Grouper.program

(** Remove Caml code offsets. Precondition: jump_dests are inserted. *)
val remove_code_offsets : Grouper.program -> Evm.instr list list
