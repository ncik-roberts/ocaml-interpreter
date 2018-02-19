(* Final result of program.
 * Abstract type so we can't accidentally run copy_code twice.
 *)
type program

val copy_code : Evm.program -> program

(* Convert abstract type to program type *)
val to_evm : program -> Evm.program
