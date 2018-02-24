(* Final result of program.
 * Abstract type so we can't accidentally run copy_code twice.
 *)
type program

(* int is the number of elements of the resulting block to
 * return *)
val copy_code : Evm.program -> return_size : int -> program

(* Convert abstract type to program type *)
val to_evm : program -> Evm.program
