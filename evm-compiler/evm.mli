type byte = int (* With 0 <= i < 256 *)

type instr =
  (* Stop *)
  | STOP

  (* Arithmetic Operations *)
  | ADD | MUL | SUB | DIV | SDIV
  | MOD | SMOD | ADDMOD | MULMOD | EXP
  | SIGNEXTEND

  (* Comparison & Bitwise Logic Operations *)
  | LT | GT | SLT | SGT | EQ | ISZERO
  | AND | OR | XOR | NOT | BYTE

  (* SHA3 *)
  | SHA3

  (* Environmental Information *)
  | ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE
  | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE
  | CODECOPY | GASPRICE | EXTCODESIZE | EXTCODECOPY

  (* Block Information *)
  | BLOCKHASH | COINBASE | TIMESTAMP | NUMBER
  | DIFFICULTY | GASLIMIT

  (* Stack, Memory, Storage, and Flow Operations *)
  | POP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE
  | JUMP | JUMPI | PC | MSIZE | GAS | JUMPDEST

  (* Push, Duplicate, and Exchange *)
  | PUSH of {
      n : int; (* 1 <= n <= 32 *)
      bytes : Byte_seq.t; (* |x| <= n *)
    }
  | DUP of int  (* 1 <= n <= 16 *)
  | SWAP of int (* 1 <= n <= 16 *)

  (* Logging Operations *)
  | LOG of int (* 1 <= i <= 4 *)

  (* System Operations *)
  | CREATE | CALL | CALLCODE | RETURN | DELEGATECALL
  | REVERT | INVALID | SELFDESTRUCT

val opcode : instr -> int (* return value is a byte *)
val delta : instr -> int * int (* items removed from stack, items added to stack *)

type program = instr list

val int64_to_bytes : int64 -> Byte_seq.t * int
val push : int64 -> instr

val instr_to_string : instr -> string
val to_bytes : program -> int list
val to_hex_string : program -> string
val to_string : program -> string
