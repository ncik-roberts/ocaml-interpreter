type byte = int (* With 0 <= i < 256 *)

type instr =
  (* Stop *)
  | STOP

  (* Arithmetic Operations *)
  | ADD | MUL | SUB | DIV | SDIV | MOD
  | SMOD | ADDMOD | MULMOD | EXP | SIGNEXTEND

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
  | LOG of int (* 0 <= i <= 4 *)

  (* System Operations *)
  | CREATE | CALL | CALLCODE | RETURN | DELEGATECALL
  | REVERT | INVALID | SELFDESTRUCT

let opcode = function
  (* Stop *)
  | STOP -> 0x00

  (* Arithmetic Operations *)
  | ADD -> 0x01 | MUL -> 0x02 | SUB -> 0x03 | DIV -> 0x04
  | SDIV -> 0x05 | MOD -> 0x06 | SMOD -> 0x07 | ADDMOD -> 0x08
  | MULMOD -> 0x09 | EXP -> 0x0a | SIGNEXTEND -> 0x0b

  (* Comparison & Bitwise Logic Operations *)
  | LT -> 0x10 | GT -> 0x11 | SLT -> 0x12 | SGT -> 0x13
  | EQ -> 0x14 | ISZERO -> 0x15 | AND -> 0x16 | OR -> 0x17
  | XOR -> 0x18 | NOT -> 0x19 | BYTE -> 0x1a

  (* SHA3 *)
  | SHA3 -> 0x20

  (* Environmental Information *)
  | ADDRESS -> 0x30 | BALANCE -> 0x31 | ORIGIN -> 0x32
  | CALLER -> 0x33 | CALLVALUE -> 0x34 | CALLDATALOAD -> 0x35
  | CALLDATASIZE -> 0x36 | CALLDATACOPY -> 0x37 | CODESIZE -> 0x38
  | CODECOPY -> 0x39 | GASPRICE -> 0x3a | EXTCODESIZE -> 0x3b
  | EXTCODECOPY -> 0x3c

  (* Block Information *)
  | BLOCKHASH -> 0x40 | COINBASE -> 0x41 | TIMESTAMP -> 0x42
  | NUMBER -> 0x43 | DIFFICULTY -> 0x44 | GASLIMIT -> 0x45

  (* Stack, Memory, Storage, and Flow Operations *)
  | POP -> 0x50 | MLOAD -> 0x51 | MSTORE -> 0x52 | MSTORE8 -> 0x53
  | SLOAD -> 0x54 | SSTORE -> 0x55 | JUMP -> 0x56 | JUMPI -> 0x57
  | PC -> 0x58 | MSIZE -> 0x59 | GAS -> 0x5a | JUMPDEST -> 0x5b

  (* Push, Duplicate, and Exchange *)
  | PUSH { n; _ } -> 0x60 + (n - 1)
  | DUP i -> 0x80 + (i - 1)
  | SWAP i -> 0x90 + (i - 1)

  (* Logging Operations *)
  | LOG i -> 0xa0 + i

  (* System Operations *)
  | CREATE -> 0xf0 | CALL -> 0xf1 | CALLCODE -> 0xf2 | RETURN -> 0xf3
  | DELEGATECALL -> 0xf4 | REVERT -> 0xf5 | INVALID -> 0xfe
  | SELFDESTRUCT -> 0xff

let delta = function
  (* Stop *)
  | STOP -> (0, 0)

  (* Arithmetic Operations *)
  | ADD -> (2, 1) | MUL -> (2, 1) | SUB -> (2, 1) | DIV -> (2, 1)
  | SDIV -> (2, 1) | MOD -> (2, 1) | SMOD -> (2, 1) | ADDMOD -> (3, 1)
  | MULMOD -> (3, 1) | EXP -> (2, 1) | SIGNEXTEND -> (2, 1)

  (* Comparison & Bitwise Logic Operations *)
  | LT -> (2, 1) | GT -> (2, 1) | SLT -> (2, 1) | SGT -> (2, 1)
  | EQ -> (2, 1) | ISZERO -> (1, 1) | AND -> (2, 1) | OR -> (2, 1)
  | XOR -> (2, 1) | NOT -> (1, 1) | BYTE -> (2, 1)

  (* SHA3 *)
  | SHA3 -> (2, 1)

  (* Environmental Information *)
  | ADDRESS -> (0, 1) | BALANCE -> (1, 1) | ORIGIN -> (0, 1)
  | CALLER -> (0, 1) | CALLVALUE -> (0, 1) | CALLDATALOAD -> (1, 1)
  | CALLDATASIZE -> (0, 1) | CALLDATACOPY -> (3, 0) | CODESIZE -> (0, 1)
  | CODECOPY -> (3, 0) | GASPRICE -> (0, 1) | EXTCODESIZE -> (1, 1)
  | EXTCODECOPY -> (4, 0)

  (* Block Information *)
  | BLOCKHASH -> (1, 1) | COINBASE -> (0, 1) | TIMESTAMP -> (0, 1)
  | NUMBER -> (0, 1) | DIFFICULTY -> (0, 1) | GASLIMIT -> (0, 1)

  (* Stack, Memory, Storage, and Flow Operations *)
  | POP -> (1, 0) | MLOAD -> (1, 1) | MSTORE -> (2, 0) | MSTORE8 -> (2, 0)
  | SLOAD -> (1, 1) | SSTORE -> (2, 0) | JUMP -> (1, 0) | JUMPI -> (2, 0)
  | PC -> (0, 1) | MSIZE -> (0, 1) | GAS -> (0, 1) | JUMPDEST -> (0, 0)

  (* Push, Duplicate, and Exchange *)
  | PUSH _ -> (0, 1)
  | DUP i -> (i, i+1)
  | SWAP i -> (i+1, i+1)

  (* Logging Operations *)
  | LOG i -> (i+2, 0)

  (* System Operations *)
  | CREATE -> (3, 1) | CALL -> (7, 1) | CALLCODE -> (7, 1) | RETURN -> (2, 0)
  | DELEGATECALL -> (6, 1) | REVERT -> (2, 0) | INVALID -> (0, 0)
  | SELFDESTRUCT -> (1, 0)

let instr_to_string = function
  (* Stop *)
  | STOP -> "STOP"

  (* Arithmetic Operations *)
  | ADD -> "ADD" | MUL -> "MUL" | SUB -> "SUB" | DIV -> "DIV"
  | SDIV -> "SDIV" | MOD -> "MOD" | SMOD -> "SMOD" | ADDMOD -> "ADDMOD"
  | MULMOD -> "MULMOD" | EXP -> "EXP" | SIGNEXTEND -> "SIGNEXTEND"

  (* Comparison & Bitwise Logic Operations *)
  | LT -> "LT" | GT -> "GT" | SLT -> "SLT" | SGT -> "SGT"
  | EQ -> "EQ" | ISZERO -> "ISZERO" | AND -> "AND" | OR -> "OR"
  | XOR -> "XOR" | NOT -> "NOT" | BYTE -> "BYTE"

  (* SHA3 *)
  | SHA3 -> "SHA3"

  (* Environmental Information *)
  | ADDRESS -> "ADDRESS" | BALANCE -> "BALANCE" | ORIGIN -> "ORIGIN"
  | CALLER -> "CALLER" | CALLVALUE -> "CALLVALUE" | CALLDATALOAD -> "CALLDATALOAD"
  | CALLDATASIZE -> "CALLDATASIZE" | CALLDATACOPY -> "CALLDATACOPY" | CODESIZE -> "CODESIZE"
  | CODECOPY -> "CODECOPY" | GASPRICE -> "GASPRICE" | EXTCODESIZE -> "EXTCODESIZE"
  | EXTCODECOPY -> "EXTCODECOPY"

  (* Block Information *)
  | BLOCKHASH -> "BLOCKHASH" | COINBASE -> "COINBASE" | TIMESTAMP -> "TIMESTAMP"
  | NUMBER -> "NUMBER" | DIFFICULTY -> "DIFFICULTY" | GASLIMIT -> "GASLIMIT"

  (* Stack, Memory, Storage, and Flow Operations *)
  | POP -> "POP" | MLOAD -> "MLOAD" | MSTORE -> "MSTORE" | MSTORE8 -> "MSTORE8"
  | SLOAD -> "SLOAD" | SSTORE -> "SSTORE" | JUMP -> "JUMP" | JUMPI -> "JUMPI"
  | PC -> "PC" | MSIZE -> "MSIZE" | GAS -> "GAS" | JUMPDEST -> "JUMPDEST"

  (* Push, Duplicate, and Exchange *)
  | PUSH { n; bytes; } -> Printf.sprintf "PUSH%d %s" n (Byte_seq.to_string bytes)
  | DUP n -> Printf.sprintf "DUP%d" n
  | SWAP n -> Printf.sprintf "SWAP%d" n

  (* Logging Operations *)
  | LOG n -> Printf.sprintf "LOG%d" n

  (* System Operations *)
  | CREATE -> "CREATE" | CALL -> "CALL" | CALLCODE -> "CALLCODE" | RETURN -> "RETURN"
  | DELEGATECALL -> "DELEGATECALL" | REVERT -> "REVERT" | INVALID -> "INVALID"
  | SELFDESTRUCT -> "SELFDESTRUCT"

type program = instr list

let to_bytes : program -> int list =
  let instr_to_bytes instr = match instr with
    | PUSH { bytes; _ } -> opcode instr :: Byte_seq.to_int_list bytes
    | _ -> [ opcode instr ]
  in Util.concat_map ~f:instr_to_bytes

let int64_to_bytes x =
  let bytes = Byte_seq.of_int64 x in
  let n = Byte_seq.length bytes in
  (bytes, n)

let push n =
  let (bytes, n) = int64_to_bytes n in
  PUSH { n; bytes }

let to_string program =
  String.concat "\n" (List.map instr_to_string program)

let to_hex_string program =
  String.concat "" (List.map (Printf.sprintf "%02x") (to_bytes program))
