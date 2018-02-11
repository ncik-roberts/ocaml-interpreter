(**
 * Byte code instructions from:
 *   http://cadmium.x9c.fr/distrib/caml-instructions.pdf
 *)
type op =
  (* Peek nth element from stack and put it into accumulator *)
  | ACC
  | ACC0 | ACC1 | ACC2 | ACC3 | ACC4 | ACC5 | ACC6 | ACC7

  (* Push the accumulator onto the stack, and then peek the (n+1)th element
   * into the accumulator position. *)
  | PUSHACC
  | PUSHACC0 | PUSHACC1 | PUSHACC2 | PUSHACC3 | PUSHACC4
  | PUSHACC5 | PUSHACC6 | PUSHACC7
  (* Same as PUSHACC0 *)
  | PUSH

  (* Perform unary operator on accumulator. *)
  | NEGINT | BOOLNOT

  (* Binary operator on accumulator and top of stack; pop element from stack
   * and save result of operation in accumulator *)
  | ADDINT | SUBINT | MULINT | DIVINT | MODINT
  | ANDINT | ORINT | XORINT | LSLINT | ASRINT | LSRINT
  | GEINT | LTINT | LEINT | GTINT
  | NEQ | EQ

  (* Pop n items from stack *)
  | POP

  (* Set nth item of stack to accumulator; set accumulator to unit value *)
  | ASSIGN

  (* Set accumulator to nth index of environment field *)
  | ENVACC
  | ENVACC1 | ENVACC2 | ENVACC3 | ENVACC4

  (* Set nth index of environment field to accumulator *)
  | PUSHENVACC
  | PUSHENVACC1 | PUSHENVACC2 | PUSHENVACC3 | PUSHENVACC4

  (* Push return address (which is the location of the program counter plus
   * the current value stored at the program counter).
   * Also push the environment and the extrArgs
   *)
  | PUSH_RETADDR

  (* Saves n-1 to extraArgs, sets pc to the accumulator, and sets the
   * environment to the value of the accumulator. *)
  | APPLY
  (* Pushes extraArgs, env, and pc to stack behind n items; saves n-1 to
   * extraArgs *)
  | APPLY1 | APPLY2 | APPLY3

  (* Params: nargs, slotsize
   * Takes the top nargs items on the stack and slides them downwards
   * nargs - slotsize positions, overwriting slotsize many elements.
   *
   * Then, set pc to code value of accumulator,
   * environment to accumulator,
   * and increase extraArgs by (n-1)
   *)
  | APPTERM

  (* Params: n
   * Peeks top i elements, pops ensuing n-i items, and replace top i elements
   * on stack. Then, set pc to code value of accumulator,
   * environment to accumulator,
   * and increase extraArgs by i-1
   *)
  | APPTERM1 | APPTERM2 | APPTERM3

  (* Params: n
   * Pops n elements from the stack, and does some fancy messing around.
   *)
  | RETURN

  (* Pushes elements of the environment onto stack, except for first element
   * of environment, which is saved as new environment. *)
  | RESTART

  (* Params: n
   * Cases on whether extraArgs is >= or < n. >= is better; otherwise, we create
   * a closure of extraArgs+3, popping many elements from the stack, and putting
   * this closure in the accumulator.
   *)
  | GRAB

  (* Put closure in accumulator based on n elements in stack *)
  | CLOSURE | CLOSUREREC

  (* Access closure at different places in the environment *)
  | OFFSETCLOSUREM2 | OFFSETCLOSURE | OFFSETCLOSURE0 | OFFSETCLOSURE2

  (* Access closure while pushing accumulator *)
  | PUSHOFFSETCLOSURE | PUSHOFFSETCLOSURE0 | PUSHOFFSETCLOSURE2 | PUSHOFFSETCLOSUREM2

  (* Get element in global array *)
  | GETGLOBAL | PUSHGETGLOBAL

  (* Get field of element in global array *)
  | GETGLOBALFIELD | PUSHGETGLOBALFIELD

  (* Set the field of index n in the global data to the acc *)
  | SETGLOBAL

  (* Set the accumulator to the nth (0th) atom *)
  | ATOM | ATOM0

  (* Equivalent to PUSH then ATOM / ATOM0 *)
  | PUSHATOM | PUSHATOM0

  (* Create a block of n (i) elements and put in accumulator *)
  | MAKEBLOCK | MAKEBLOCK1 | MAKEBLOCK2 | MAKEBLOCK3

  (* Make float block of n values *)
  | MAKEFLOATBLOCK

  (* Get field at index n (i) of accumulator *)
  | GETFIELD | GETFIELD0 | GETFIELD1 | GETFIELD2 | GETFIELD3

  (* Get the float field at index n (i) of accumulator*)
  | GETFLOATFIELD

  (* Sets the nth (ith) field of block, popping value off stack. *)
  | SETFIELD | SETFIELD0 | SETFIELD1 | SETFIELD2 | SETFIELD3

  (* Sets the nth field to float at top of stack *)
  | SETFLOATFIELD

  (* Sets the accumulator to length of block *)
  | VECTLENGTH

  (* Access nth vector item of block in the acc *)
  | GETVECTITEM

  (* Pops two elements from stack, n then v.
   * Set field n to v; setting acc to unit value.
   *)
  | SETVECTITEM

  (* Get nth character of string *)
  | GETSTRINGCHAR

  (* Set nth character of string to v *)
  | SETSTRINGCHAR

  (* Performs an unconditional jump by adding ofs to pc. *)
  | BRANCH

  (* Branch based on value of acc *)
  | BRANCHIF | BRANCHIFNOT

  (* ??? *)
  | SWITCH

  (* Pushes extraArgs, etc. onto stack, and sets trackSp to sp *)
  | PUSHTRAP

  (* Pop an element *)
  | POPTRAP

  (* Raise an exception *)
  | RAISE

  (* Handle signals *)
  | CHECK_SIGNALS

  (* Calls a c function with n parameters *)
  | C_CALL1 | C_CALL2 | C_CALL3 | C_CALL4 | C_CALL5 | C_CALLN

  (* Sets the accumulator to n *)
  | CONST0 | CONST1 | CONST2 | CONST3 | CONSTINT

  (* Push acc to stack and set acc to n *)
  | PUSHCONST0 | PUSHCONST1 | PUSHCONST2 | PUSHCONST3 | PUSHCONSTINT

  (* Add ofs to the accumulator, or to set the field of the acc *)
  | OFFSETINT | OFFSETREF

  (* Set acc to 1 or 0 depending on whether top of stack is long value *)
  | ISINT

  (* Set acc to x(0)(z), where x is top of stack and z is acc *)
  | GETMETHOD

  (* Increment pc by ofs - 1 if val has correct relationship to acc *)
  | BEQ | BNEQ
  | BGEINT | BGTINT | BLEINT | BLTINT
  | BUGEINT | BULTINT

  (* Set the accumulator to a non-zero value or to zero whether the
   * accumulator is lower than the value popped from the stack *)
  | UGEINT | ULTINT

  (* Get the method with the tag *)
  | GETPUBMET

  (* Get the method for the class *)
  | GETDYNMET

  (* Stop the execution of the program *)
  | STOP

  (* Send message to debugger *)
  | BREAK | EVENT

let to_opcode = function
  | ACC0 -> 0
  | ACC1 -> 1
  | ACC2 -> 2
  | ACC3 -> 3
  | ACC4 -> 4
  | ACC5 -> 5
  | ACC6 -> 6
  | ACC7 -> 7
  | ACC -> 8
  | PUSH -> 9
  | PUSHACC0 -> 10
  | PUSHACC1 -> 11
  | PUSHACC2 -> 12
  | PUSHACC3 -> 13
  | PUSHACC4 -> 14
  | PUSHACC5 -> 15
  | PUSHACC6 -> 16
  | PUSHACC7 -> 17
  | PUSHACC -> 18
  | POP -> 19
  | ASSIGN -> 20
  | ENVACC1 -> 21
  | ENVACC2 -> 22
  | ENVACC3 -> 23
  | ENVACC4 -> 24
  | ENVACC -> 25
  | PUSHENVACC1 -> 26
  | PUSHENVACC2 -> 27
  | PUSHENVACC3 -> 28
  | PUSHENVACC4 -> 29
  | PUSHENVACC -> 30
  | PUSH_RETADDR -> 31
  | APPLY -> 32
  | APPLY1 -> 33
  | APPLY2 -> 34
  | APPLY3 -> 35
  | APPTERM -> 36
  | APPTERM1 -> 37
  | APPTERM2 -> 38
  | APPTERM3 -> 39
  | RETURN -> 40
  | RESTART -> 41
  | GRAB -> 42
  | CLOSURE -> 43
  | CLOSUREREC -> 44
  | OFFSETCLOSUREM2 -> 45
  | OFFSETCLOSURE0 -> 46
  | OFFSETCLOSURE2 -> 47
  | OFFSETCLOSURE -> 48
  | PUSHOFFSETCLOSUREM2 -> 49
  | PUSHOFFSETCLOSURE0 -> 50
  | PUSHOFFSETCLOSURE2 -> 51
  | PUSHOFFSETCLOSURE -> 52
  | GETGLOBAL -> 53
  | PUSHGETGLOBAL -> 54
  | GETGLOBALFIELD -> 55
  | PUSHGETGLOBALFIELD -> 56
  | SETGLOBAL -> 57
  | ATOM0 -> 58
  | ATOM -> 59
  | PUSHATOM0 -> 60
  | PUSHATOM -> 61
  | MAKEBLOCK -> 62
  | MAKEBLOCK1 -> 63
  | MAKEBLOCK2 -> 64
  | MAKEBLOCK3 -> 65
  | MAKEFLOATBLOCK -> 66
  | GETFIELD0 -> 67
  | GETFIELD1 -> 68
  | GETFIELD2 -> 69
  | GETFIELD3 -> 70
  | GETFIELD -> 71
  | GETFLOATFIELD -> 72
  | SETFIELD0 -> 73
  | SETFIELD1 -> 74
  | SETFIELD2 -> 75
  | SETFIELD3 -> 76
  | SETFIELD -> 77
  | SETFLOATFIELD -> 78
  | VECTLENGTH -> 79
  | GETVECTITEM -> 80
  | SETVECTITEM -> 81
  | GETSTRINGCHAR -> 82
  | SETSTRINGCHAR -> 83
  | BRANCH -> 84
  | BRANCHIF -> 85
  | BRANCHIFNOT -> 86
  | SWITCH -> 87
  | BOOLNOT -> 88
  | PUSHTRAP -> 89
  | POPTRAP -> 90
  | RAISE -> 91
  | CHECK_SIGNALS -> 92
  | C_CALL1 -> 93
  | C_CALL2 -> 94
  | C_CALL3 -> 95
  | C_CALL4 -> 96
  | C_CALL5 -> 97
  | C_CALLN -> 98
  | CONST0 -> 99
  | CONST1 -> 100
  | CONST2 -> 101
  | CONST3 -> 102
  | CONSTINT -> 103
  | PUSHCONST0 -> 104
  | PUSHCONST1 -> 105
  | PUSHCONST2 -> 106
  | PUSHCONST3 -> 107
  | PUSHCONSTINT -> 108
  | NEGINT -> 109
  | ADDINT -> 110
  | SUBINT -> 111
  | MULINT -> 112
  | DIVINT -> 113
  | MODINT -> 114
  | ANDINT -> 115
  | ORINT -> 116
  | XORINT -> 117
  | LSLINT -> 118
  | LSRINT -> 119
  | ASRINT -> 120
  | EQ -> 121
  | NEQ -> 122
  | LTINT -> 123
  | LEINT -> 124
  | GTINT -> 125
  | GEINT -> 126
  | OFFSETINT -> 127
  | OFFSETREF -> 128
  | ISINT -> 129
  | GETMETHOD -> 130
  | BEQ -> 131
  | BNEQ -> 132
  | BLTINT -> 133
  | BLEINT -> 134
  | BGTINT -> 135
  | BGEINT -> 136
  | ULTINT -> 137
  | UGEINT -> 138
  | BULTINT -> 139
  | BUGEINT -> 140
  | GETPUBMET -> 141
  | GETDYNMET -> 142
  | STOP -> 143
  | EVENT -> 144
  | BREAK -> 145

let of_opcode = function
  | 0 -> ACC0
  | 1 -> ACC1
  | 2 -> ACC2
  | 3 -> ACC3
  | 4 -> ACC4
  | 5 -> ACC5
  | 6 -> ACC6
  | 7 -> ACC7
  | 8 -> ACC
  | 9 -> PUSH
  | 10 -> PUSHACC0
  | 11 -> PUSHACC1
  | 12 -> PUSHACC2
  | 13 -> PUSHACC3
  | 14 -> PUSHACC4
  | 15 -> PUSHACC5
  | 16 -> PUSHACC6
  | 17 -> PUSHACC7
  | 18 -> PUSHACC
  | 19 -> POP
  | 20 -> ASSIGN
  | 21 -> ENVACC1
  | 22 -> ENVACC2
  | 23 -> ENVACC3
  | 24 -> ENVACC4
  | 25 -> ENVACC
  | 26 -> PUSHENVACC1
  | 27 -> PUSHENVACC2
  | 28 -> PUSHENVACC3
  | 29 -> PUSHENVACC4
  | 30 -> PUSHENVACC
  | 31 -> PUSH_RETADDR
  | 32 -> APPLY
  | 33 -> APPLY1
  | 34 -> APPLY2
  | 35 -> APPLY3
  | 36 -> APPTERM
  | 37 -> APPTERM1
  | 38 -> APPTERM2
  | 39 -> APPTERM3
  | 40 -> RETURN
  | 41 -> RESTART
  | 42 -> GRAB
  | 43 -> CLOSURE
  | 44 -> CLOSUREREC
  | 45 -> OFFSETCLOSUREM2
  | 46 -> OFFSETCLOSURE0
  | 47 -> OFFSETCLOSURE2
  | 48 -> OFFSETCLOSURE
  | 49 -> PUSHOFFSETCLOSUREM2
  | 50 -> PUSHOFFSETCLOSURE0
  | 51 -> PUSHOFFSETCLOSURE2
  | 52 -> PUSHOFFSETCLOSURE
  | 53 -> GETGLOBAL
  | 54 -> PUSHGETGLOBAL
  | 55 -> GETGLOBALFIELD
  | 56 -> PUSHGETGLOBALFIELD
  | 57 -> SETGLOBAL
  | 58 -> ATOM0
  | 59 -> ATOM
  | 60 -> PUSHATOM0
  | 61 -> PUSHATOM
  | 62 -> MAKEBLOCK
  | 63 -> MAKEBLOCK1
  | 64 -> MAKEBLOCK2
  | 65 -> MAKEBLOCK3
  | 66 -> MAKEFLOATBLOCK
  | 67 -> GETFIELD0
  | 68 -> GETFIELD1
  | 69 -> GETFIELD2
  | 70 -> GETFIELD3
  | 71 -> GETFIELD
  | 72 -> GETFLOATFIELD
  | 73 -> SETFIELD0
  | 74 -> SETFIELD1
  | 75 -> SETFIELD2
  | 76 -> SETFIELD3
  | 77 -> SETFIELD
  | 78 -> SETFLOATFIELD
  | 79 -> VECTLENGTH
  | 80 -> GETVECTITEM
  | 81 -> SETVECTITEM
  | 82 -> GETSTRINGCHAR
  | 83 -> SETSTRINGCHAR
  | 84 -> BRANCH
  | 85 -> BRANCHIF
  | 86 -> BRANCHIFNOT
  | 87 -> SWITCH
  | 88 -> BOOLNOT
  | 89 -> PUSHTRAP
  | 90 -> POPTRAP
  | 91 -> RAISE
  | 92 -> CHECK_SIGNALS
  | 93 -> C_CALL1
  | 94 -> C_CALL2
  | 95 -> C_CALL3
  | 96 -> C_CALL4
  | 97 -> C_CALL5
  | 98 -> C_CALLN
  | 99 -> CONST0
  | 100 -> CONST1
  | 101 -> CONST2
  | 102 -> CONST3
  | 103 -> CONSTINT
  | 104 -> PUSHCONST0
  | 105 -> PUSHCONST1
  | 106 -> PUSHCONST2
  | 107 -> PUSHCONST3
  | 108 -> PUSHCONSTINT
  | 109 -> NEGINT
  | 110 -> ADDINT
  | 111 -> SUBINT
  | 112 -> MULINT
  | 113 -> DIVINT
  | 114 -> MODINT
  | 115 -> ANDINT
  | 116 -> ORINT
  | 117 -> XORINT
  | 118 -> LSLINT
  | 119 -> LSRINT
  | 120 -> ASRINT
  | 121 -> EQ
  | 122 -> NEQ
  | 123 -> LTINT
  | 124 -> LEINT
  | 125 -> GTINT
  | 126 -> GEINT
  | 127 -> OFFSETINT
  | 128 -> OFFSETREF
  | 129 -> ISINT
  | 130 -> GETMETHOD
  | 131 -> BEQ
  | 132 -> BNEQ
  | 133 -> BLTINT
  | 134 -> BLEINT
  | 135 -> BGTINT
  | 136 -> BGEINT
  | 137 -> ULTINT
  | 138 -> UGEINT
  | 139 -> BULTINT
  | 140 -> BUGEINT
  | 141 -> GETPUBMET
  | 142 -> GETDYNMET
  | 143 -> STOP
  | 144 -> EVENT
  | 145 -> BREAK
  | _ -> assert false
