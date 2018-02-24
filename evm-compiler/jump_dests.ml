module G = Grouper
module E = Evm
module L = Label

module M = Map.Make(struct
  type t = int
  let compare = (compare : int -> int -> int)
end)

type t =
  | Evm of Evm.instr
  | Label of L.t
  | Goto of L.t

type program = (t G.group * int) list

let to_string (p : program) : string =
  let instr_to_string = function
    | Evm e -> "\t" ^ E.instr_to_string e
    | Label lbl -> "\tlabel " ^ string_of_int (L.to_int lbl)
    | Goto lbl -> "\tgoto " ^ string_of_int (L.to_int lbl)
  in
  let group_to_string (g, total_caml_len) : string =
    Printf.sprintf "Group (%d; total: %d):\n%s"
      G.(g.caml_len) total_caml_len
      (List.map instr_to_string G.(g.instrs)
        |> String.concat "\n")
  in
  String.concat "\n" (List.map group_to_string p)

(** See MLI file *)
let insert_labels (p : G.program) : program =
  (* Accumulate locations of jump destinations.
   * A "jump destination" is an index representing which caml instruction
   * is being jumped to.
   *)
  let step (dests : L.t M.t) ((g, total_caml_len) : G.t G.group * int) =

    (* Perform jump from offset of NEXT caml instruction *)
    let n = total_caml_len + G.(g.caml_len) in

    let step_instr (dests : L.t M.t) : G.t -> L.t M.t = function
      | G.Evm _ -> dests
      | G.Push_caml_code_offset i ->
          let abs_offset = n + i in
          if M.mem abs_offset dests then dests
          else M.add abs_offset (L.create ()) dests
    in
    List.fold_left step_instr dests G.(g.instrs)
  in

  (* Create set of destinations to jump to *)
  let dests = List.fold_left step M.empty p in

  (* caml_len is 0 because a jumpdest instruction doesn't correspond to any
   * caml instructions *)
  let jump_dest lbl = G.{
    caml_len = 0;
    instrs = [ Label lbl; ];
  } in

  Util.concat_map p ~f:(fun (g, total_caml_len) ->
    let n = total_caml_len + G.(g.caml_len) in
    let g' =
      { g with
          G.instrs =
            List.map (function
              | G.Evm i -> Evm i
              | G.Push_caml_code_offset i ->
                  Goto (M.find (n+i) dests))
            G.(g.instrs) }
   in

   (* Insert jump destination if we are the destination of a jump *)
   if M.mem total_caml_len dests
   then [ (jump_dest (M.find total_caml_len dests), total_caml_len);
          (g', total_caml_len) ]
   else [ (g', total_caml_len) ]
  )

let add_label map i instr =
  match instr with
  | Label lbl -> L.Map.add lbl i map
  | _ -> map

let instr_size = function
  (* Goto involves pushing a byte onto the stack *)
  (* We allocate 4 bytes (i.e. PUSH3 ....), which allows
   * for up to 2^24 different labels. *)
  | Goto _ -> 4
  | Label _ -> 1
  | Evm instr -> E.instr_size instr

let repeat n x =
  let rec loop i acc = match i with
    | 0 -> acc
    | _ -> loop (i-1) (x :: acc)
  in loop n []

(** See MLI file *)
let remove_labels (p : program) : E.instr list list =

  (* First, create a map from labels to absolute EVM offsets. *)
  let (label_map, _) = List.fold_left (fun (map, total_byte_len) (g, _) ->
      List.fold_left (fun (map, i) instr ->
          let size = instr_size instr in
          (add_label map i instr, i + size))
        (map, total_byte_len) G.(g.instrs))
    (L.Map.empty, 0) p
  in

  Util.concat_map p ~f:(fun (g, _) ->
    List.map (function
      | Evm i -> [ i ]
      | Goto lbl ->
          let i = L.Map.find lbl label_map in
          let push = E.push (Int64.of_int i) in
          let padding = 4 - E.instr_size push in

          (* JUMPDEST is a noop *)
          push :: repeat padding E.JUMPDEST

      | Label lbl -> [ E.JUMPDEST ])
    G.(g.instrs))

(** See MLI file *)
let process (p : G.program) : E.program =
  insert_labels p
    |> remove_labels
    |> List.concat
