module G = Grouper
module E = Evm

module S = Set.Make(struct
  type t = int
  let compare = (compare : int -> int -> int)
end)

module M = Map.Make(struct
  type t = int
  let compare = (compare : int -> int -> int)
end)

(** Perform only jump-destination insertion *)
let insert_jump_dests (p : G.program) : G.program =
  (* Accumulate locations of jump destinations.
   * A "jump destination" is an index representing which caml instruction
   * is being jumped to.
   *)
  let step ((dests, total_caml_len) : S.t * int) (group : G.group) =
    let offsets = Util.map_filter G.(group.instrs) ~f:(function
      | G.Evm _ -> None
      | G.Push_caml_code_offset i -> Some (total_caml_len + i))
    in
    let dests' = S.union dests (S.of_list offsets) in
    let total_caml_len' = total_caml_len + G.(group.caml_len) in
    (dests', total_caml_len')
  in

  (* Create set of destinations to jump to *)
  let (dests, _) = List.fold_left step (S.empty, 0) p in

  (* caml_len is 0 because a jumpdest instruction doesn't correspond to any
   * caml instructions *)
  let jump_dest = G.{
    caml_len = 0;
    instrs = [ G.Evm E.JUMPDEST ];
  } in

  let (rev_jump_dest_inserted, _) =
    List.fold_left (fun (acc, total_caml_len) group ->
      (* Always retain old instructions *)
      let acc' =
        if S.mem total_caml_len dests
        then group :: jump_dest :: acc
        else group :: acc (* Always retain old instructions *)
      in (acc', total_caml_len + G.(group.caml_len))
    ) ([], 0) p
  in

  List.rev rev_jump_dest_inserted

(** Remove Caml code offsets. Precondition: jump_dests are inserted. *)
let remove_code_offsets (p : G.program) : E.instr list list =

  (* Augment list with the running total of caml length *)
  let with_caml_lens = List.fold_left (fun (acc, total_caml_len) group ->
    let acc' = (group, total_caml_len) :: acc in
    (acc', total_caml_len + G.(group.caml_len)))
    ([], 0) p

      |> fst (* We only care about final value of list accumulator *)
      |> List.rev
  in

  (* A mapping from absolute Caml offsets to absolute byte offsets. *)
  let byte_lens = List.fold_left (fun acc (group, total_caml_len) ->
    let byte_len = List.length G.(group.instrs) in
    M.add total_caml_len byte_len acc
  ) M.empty with_caml_lens in

  with_caml_lens |> List.map (fun (group, total_caml_len) ->
    G.(group.instrs) |> List.map (fun instr ->
      match instr with
      | G.Evm i -> i
      | G.Push_caml_code_offset i ->
          let byte_offset = M.find (i + total_caml_len) byte_lens in
          E.push (Int64.of_int byte_offset)))

(** See MLI file *)
let process (p : G.program) : E.program =
  remove_code_offsets (insert_jump_dests p)
    |> List.concat
