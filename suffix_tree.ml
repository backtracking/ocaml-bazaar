(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Suffix trees *)

(* Following Ukkonen's algorithm as described in Gusfield's book "Algorithms
   on Strings, Trees and Sequences" *)

module type Alphabet = sig
  type t
  val dummy : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val print :  Format.formatter -> t -> unit
  type s
  val length : s -> int
  val get : s -> int -> t
end

module type Branching = sig
  type key
  type 'a t
  val create : unit -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

let debug = false

module Make(A : Alphabet)(B : Branching with type key = A.t) = struct

  type node = {
    mutable label_start : int;
    mutable label_end : int;
    mutable father : node;
    mutable node_type : node_type;
    mutable suffix_link : node;
    path_position : int
  }

  and node_type = Leaf of int | Branch of node B.t

  type t = { tree_string : A.s; tree_root : node; }

  open Format

  let print fmt t =
    let m = A.length t.tree_string in
    let rec print_node n depth =
      if depth > 0 then begin
	for _i = 1 to depth - 1 do printf "|"; done;
	fprintf fmt "+";
	for i = n.label_start to n.label_end do
          A.print fmt (if i == m+1 then A.dummy else A.get t.tree_string (i-1))
	done;
	if debug then
          fprintf fmt "  \t\t\t(%d,%d)" n.label_start n.label_end;
	fprintf fmt "@\n"
      end;
      match n.node_type with
	| Leaf _ -> ()
	| Branch b -> B.iter (fun _ n -> print_node n (depth+1)) b
    in
    print_node t.tree_root 0

  (* a dummy node that will never appear in returned suffix trees *)
  let rec dummy_node =
    { label_start = 0; label_end = 0; father = dummy_node;
      node_type = Leaf (-1); suffix_link = dummy_node; path_position = 0 }

  let create_node ?(suffix_link=dummy_node) ~father ls le nt pp =
    { label_start = ls; label_end = le; father = father;
      node_type = nt; suffix_link = suffix_link; path_position = pp }

  type rule = Rule1 | Rule2 | Rule3

  type position = { mutable pos_node : node; mutable pos_in_edge : int }

  type extension_rule_2 = New_son | Split

  (* Ukkonen's algorithm *)
  let create s =
    let m = A.length s in
    (* we do not build [s$]; instead we override [get] to cover [1..m+1] *)
    let get i =
      assert (1 <= i && i <= m+1);
      if i <= m then A.get s (i-1) else A.dummy
    in
    (* the root tree and the first node *)
    let root_sons = B.create () in
    let rec root =
      { label_start = 0; label_end = 0; father = root;
	node_type = Branch root_sons; suffix_link = root; path_position = 0 }
    in
    let tree = { tree_string = s; tree_root = root } in
    let n = create_node ~suffix_link:root ~father:root 1 (m+1) (Leaf 1) 1 in
    B.add root_sons (get 1) n;
    (* label end: during construction label end of a leaf is [!e] *)
    let e = ref 0 in
    let label_end n = match n.node_type with
      | Leaf _ -> !e
      | Branch _ -> n.label_end
    in
    (* DEBUG *)
    let print fmt t =
      let m = A.length t.tree_string in
      let rec print_node n depth =
	let e = label_end n in
	if depth > 0 then begin
	  for _i = 1 to depth - 1 do printf "|"; done;
	  fprintf fmt "+";
	  for i = n.label_start to e do
            A.print fmt
	      (if i == m+1 then A.dummy else A.get t.tree_string (i-1))
	  done;
	  if debug then
            fprintf fmt "  \t\t\t(%d,%d)" n.label_start e;
	  fprintf fmt "@\n"
	end;
	match n.node_type with
	  | Leaf _ -> ()
	  | Branch b -> B.iter (fun _ n -> print_node n (depth+1)) b
      in
      print_node t.tree_root 0
    in
    (* END DEBUG *)
    let suffix_less = ref dummy_node in
    let set_suffix_link n =
      if !suffix_less != dummy_node then begin
	!suffix_less.suffix_link <- n;
	suffix_less := dummy_node
      end
    in
    let pos = { pos_node = root; pos_in_edge = 0 } in
    let label_length n = label_end n - n.label_start + 1 in
    let last_char_in_edge () =
      if debug then printf "last_char_in_edge = %b@."
	(pos.pos_in_edge = label_length pos.pos_node - 1);
      pos.pos_in_edge = label_length pos.pos_node - 1
    in
    (* select the right branch *)
    let find_son n c = match n.node_type with
      | Leaf _ -> dummy_node
      | Branch b -> (try B.find b c with Not_found -> dummy_node)
    in
    (* tracing a string down the tree (updating [pos]) *)
    let trace_single_edge (gs,ge) ~skip =
      let cont_node = find_son pos.pos_node (get gs) in
      if cont_node == dummy_node then begin
	pos.pos_in_edge <- label_length pos.pos_node - 1;
	0, true
      end else begin
	pos.pos_node <- cont_node;
	let length = label_length pos.pos_node in
	let str_len = ge - gs + 1 in
	if skip then begin
	  if length <= str_len then begin
	    pos.pos_in_edge <- length - 1;
	    length, not (length < str_len)
	  end else begin
	    pos.pos_in_edge <- str_len - 1;
	    str_len, true
	  end
	end else begin
	  let length = if str_len < length then str_len else length in
	  pos.pos_in_edge <- 1;
	  let rec loop chars_found =
	    if pos.pos_in_edge < length then begin
              if not (A.equal
			(get (pos.pos_node.label_start + pos.pos_in_edge))
			(get (gs + pos.pos_in_edge)))
	      then begin
		pos.pos_in_edge <- pos.pos_in_edge - 1;
		chars_found, true
	      end else begin
		pos.pos_in_edge <- pos.pos_in_edge + 1;
		loop (succ chars_found)
	      end
	    end else begin
	      pos.pos_in_edge <- pos.pos_in_edge - 1;
	      chars_found, not (chars_found < str_len)
	    end
	  in
	  loop 1
	end
      end
    in
    let trace_string gamma ~skip =
      let rec trace chars_found ((gs,ge) as g) =
	pos.pos_in_edge <- 0;
	let edge_chars_found, search_done = trace_single_edge g ~skip in
	let chars_found = chars_found + edge_chars_found in
	if not search_done then
	  trace chars_found (gs + edge_chars_found, ge)
	else
	  chars_found
      in
      trace 0 gamma
    in
    (* following the suffix link *)
    let follow_suffix_link () =
      if pos.pos_node != root then begin
	if pos.pos_node.suffix_link == dummy_node || not (last_char_in_edge ())
	then begin
	  if pos.pos_node.father == root then
	    pos.pos_node <- root
	  else begin
	    let s = pos.pos_node.label_start in
	    let gamma = (s, s + pos.pos_in_edge) in
	    pos.pos_node <- pos.pos_node.father.suffix_link;
	    ignore (trace_string gamma ~skip:true)
	  end
        end else begin
	  pos.pos_node <- pos.pos_node.suffix_link;
	  pos.pos_in_edge <- label_length pos.pos_node - 1
	end
      end
    in
    (* node creation (Rule 2) *)
    let apply_extension_rule_2 ls le path_pos edge_pos = function
      | New_son ->
	  if debug then printf "rule 2: new leaf (%d,%d)@." ls le;
	  let leaf =
	    create_node ls le ~father:pos.pos_node (Leaf path_pos) path_pos
	  in
	  begin match pos.pos_node.node_type with
	    | Branch b -> B.add b (get ls) leaf; leaf
	    | Leaf _ -> assert false
	  end
      | Split ->
	  if debug then printf "rule 2: split (%d,%d)@." ls le;
	  let node = pos.pos_node in
	  let b = B.create () in
	  let intl =
	    create_node node.label_start (node.label_start + edge_pos)
	      ~father:node.father (Branch b) node.path_position
	  in
	  node.label_start <- node.label_start + edge_pos + 1;
	  let leaf = create_node ~father:intl ls le (Leaf path_pos) path_pos in
	  B.add b (get node.label_start) node;
	  B.add b (get ls) leaf;
	  node.father <- intl;
	  begin match intl.father.node_type with
	    | Branch b -> B.add b (get intl.label_start) intl; intl
	    | Leaf _ -> assert false
	  end
    in
    (* Single Extension Algorithm (SEA) *)
    let sea j0 i_1 after_rule_3 =
      if debug then begin
	printf "%a@." print tree;
	printf "extension: %d  phase+1: %d" j0 i_1;
	if after_rule_3 then
	  printf "   starting at (%d,%d | %d)\n" pos.pos_node.label_start
	    (label_end pos.pos_node) pos.pos_in_edge
	else
	  printf "   followed from (%d,%d | %d)\n" pos.pos_node.label_start
	    (label_end pos.pos_node) pos.pos_in_edge
      end;
      let j = ref j0 in
      if not after_rule_3 then follow_suffix_link ();
      let chars_found =
	if pos.pos_node == root then begin
	  trace_string (!j,i_1) ~skip:false
	end else begin
	  j := i_1;
	  if last_char_in_edge () then begin
            let tmp = find_son pos.pos_node (get i_1) in
            if tmp != dummy_node then begin
              pos.pos_node <- tmp;
              pos.pos_in_edge <- 0;
              1
            end else
	      0
	  end else
	    if
	      A.equal (get (pos.pos_node.label_start + pos.pos_in_edge + 1))
		(get i_1)
	    then begin
	      pos.pos_in_edge <- pos.pos_in_edge + 1;
              1
	    end else
	      0
	end
      in
      if debug then printf "chars_found = %d@." chars_found;
      if chars_found = i_1 - !j + 1 then begin
	(* Rule 3 applies *)
        if debug then printf "rule 3 (%d,%d)@." !j i_1;
	set_suffix_link pos.pos_node.father;
	Rule3
      end else if last_char_in_edge () || pos.pos_node == root then
	match pos.pos_node.node_type with
	  | Branch _b ->
	      ignore
		(apply_extension_rule_2 (!j + chars_found) i_1 j0 0 New_son);
	      set_suffix_link pos.pos_node;
	      Rule2
	  | Leaf _ ->
	      Rule1
      else begin
	let tmp =
	  apply_extension_rule_2 (!j+chars_found) i_1 j0 pos.pos_in_edge Split
	in
	if !suffix_less != dummy_node then !suffix_less.suffix_link <- tmp;
	if label_length tmp = 1 && tmp.father == root then begin
	  tmp.suffix_link <- root;
	  suffix_less := dummy_node
	end else
	  suffix_less := tmp;
	pos.pos_node <- tmp;
	Rule2
      end
    in
    (* Ukkonen main loop *)
    let extension = ref 2 in
    let last_rule_is_3 = ref false in
    for i = 2 to m do (* phase [i+1] *)
      e := i+1;
      let rec spa () =
	if !extension <= i+1 then begin
	  let rule = sea !extension (i+1) !last_rule_is_3 in
	  last_rule_is_3 := (rule = Rule3);
	  if not !last_rule_is_3 then begin incr extension; spa () end
	end
      in
      spa ()
    done;
    let rec set_leaf_end n = match n.node_type with
      | Leaf _ -> n.label_end <- m+1
      | Branch b -> B.iter (fun _ n -> set_leaf_end n) b
    in
    set_leaf_end root;
    tree

  let find t s =
    let m = A.length t.tree_string in
    let p = A.length s in
    let get i =
      assert (1 <= i && i <= m+1);
      if i <= m then A.get t.tree_string (i-1) else A.dummy
    in
    let find_son n c = match n.node_type with
      | Leaf _ -> raise Not_found
      | Branch b -> B.find b c
    in
    let rec descend n j =
      let rec descend_edge j k =
	if j < p && k <= n.label_end && A.equal (get k) (A.get s j) then
	  descend_edge (succ j) (succ k)
	else if j = p then
	  { pos_node = n; pos_in_edge = k }
	else if k > n.label_end then
	  descend (find_son n (A.get s j)) j
	else
	  raise Not_found
      in
      descend_edge j n.label_start
    in
    let n0 = match t.tree_root.node_type with
      | Branch b -> B.find b (if p = 0 then A.dummy else A.get s 0)
      | Leaf _ -> assert false
    in
    descend n0 0

  let substring t s =
    if A.length s = 0 then
      0
    else
      let p = find t s in
      p.pos_node.path_position - 1

  let leaves f p =
    let rec iter n = match n.node_type with
      | Leaf j -> f (j - 1)
      | Branch b -> B.iter (fun _ n -> iter n) b
    in
    iter p.pos_node

end


(* some usual branching implementations *)

module Bmap(X : Map.OrderedType) : Branching with type key = X.t =
struct
  type key = X.t
  module M = Map.Make(X)
  type 'a t = 'a M.t ref
  let create () = ref M.empty
  let add m k v = m := M.add k v !m
  let find m k = M.find k !m
  let iter f m = M.iter f !m
end

module Barray(A : sig val size : int end) : Branching with type key = int =
struct
  type key = int
  type 'a t = 'a option array
  let create () = Array.make A.size None
  let add m k v = m.(k) <- Some v
  let find m k = match m.(k) with Some v -> v | None -> raise Not_found
  let iter f m =
    Array.iteri (fun k v -> match v with None -> () | Some v -> f k v) m
end

module Blist(X : sig type t val equal : t -> t -> bool end)
  : Branching with type key = X.t =
struct
  type key = X.t
  type 'a t = (key * 'a) list ref
  let create () = ref []
  let add m k v =
    let rec replace = function
      | [] -> raise Not_found
      | (a,_ as p) :: l -> if X.equal k a then (a,v) :: l else p :: replace l
    in
    m := try replace !m with Not_found -> (k,v) :: !m
  let find m k =
    let rec lookup = function
      | [] -> raise Not_found
      | (a,b) :: l -> if X.equal k a then b else lookup l
    in
    lookup !m
  let iter f m = List.iter (fun (a,b) -> f a b) !m
end

module Bhash(A : sig val size : int end)(X : Hashtbl.HashedType)
  : Branching with type key = X.t =
struct
  type key = X.t
  module H = Hashtbl.Make(X)
  type 'a t = 'a H.t
  let create () = H.create A.size
  let add m k v = H.replace m k v
  let find m k = H.find m k
  let iter = H.iter
end

module CharString = struct
  type t = char
  let dummy = Char.chr 0
  let equal x y = x == y
  let compare x y = Char.code x - Char.code y
  let _hash c = Char.code c
  let print fmt c = Format.fprintf fmt "%c" c
  type s = string
  let length = String.length
  let get = String.unsafe_get
end

(*
module IntArray = struct
  type t = int
  let dummy = -1
  let equal x y = x == y
  let compare = compare
  let hash c = Char.code c
  let print fmt c = Format.fprintf fmt "%d" c
  type s = int array
  let length = Array.length
  let get = Array.unsafe_get
end
 *)

(* manually defunctorized code for type string *)
module Ukkonen = struct

  (*module B = Bhash(struct let size = 17 end)(CharString)*)
  module B = Bmap(CharString)

  type node = {
    mutable label_start : int;
    mutable label_end : int;
    mutable father : node;
    mutable node_type : node_type;
    mutable suffix_link : node;
    path_position : int;
  }

  and node_type = Leaf of int | Branch of node B.t

  type t = { tree_string : string; tree_root : node; }

  open Format

  let print fmt t =
    let m = String.length t.tree_string in
    let rec print_node n depth =
      if depth > 0 then begin
	for _i = 1 to depth - 1 do printf "|"; done;
	fprintf fmt "+";
	for i = n.label_start to n.label_end do
          fprintf fmt "%c"
	    (if i == m+1 then '$' else String.unsafe_get t.tree_string (i-1))
	done;
	if debug then
          fprintf fmt "  \t\t\t(%d,%d)" n.label_start n.label_end;
	fprintf fmt "@\n"
      end;
      match n.node_type with
	| Leaf _ -> ()
	| Branch b -> B.iter (fun _ n -> print_node n (depth+1)) b
    in
    print_node t.tree_root 0

  (* a dummy node that will never appear in returned suffix trees *)
  let rec dummy_node =
    { label_start = 0; label_end = 0; father = dummy_node;
      node_type = Leaf (-1); suffix_link = dummy_node; path_position = 0 }

  let create_node ?(suffix_link=dummy_node) ~father ls le nt pp =
    { label_start = ls; label_end = le; father = father;
      node_type = nt; suffix_link = suffix_link; path_position = pp }

  type rule = Rule1 | Rule2 | Rule3

  type position = { mutable pos_node : node; mutable pos_in_edge : int }

  type extension_rule_2 = New_son | Split

  (* Ukkonen's algorithm *)
  let create s =
    let m = String.length s in
    let s0 = Bytes.make (m+2) '\000' in
    String.blit s 0 s0 1 m;
    (* we do not build [s$]; instead we override [get] to cover [1..m+1] *)
    let get i =
      assert (1 <= i && i <= m+1);
      Bytes.unsafe_get s0 i
    in
    (* the root tree and the first node *)
    let root_sons = B.create () in
    let rec root =
      { label_start = 0; label_end = 0; father = root;
	node_type = Branch root_sons; suffix_link = root; path_position = 0 }
    in
    let tree = { tree_string = s; tree_root = root } in
    let n = create_node ~suffix_link:root ~father:root 1 (m+1) (Leaf 1) 1 in
    B.add root_sons (get 1) n;
    (* label end: during construction label end of a leaf is [!e] *)
    let e = ref 0 in
    let label_end n = match n.node_type with
      | Leaf _ -> !e
      | Branch _ -> n.label_end
    in
    (* DEBUG *)
    let print fmt t =
      let m = String.length t.tree_string in
      let rec print_node n depth =
	let e = label_end n in
	if depth > 0 then begin
	  for _i = 1 to depth - 1 do printf "|"; done;
	  fprintf fmt "+";
	  for i = n.label_start to e do
            fprintf fmt "%c"
	      (if i == m+1 then '$' else String.unsafe_get t.tree_string (i-1))
	  done;
	  if debug then
            fprintf fmt "  \t\t\t(%d,%d)" n.label_start e;
	  fprintf fmt "@\n"
	end;
	match n.node_type with
	  | Leaf _ -> ()
	  | Branch b -> B.iter (fun _ n -> print_node n (depth+1)) b
      in
      print_node t.tree_root 0
    in
    (* END DEBUG *)
    let suffix_less = ref dummy_node in
    let set_suffix_link n =
      if !suffix_less != dummy_node then begin
	!suffix_less.suffix_link <- n;
	suffix_less := dummy_node
      end
    in
    let pos = { pos_node = root; pos_in_edge = 0 } in
    let label_length n = label_end n - n.label_start + 1 in
    let last_char_in_edge () =
      if debug then printf "last_char_in_edge = %b@."
	(pos.pos_in_edge = label_length pos.pos_node - 1);
      pos.pos_in_edge = label_length pos.pos_node - 1
    in
    (* select the right branch *)
    let find_son n c = match n.node_type with
      | Leaf _ -> dummy_node
      | Branch b -> (try B.find b c with Not_found -> dummy_node)
    in
    (* tracing a string down the tree (updating [pos]) *)
    let trace_single_edge (gs,ge) ~skip =
      let cont_node = find_son pos.pos_node (Bytes.get s0 gs) in
      if cont_node == dummy_node then begin
	pos.pos_in_edge <- label_length pos.pos_node - 1;
	0, true
      end else begin
	pos.pos_node <- cont_node;
	let length = label_length pos.pos_node in
	let str_len = ge - gs + 1 in
	if skip then begin
	  if length <= str_len then begin
	    pos.pos_in_edge <- length - 1;
	    length, not (length < str_len)
	  end else begin
	    pos.pos_in_edge <- str_len - 1;
	    str_len, true
	  end
	end else begin
	  let length = if str_len < length then str_len else length in
	  pos.pos_in_edge <- 1;
	  let rec loop chars_found =
	    if pos.pos_in_edge < length then begin
              if not (Bytes.get s0 (pos.pos_node.label_start + pos.pos_in_edge) ==
			Bytes.get s0 (gs + pos.pos_in_edge))
	      then begin
		pos.pos_in_edge <- pos.pos_in_edge - 1;
		chars_found, true
	      end else begin
		pos.pos_in_edge <- pos.pos_in_edge + 1;
		loop (succ chars_found)
	      end
	    end else begin
	      pos.pos_in_edge <- pos.pos_in_edge - 1;
	      chars_found, not (chars_found < str_len)
	    end
	  in
	  loop 1
	end
      end
    in
    let trace_string gamma ~skip =
      let rec trace chars_found ((gs,ge) as g) =
	pos.pos_in_edge <- 0;
	let edge_chars_found, search_done = trace_single_edge g ~skip in
	let chars_found = chars_found + edge_chars_found in
	if not search_done then
	  trace chars_found (gs + edge_chars_found, ge)
	else
	  chars_found
      in
      trace 0 gamma
    in
    (* following the suffix link *)
    let follow_suffix_link () =
      if pos.pos_node != root then begin
	if pos.pos_node.suffix_link == dummy_node || not (last_char_in_edge ())
	then begin
	  if pos.pos_node.father == root then
	    pos.pos_node <- root
	  else begin
	    let s = pos.pos_node.label_start in
	    let gamma = (s, s + pos.pos_in_edge) in
	    pos.pos_node <- pos.pos_node.father.suffix_link;
	    ignore (trace_string gamma ~skip:true)
	  end
        end else begin
	  pos.pos_node <- pos.pos_node.suffix_link;
	  pos.pos_in_edge <- label_length pos.pos_node - 1
	end
      end
    in
    (* node creation (Rule 2) *)
    let apply_extension_rule_2 ls le path_pos edge_pos = function
      | New_son ->
	  if debug then printf "rule 2: new leaf (%d,%d)@." ls le;
	  let leaf =
	    create_node ls le ~father:pos.pos_node (Leaf path_pos) path_pos
	  in
	  begin match pos.pos_node.node_type with
	    | Branch b -> B.add b (Bytes.get s0 ls) leaf; leaf
	    | Leaf _ -> assert false
	  end
      | Split ->
	  if debug then printf "rule 2: split (%d,%d)@." ls le;
	  let node = pos.pos_node in
	  let b = B.create () in
	  let intl =
	    create_node node.label_start (node.label_start + edge_pos)
	      ~father:node.father (Branch b) node.path_position
	  in
	  node.label_start <- node.label_start + edge_pos + 1;
	  let leaf = create_node ~father:intl ls le (Leaf path_pos) path_pos in
	  B.add b (Bytes.get s0 (node.label_start)) node;
	  B.add b (Bytes.get s0 ls) leaf;
	  node.father <- intl;
	  begin match intl.father.node_type with
	    | Branch b -> B.add b (Bytes.get s0 intl.label_start) intl; intl
	    | Leaf _ -> assert false
	  end
    in
    (* Single Extension Algorithm (SEA) *)
    let sea j0 i_1 after_rule_3 =
      if debug then begin
	printf "%a@." print tree;
	printf "extension: %d  phase+1: %d" j0 i_1;
	if after_rule_3 then
	  printf "   starting at (%d,%d | %d)\n" pos.pos_node.label_start
	    (label_end pos.pos_node) pos.pos_in_edge
	else
	  printf "   followed from (%d,%d | %d)\n" pos.pos_node.label_start
	    (label_end pos.pos_node) pos.pos_in_edge
      end;
      let j = ref j0 in
      if not after_rule_3 then follow_suffix_link ();
      let chars_found =
	if pos.pos_node == root then begin
	  trace_string (!j,i_1) ~skip:false
	end else begin
	  j := i_1;
	  if last_char_in_edge () then begin
            let tmp = find_son pos.pos_node (Bytes.get s0 i_1) in
            if tmp != dummy_node then begin
              pos.pos_node <- tmp;
              pos.pos_in_edge <- 0;
              1
            end else
	      0
	  end else
	    if
	      Bytes.get s0 (pos.pos_node.label_start + pos.pos_in_edge + 1) ==
		Bytes.get s0 i_1
	    then begin
	      pos.pos_in_edge <- pos.pos_in_edge + 1;
              1
	    end else
	      0
	end
      in
      if debug then printf "chars_found = %d@." chars_found;
      if chars_found = i_1 - !j + 1 then begin
	(* Rule 3 applies *)
        if debug then printf "rule 3 (%d,%d)@." !j i_1;
	set_suffix_link pos.pos_node.father;
	Rule3
      end else if last_char_in_edge () || pos.pos_node == root then
	match pos.pos_node.node_type with
	  | Branch _b ->
	      ignore
		(apply_extension_rule_2 (!j + chars_found) i_1 j0 0 New_son);
	      set_suffix_link pos.pos_node;
	      Rule2
	  | Leaf _ ->
	      Rule1
      else begin
	let tmp =
	  apply_extension_rule_2 (!j+chars_found) i_1 j0 pos.pos_in_edge Split
	in
	if !suffix_less != dummy_node then !suffix_less.suffix_link <- tmp;
	if label_length tmp = 1 && tmp.father == root then begin
	  tmp.suffix_link <- root;
	  suffix_less := dummy_node
	end else
	  suffix_less := tmp;
	pos.pos_node <- tmp;
	Rule2
      end
    in
    (* Ukkonen main loop *)
    let extension = ref 2 in
    let last_rule_is_3 = ref false in
    for i = 2 to m do (* phase [i+1] *)
      e := i+1;
      let rec spa () =
	if !extension <= i+1 then begin
	  let rule = sea !extension (i+1) !last_rule_is_3 in
	  last_rule_is_3 := (rule = Rule3);
	  if not !last_rule_is_3 then begin incr extension; spa () end
	end
      in
      spa ()
    done;
    let rec set_leaf_end n = match n.node_type with
      | Leaf _ -> n.label_end <- m+1
      | Branch b -> B.iter (fun _ n -> set_leaf_end n) b
    in
    set_leaf_end root;
    tree

  let dummy_char = Char.chr 0

  let find t s =
    let m = String.length t.tree_string in
    let p = String.length s in
    let get i =
      assert (1 <= i && i <= m+1);
      if i <= m then String.unsafe_get t.tree_string (i-1) else dummy_char
    in
    let find_son n c = match n.node_type with
      | Leaf _ -> raise Not_found
      | Branch b -> B.find b c
    in
    let rec descend n j =
      let rec descend_edge j k =
	if j < p && k <= n.label_end && (get k) == (String.unsafe_get s j) then
	  descend_edge (succ j) (succ k)
	else if j = p then
	  { pos_node = n; pos_in_edge = k }
	else if k > n.label_end then
	  descend (find_son n (String.unsafe_get s j)) j
	else
	  raise Not_found
      in
      descend_edge j n.label_start
    in
    let n0 = match t.tree_root.node_type with
      | Branch b ->
	  B.find b (if p = 0 then dummy_char else String.unsafe_get s 0)
      | Leaf _ -> assert false
    in
    descend n0 0

  let substring t s =
    if String.length s = 0 then
      0
    else
      let p = find t s in
      p.pos_node.path_position - 1

  let leaves f p =
    let rec iter n = match n.node_type with
      | Leaf j -> f (j - 1)
      | Branch b -> B.iter (fun _ n -> iter n) b
    in
    iter p.pos_node

  (* ne fonctionne que si [B = Bmap] car il faut que [iter] soit lexic. *)
  let suffix_array t =
    let m = String.length t.tree_string in
    let rec iter n = match n.node_type with
      | Leaf j -> printf "%s\n" (String.sub t.tree_string (j-1) (m-j+1))
      | Branch b -> B.iter (fun _ n -> iter n) b
    in
    iter t.tree_root


end
