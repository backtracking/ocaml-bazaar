
(*
  Find five 5-letter words using 25 different letters.

  The "Jotto" problem.
  See https://digitalcommons.butler.edu/wordways/vol1/iss1/8/

  Where to five word files:
  https://github.com/stew675/standup5x5/blob/master/nyt_wordle.txt
  https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt

  Usage: this_program < word_file

  Algorithm:
  1. Builds a dictionary from masks to lists of words, where a mask
     is a 26-bit pattern (bit 0 for 'a', bit 1 for 'b', etc.).
     This dictionary is implemented as a Patricia tree (module `PT` below).
  2. Use backtracking, with the current patterns of letters already used,
     and efficient traversal of the Patricia tree portions that are
     still compatible with this pattern.

  Results are given at the end of this file. *)

open Format

module PT = struct

  type 'a t =
    | Empty
    | Leaf of int * 'a
    | Branch of int * int * 'a t * 'a t

  let zero_bit k m = (k land m) == 0

  let rec find k = function
    | Empty -> raise Not_found
    | Leaf (j,x) -> if k == j then x else raise Not_found
    | Branch (_, m, l, r) -> find k (if zero_bit k m then l else r)

  let lowest_bit x = x land (-x)

  let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

  let mask p m = p land (m-1)

  let join (p0,t0,p1,t1) =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then
      Branch (mask p0 m, m, t0, t1)
    else
      Branch (mask p0 m, m, t1, t0)

  let match_prefix k p m = (mask k m) == p

  let add k x t =
    let rec ins = function
      | Empty -> Leaf (k,x)
      | Leaf (j,_) as t ->
          if j == k then Leaf (k,x) else join (k, Leaf (k,x), j, t)
      | Branch (p,m,t0,t1) as t ->
          if match_prefix k p m then
            if zero_bit k m then
              Branch (p, m, ins t0, t1)
            else
              Branch (p, m, t0, ins t1)
          else
            join (k, Leaf (k,x), p, t)
    in
    ins t

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

  let rec iter used f = function
    | Empty -> ()
    | Leaf (k,x) -> if k land used = 0 then f k x
    | Branch (p,m,t0,t1) ->
        if used land p = 0 then iter used f t0;
        if used land (p lor m) = 0 then iter used f t1

end

let bit c = Char.code c - Char.code 'a'

let mask w =
  let add_bit m c = m lor (1 lsl (bit c)) in
  String.fold_left add_bit 0 w

let rec pop x = if x = 0 then 0 else 1 + pop (x - x land -x)

let words = ref PT.Empty

let count = ref 0

let add w =
  let m = mask w in
  if pop m = 5 then (
    incr count;
    let r = try PT.find m !words
            with Not_found -> let r = ref [] in words := PT.add m r !words; r in
    r := w :: !r
  )

let () =
  let rec fill () = match read_line () with
    | w -> if String.length w = 5 then add w; fill ()
    | exception End_of_file -> () in
  fill ();
  printf "%d words@." !count;
  printf "%d masks@." (PT.cardinal !words)

let print l =
  printf "{ @[%a@] }"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
       pp_print_string) l

let nbsol = ref 0

let rec solve sol used n =
  if n = 5 then (
    incr nbsol;
    List.iter print sol; printf "@."
  ) else
    PT.iter used
      (fun m r ->
        if m land used = 0 && (sol = [] || !r < List.hd sol) then
          solve (!r :: sol) (used lor m) (n + 1))
      !words

let () =
  solve [] 0 0;
  printf "%d solutions@." !nbsol

(*
https://github.com/stew675/standup5x5/blob/master/nyt_wordle.txt
3834 words
2845 masks
{ brung }{ kempt }{ vozhd }{ waqfs }{ xylic, cylix }
{ fjord }{ gucks }{ nymph }{ vibex }{ waltz }
{ chunk }{ fjord }{ gymps }{ vibex }{ waltz }
{ brick }{ glent }{ jumpy }{ vozhd }{ waqfs }
{ bemix }{ clunk }{ grypt }{ vozhd }{ waqfs }
{ blunk }{ cimex }{ grypt }{ vozhd }{ waqfs }
{ clipt }{ jumby }{ kreng }{ vozhd }{ waqfs }
{ glent }{ jumby }{ prick }{ vozhd }{ waqfs }
{ bling }{ jumpy }{ treck }{ vozhd }{ waqfs }
{ jumby }{ pling }{ treck }{ vozhd }{ waqfs }
real    1m11.933s

https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt
10175 words
5977 masks
{ bejig }{ fldxt }{ knyaz }{ mowch }{ supvr }
{ fldxt }{ ganch, chang }{ jimpy }{ uzbek }{ vrows }
{ chank }{ fldxt }{ jimpy }{ uzbeg }{ vrows }
{ bejig }{ chump }{ fldxt }{ knyaz }{ vrows }
{ bejig }{ fldxt }{ nymph }{ quack }{ vrows }
{ cheng }{ fldxt }{ jimpy }{ uzbak }{ vrows }
{ bewig }{ flock }{ hdqrs }{ japyx }{ muntz }
{ bowge }{ flick }{ hdqrs }{ japyx }{ muntz }
{ chivw }{ fldxt }{ grosz }{ jambe }{ punky }
{ chivw }{ fldxt }{ graze, gazer }{ jumby }{ knosp, knops }
{ bigam }{ expwy }{ fconv }{ hdqrs }{ klutz }
{ ampyx }{ bewig }{ fconv }{ hdqrs }{ klutz }
{ ampyx }{ bejig }{ fconv }{ hdqrs }{ klutz }
{ bewig }{ fconv }{ hdqrs }{ japyx }{ klutz }
{ bongs }{ chivw }{ fremd }{ japyx }{ klutz }
{ baken }{ chivw }{ fldxt }{ grosz }{ jumpy }
{ bronk }{ chivw }{ fldxt }{ gazes }{ jumpy }
{ chivw }{ expdt }{ flamb }{ grosz }{ junky }
{ benjy }{ chowk }{ fldxt }{ gramp }{ squiz }
{ bench }{ fldxt }{ gawks }{ jumpy }{ vizor }
{ bench }{ fldxt }{ gawky }{ jumps }{ vizor }
{ fldxt }{ gawby }{ jumps }{ kench }{ vizor }
{ benjy }{ chump }{ fldxt }{ gawks }{ vizor }
{ burgh, brugh }{ campy }{ fldxt }{ vejoz }{ winks, swink }
{ champ }{ fldxt }{ rugby }{ vejoz }{ winks, swink }
{ fldxt }{ gumby }{ parch }{ vejoz }{ winks, swink }
{ bumph }{ fldxt }{ gracy }{ vejoz }{ winks, swink }
{ chomp }{ fldxt }{ jarvy }{ uzbeg }{ winks, swink }
{ bawke }{ fultz }{ gconv }{ hdqrs }{ jimpy }
{ burez }{ fldxt }{ gconv }{ hawks }{ jimpy }
{ exptl }{ fconv }{ gawby }{ hdqrs }{ mujik }
{ bawke }{ fldxt }{ gconv }{ jimpy }{ qursh }
{ avick }{ benjy }{ fldxt }{ grosz }{ whump }
{ backs }{ fldxt }{ ringy, girny }{ vejoz }{ whump }
{ backy }{ fldxt }{ rings, grins, girns }{ vejoz }{ whump }
{ carby }{ fldxt }{ kings, ginks }{ vejoz }{ whump }
{ brisk, birks }{ cangy }{ fldxt }{ vejoz }{ whump }
{ bangs }{ fldxt }{ ricky }{ vejoz }{ whump }
{ bangy }{ fldxt }{ ricks }{ vejoz }{ whump }
{ fldxt }{ grabs, garbs, brags }{ nicky }{ vejoz }{ whump }
{ banks }{ fldxt }{ gyric }{ vejoz }{ whump }
{ barks }{ fldxt }{ gynic }{ vejoz }{ whump }
{ cribs }{ fldxt }{ kyang }{ vejoz }{ whump }
{ bingy }{ fldxt }{ racks, carks }{ vejoz }{ whump }
{ bejan }{ fldxt }{ grosz }{ vicky }{ whump }
{ fldxt }{ ginzo }{ jacks }{ verby }{ whump }
{ fldxt }{ ginzo }{ jacky }{ verbs }{ whump }
{ fldxt }{ gconv }{ jizya }{ krebs, kerbs }{ whump }
{ becks }{ fldxt }{ ginzo }{ jarvy }{ whump }
{ brisk, birks }{ fldxt }{ gconv }{ jazey }{ whump }
{ fldxt }{ gconv }{ jerky }{ sabzi }{ whump }
{ fldxt }{ gconv }{ jerky }{ saqib }{ whump }
{ burez }{ fldxt }{ jacks }{ vying }{ whomp }
{ carvy }{ fldxt }{ jinks }{ uzbeg }{ whomp }
{ fldxt }{ jarvy }{ snick, nicks }{ uzbeg }{ whomp }
{ brack }{ fldxt }{ mungy }{ vejoz }{ whisp, whips }
{ crank }{ fldxt }{ gumby }{ vejoz }{ whisp, whips }
{ crumb }{ fldxt }{ kyang }{ vejoz }{ whisp, whips }
{ fldxt }{ gconv }{ jumby }{ karez }{ whisp, whips }
{ backs }{ fldxt }{ grump }{ vejoz }{ whiny }
{ bucks }{ fldxt }{ gramp }{ vejoz }{ whiny }
{ backy }{ fldxt }{ grump }{ vejoz }{ whins }
{ bucky }{ fldxt }{ gramp }{ vejoz }{ whins }
{ brack }{ fldxt }{ pungy }{ vejoz }{ whims }
{ bucky }{ fldxt }{ prang }{ vejoz }{ whims }
{ bangy }{ crump }{ fldxt }{ vejoz }{ whisk }
{ bungy }{ cramp }{ fldxt }{ vejoz }{ whisk }
{ bumpy }{ crang }{ fldxt }{ vejoz }{ whisk }
{ expdt }{ furzy }{ gconv }{ jambs }{ whilk }
{ chivw }{ enzym }{ fldxt }{ jakob }{ sprug }
{ fldxt }{ jambe }{ qophs }{ vicky }{ wrung }
{ brack }{ fldxt }{ jives }{ whump }{ zygon }
{ becks }{ fldxt }{ rajiv }{ whump }{ zygon }
{ brevi }{ fldxt }{ jacks }{ whump }{ zygon }
{ chivw }{ fldxt }{ jambs }{ puker }{ zygon }
{ chivw }{ fldxt }{ jumba }{ perks }{ zygon }
{ bruja }{ chivw }{ fldxt }{ skemp, kemps }{ zygon }
{ bumph }{ fldxt }{ jacks }{ wrive, wiver }{ zygon }
{ bumph }{ fldxt }{ javer }{ wicks, swick }{ zygon }
{ behav }{ fldxt }{ jumps }{ wrick }{ zygon }
{ bumps }{ fldxt }{ jahve }{ wrick }{ zygon }
{ fldxt }{ jambe }{ supvr }{ whick }{ zygon }
{ breva, brave }{ fldxt }{ jumps }{ whick }{ zygon }
{ bumps }{ fldxt }{ javer }{ whick }{ zygon }
{ chimp }{ fldxt }{ quawk }{ verbs }{ zygon }
{ fldxt }{ quick }{ verbs }{ whamp }{ zygon }
{ bumph }{ fldxt }{ jives }{ wrack }{ zygon }
{ brevi }{ fldxt }{ jumps }{ whack, chawk }{ zygon }
{ chivw }{ fldxt }{ jumps }{ kebar, break, brake, baker }{ zygon }
{ fldxt }{ jimpy }{ schav }{ uzbek }{ wrong, grown }
{ chivw }{ fldxt }{ jumby }{ karez }{ spong }
{ ampyx }{ bortz }{ chivw }{ fjeld }{ gunks }
{ chivw }{ expdt }{ furzy }{ jambs }{ klong }
{ chowk }{ fldxt }{ jambe }{ supvr }{ zingy }
{ breva, brave }{ chowk }{ fldxt }{ jumps }{ zingy }
{ breva, brave }{ fldxt }{ jocks }{ whump }{ zingy }
{ fldxt }{ jacko }{ verbs }{ whump }{ zingy }
{ bocks }{ fldxt }{ javer }{ whump }{ zingy }
{ bevor }{ fldxt }{ jacks }{ whump }{ zingy }
{ fldxt }{ jerks }{ vocab }{ whump }{ zingy }
{ fldxt }{ quack }{ verbs }{ whomp }{ zingy }
{ bucks }{ fldxt }{ javer }{ whomp }{ zingy }
{ bumph }{ fldxt }{ jacks }{ vower }{ zingy }
{ fldxt }{ jumba }{ qophs }{ wreck }{ zingy }
{ fldxt }{ jumbo }{ pshav }{ wreck }{ zingy }
{ chomp }{ fldxt }{ quawk }{ verbs }{ zingy }
{ bumph }{ fldxt }{ jocks }{ waver, warve }{ zingy }
{ bumps }{ chowk }{ fldxt }{ javer }{ zingy }
{ bevor }{ fldxt }{ jumps }{ whack, chawk }{ zingy }
{ brack }{ fldxt }{ sumph, humps }{ vejoz }{ wingy }
{ backs }{ fldxt }{ murph }{ vejoz }{ wingy }
{ barks }{ chump }{ fldxt }{ vejoz }{ wingy }
{ brusk }{ champ }{ fldxt }{ vejoz }{ wingy }
{ brahm }{ fldxt }{ pucks }{ vejoz }{ wingy }
{ bucks }{ fldxt }{ prahm, pharm }{ vejoz }{ wingy }
{ crumb }{ fldxt }{ kaphs }{ vejoz }{ wingy }
{ bumph }{ fldxt }{ racks, carks }{ vejoz }{ wingy }
{ fldxt }{ rhumb }{ spack, packs }{ vejoz }{ wingy }
{ bumps }{ fldxt }{ karch, chark }{ vejoz }{ wingy }
{ crawm }{ fldxt }{ qophs }{ uzbek }{ vying }
{ expwy }{ flack }{ hdqrs }{ jumbo }{ vingt }
{ expwy }{ flock }{ hdqrs }{ jumba }{ vingt }
{ breva, brave }{ chowk }{ fldxt }{ jumpy }{ zings }
{ fldxt }{ jacko }{ verby }{ whump }{ zings }
{ bevor }{ fldxt }{ jacky }{ whump }{ zings }
{ fldxt }{ jerky }{ vocab }{ whump }{ zings }
{ fldxt }{ quack }{ verby }{ whomp }{ zings }
{ bucky }{ fldxt }{ javer }{ whomp }{ zings }
{ bumph }{ fldxt }{ jacko }{ wyver }{ zings }
{ chump }{ fldxt }{ jakob }{ wyver }{ zings }
{ bumph }{ fldxt }{ jacky }{ vower }{ zings }
{ chomp }{ fldxt }{ quawk }{ verby }{ zings }
{ chowk }{ fldxt }{ jumby }{ verpa, paver, parve }{ zings }
{ bumpy }{ chowk }{ fldxt }{ javer }{ zings }
{ bevor }{ fldxt }{ jumpy }{ whack, chawk }{ zings }
{ fldxt }{ jumby }{ prove }{ whack, chawk }{ zings }
{ brack }{ fldxt }{ humpy }{ vejoz }{ wings, swing }
{ backy }{ fldxt }{ murph }{ vejoz }{ wings, swing }
{ braky, barky }{ chump }{ fldxt }{ vejoz }{ wings, swing }
{ bucky }{ fldxt }{ prahm, pharm }{ vejoz }{ wings, swing }
{ bumpy }{ fldxt }{ karch, chark }{ vejoz }{ wings, swing }
{ chomp }{ fldxt }{ jarvy }{ uzbek }{ wings, swing }
{ backs }{ fldxt }{ humpy }{ vejoz }{ wring }
{ backy }{ fldxt }{ sumph, humps }{ vejoz }{ wring }
{ busky }{ champ }{ fldxt }{ vejoz }{ wring }
{ bucks }{ fldxt }{ phyma, pamhy, yamph }{ vejoz }{ wring }
{ bucky }{ fldxt }{ phasm, pashm }{ vejoz }{ wring }
{ bumps }{ fldxt }{ hacky, chyak }{ vejoz }{ wring }
{ bumpy }{ fldxt }{ shack, hacks }{ vejoz }{ wring }
{ breck }{ fldxt }{ jowpy }{ nizam, nazim }{ vughs }
{ bovld }{ freck }{ japyx }{ muntz }{ whigs }
{ banky }{ crump }{ fldxt }{ vejoz }{ whigs }
{ bumpy }{ crank }{ fldxt }{ vejoz }{ whigs }
{ brack }{ fldxt }{ spumy }{ vejoz }{ whing }
{ backs }{ fldxt }{ rumpy }{ vejoz }{ whing }
{ backy }{ fldxt }{ rumps }{ vejoz }{ whing }
{ brusk }{ campy }{ fldxt }{ vejoz }{ whing }
{ busky }{ cramp }{ fldxt }{ vejoz }{ whing }
{ barmy, ambry }{ fldxt }{ pucks }{ vejoz }{ whing }
{ bucky }{ fldxt }{ sparm, ramps, prams }{ vejoz }{ whing }
{ bumpy }{ fldxt }{ racks, carks }{ vejoz }{ whing }
{ bryum }{ fldxt }{ spack, packs }{ vejoz }{ whing }
{ burps }{ fldxt }{ mckay }{ vejoz }{ whing }
{ becks }{ fultz }{ japyx }{ mordv }{ whing }
{ fultz }{ jacky }{ mordv }{ pbxes }{ whing }
{ comps }{ fldxt }{ jarvy }{ uzbek }{ whing }
{ becks }{ fldxt }{ jumpy }{ voraz }{ whing }
{ becky }{ fldxt }{ jumps }{ voraz }{ whing }
{ fldxt }{ jumby }{ speck, pecks }{ voraz }{ whing }
{ bumph }{ fldxt }{ ginzo }{ jacks }{ wyver }
{ fldxt }{ gryph }{ jambs }{ quick }{ woven }
{ blitz }{ fconv }{ gawky }{ hdqrs }{ pumex }
{ chimb }{ fldxt }{ gawky }{ spurn, snurp }{ vejoz }
{ fldxt }{ gawby }{ kinch, chink }{ rumps }{ vejoz }
{ birch }{ fldxt }{ gawky }{ numps }{ vejoz }
{ chirk }{ fldxt }{ gawby }{ numps }{ vejoz }
{ chimp }{ fldxt }{ gawby }{ knurs }{ vejoz }
{ chirm }{ fldxt }{ gawby }{ spunk, punks }{ vejoz }
{ bunch }{ fldxt }{ gawks }{ primy }{ vejoz }
{ bunch }{ fldxt }{ gawky }{ prism, prims }{ vejoz }
{ chunk }{ fldxt }{ gawby }{ prism, prims }{ vejoz }
{ bhang }{ crump }{ fldxt }{ skiwy }{ vejoz }
{ bumph }{ crang }{ fldxt }{ skiwy }{ vejoz }
{ bunch }{ fldxt }{ gramp }{ skiwy }{ vejoz }
{ fldxt }{ gawby }{ munch }{ skirp }{ vejoz }
{ chawn }{ fldxt }{ gumby }{ skirp }{ vejoz }
{ chump }{ fldxt }{ gawby }{ rinks, kirns }{ vejoz }
{ churm }{ fldxt }{ gawby }{ spink, pinks }{ vejoz }
{ chums }{ fldxt }{ gawby }{ prink }{ vejoz }
{ fldxt }{ gawby }{ punch }{ smirk, mirks }{ vejoz }
{ burgh, brugh }{ cawny }{ fldxt }{ skimp }{ vejoz }
{ fldxt }{ gawby }{ runch, churn }{ skimp }{ vejoz }
{ chawn }{ fldxt }{ rugby }{ skimp }{ vejoz }
{ crink }{ fldxt }{ gawby }{ sumph, humps }{ vejoz }
{ braws }{ fldxt }{ gucki }{ nymph }{ vejoz }
{ crimp }{ fldxt }{ gawby }{ hunks }{ vejoz }
{ crump }{ fldxt }{ gawby }{ knish }{ vejoz }
{ braky, barky }{ chimp }{ fldxt }{ swung }{ vejoz }
{ brahm }{ fldxt }{ picky }{ swung }{ vejoz }
{ champ }{ fldxt }{ kirby, birky }{ swung }{ vejoz }
{ chimb }{ fldxt }{ parky }{ swung }{ vejoz }
{ brick }{ fldxt }{ phyma, pamhy, yamph }{ swung }{ vejoz }
{ chimb }{ fldxt }{ pawky }{ rungs }{ vejoz }
{ birch }{ fldxt }{ mawks }{ pungy }{ vejoz }
{ chimb }{ fldxt }{ rawky }{ spung, pungs }{ vejoz }
{ birch }{ fldxt }{ mawky }{ spung, pungs }{ vejoz }
{ braws }{ chimp }{ fldxt }{ kyung, gunky }{ vejoz }
{ bunch }{ fldxt }{ mawky }{ sprig, prigs, grips }{ vejoz }
{ crumb }{ fldxt }{ hawky }{ pings }{ vejoz }
{ braws }{ chunk }{ fldxt }{ pigmy, gimpy }{ vejoz }
{ brusk }{ chawn }{ fldxt }{ pigmy, gimpy }{ vejoz }
{ busky }{ chawn }{ fldxt }{ grimp }{ vejoz }
{ ampyx }{ crwth }{ fdubs }{ kling, glink }{ vejoz }
{ crumb }{ fldxt }{ pawky }{ singh, nighs }{ vejoz }
{ crwth }{ gambs }{ kylix }{ pfund }{ vejoz }
{ bumph }{ fldxt }{ gawky }{ scrin }{ vejoz }
{ flamb }{ hdqrs }{ pungy }{ twick }{ vejoz }
{ bangy }{ flump }{ hdqrs }{ twick }{ vejoz }
{ ampyx }{ flung }{ hdqrs }{ twick }{ vejoz }
{ bumpy }{ flang }{ hdqrs }{ twick }{ vejoz }
{ fangy }{ hdqrs }{ plumb }{ twick }{ vejoz }
{ bungy }{ hdqrs }{ lampf }{ twick }{ vejoz }
{ braws }{ fldxt }{ nymph }{ quick }{ vejoz }
{ fldxt }{ grabs, garbs, brags }{ nymph }{ quick }{ vejoz }
{ fldxt }{ gawby }{ murph }{ snick, nicks }{ vejoz }
{ fldxt }{ grimp }{ quawk }{ synch }{ vejoz }
{ brigs }{ fldxt }{ mawky }{ punch }{ vejoz }
{ brims }{ fldxt }{ gawky }{ punch }{ vejoz }
{ brawn }{ fldxt }{ pigmy, gimpy }{ shuck, hucks }{ vejoz }
{ fldxt }{ gawky }{ numbs }{ prich, chirp }{ vejoz }
{ bungs }{ fldxt }{ mawky }{ prich, chirp }{ vejoz }
{ bungy }{ fldxt }{ mawks }{ prich, chirp }{ vejoz }
{ bumps }{ fldxt }{ gawky }{ rinch }{ vejoz }
{ bumpy }{ fldxt }{ gawks }{ rinch }{ vejoz }
{ fldxt }{ gawby }{ murks }{ pinch }{ vejoz }
{ fldxt }{ grubs, burgs }{ mawky }{ pinch }{ vejoz }
{ bryum }{ fldxt }{ gawks }{ pinch }{ vejoz }
{ fldxt }{ gumby }{ prawn }{ shick, hicks }{ vejoz }
{ chimp }{ fldxt }{ gawks }{ runby, burny }{ vejoz }
{ fldxt }{ mawks }{ pinch }{ rugby }{ vejoz }
{ fldxt }{ gawks }{ nymph }{ urbic }{ vejoz }
{ brick }{ fldxt }{ nymph }{ squaw }{ vejoz }
{ burgh, brugh }{ fldxt }{ micky }{ spawn, pawns }{ vejoz }
{ chirk }{ fldxt }{ gumby }{ spawn, pawns }{ vejoz }
{ birch }{ fldxt }{ kyung, gunky }{ swamp }{ vejoz }
{ bungy }{ chirk }{ fldxt }{ swamp }{ vejoz }
{ burgh, brugh }{ fldxt }{ nicky }{ swamp }{ vejoz }
{ chung }{ fldxt }{ kirby, birky }{ swamp }{ vejoz }
{ fldxt }{ kinch, chink }{ rugby }{ swamp }{ vejoz }
{ bunch }{ fldxt }{ gimps }{ rawky }{ vejoz }
{ bungs }{ chimp }{ fldxt }{ rawky }{ vejoz }
{ bumps }{ ching }{ fldxt }{ rawky }{ vejoz }
{ brigs }{ fldxt }{ nymph }{ quawk }{ vejoz }
{ cribs }{ fldxt }{ nymph }{ quawk }{ vejoz }
{ bring }{ fldxt }{ psych }{ quawk }{ vejoz }
{ bring }{ chums }{ fldxt }{ pawky }{ vejoz }
{ bungs }{ chirm }{ fldxt }{ pawky }{ vejoz }
{ brims }{ chung }{ fldxt }{ pawky }{ vejoz }
{ brigs }{ fldxt }{ munch }{ pawky }{ vejoz }
{ bichy }{ fldxt }{ grump }{ swank }{ vejoz }
{ fldxt }{ gumby }{ prich, chirp }{ swank }{ vejoz }
{ bumph }{ fldxt }{ gyric }{ swank }{ vejoz }
{ chimp }{ fldxt }{ rugby }{ swank }{ vejoz }
{ burps }{ ching }{ fldxt }{ mawky }{ vejoz }
{ bunch }{ fldxt }{ gripy }{ mawks }{ vejoz }
{ bucky }{ fldxt }{ grimp }{ shawn }{ vejoz }
{ fldxt }{ gumby }{ prick }{ shawn }{ vejoz }
{ bungs }{ crimp }{ fldxt }{ hawky }{ vejoz }
{ bingy }{ crump }{ fldxt }{ hawks }{ vejoz }
{ bungy }{ crimp }{ fldxt }{ hawks }{ vejoz }
{ bumph }{ crink }{ fldxt }{ gawsy }{ vejoz }
{ brink }{ chump }{ fldxt }{ gawsy }{ vejoz }
{ brick }{ fldxt }{ nymph }{ quags }{ vejoz }
{ bucky }{ flimp }{ hdqrs }{ twang }{ vejoz }
{ bumpy }{ flick }{ hdqrs }{ twang }{ vejoz }
{ brick }{ fldxt }{ humpy }{ swang, gnaws }{ vejoz }
{ bumpy }{ chirk }{ fldxt }{ swang, gnaws }{ vejoz }
{ bumph }{ fldxt }{ ricky }{ swang, gnaws }{ vejoz }
{ chump }{ fldxt }{ kirby, birky }{ swang, gnaws }{ vejoz }
{ fldxt }{ picky }{ rhumb }{ swang, gnaws }{ vejoz }
{ brins }{ chump }{ fldxt }{ gawky }{ vejoz }
{ burns }{ chimp }{ fldxt }{ gawky }{ vejoz }
{ briny, birny }{ chump }{ fldxt }{ gawks }{ vejoz }
{ brigs }{ fldxt }{ nymph }{ quack }{ vejoz }
{ fldxt }{ gumby }{ prink }{ schwa, chwas, chaws }{ vejoz }
{ fldxt }{ gconv }{ hawks }{ jumby }{ prize }
{ bumph }{ fldxt }{ gravy }{ jocks }{ wizen, winze }
{ bumph }{ fldxt }{ grovy }{ jacks }{ wizen, winze }
{ fcomp }{ gawby }{ hdqrs }{ klutz }{ vixen }
{ bumph }{ fldxt }{ grosz }{ jacky }{ vinew }
{ brack }{ fldxt }{ jowpy }{ vughs }{ zemni, mizen }
{ fldxt }{ gconv }{ jimpy }{ uzbak }{ wersh, shrew }
{ dumbs }{ fritz }{ gconv }{ japyx }{ whelk }
{ bocks }{ flegm }{ japyx }{ thruv }{ windz }
{ block }{ fremt }{ japyx }{ vughs }{ windz }
{ frock }{ japyx }{ seqwl }{ vingt }{ zhmud }
{ brock }{ japyx }{ seqwl }{ vingt }{ zhmud }
{ frack }{ jowly }{ pbxes }{ vingt }{ zhmud }
{ frowl }{ jacky }{ pbxes }{ vingt }{ zhmud }
{ bleck }{ frows }{ japyx }{ vingt }{ zhmud }
{ becks }{ frowl }{ japyx }{ vingt }{ zhmud }
{ brock }{ flews }{ japyx }{ vingt }{ zhmud }
{ brews }{ flock }{ japyx }{ vingt }{ zhmud }
{ bowls, blows }{ freck }{ japyx }{ vingt }{ zhmud }
{ brows }{ fleck }{ japyx }{ vingt }{ zhmud }
{ breck }{ japyx }{ vingt }{ wolfs, fowls, flows }{ zhmud }
{ flong }{ jarvy }{ pbxes }{ twick }{ zhmud }
{ flong }{ japyx }{ twick }{ verbs }{ zhmud }
{ bilks }{ fconv }{ grewt }{ japyx }{ zhmud }
{ bring }{ fldxt }{ vejoz }{ whamp }{ yucks }
{ bumph }{ fldxt }{ gowks }{ javer }{ zincy }
{ brawn }{ fldxt }{ kopje }{ vughs }{ zymic }
{ bhang }{ fldxt }{ rumps }{ vejoz }{ wicky }
{ bargh }{ fldxt }{ numps }{ vejoz }{ wicky }
{ bangs }{ fldxt }{ murph }{ vejoz }{ wicky }
{ brahm }{ fldxt }{ spung, pungs }{ vejoz }{ wicky }
{ fldxt }{ graph }{ numbs }{ vejoz }{ wicky }
{ bungs }{ fldxt }{ prahm, pharm }{ vejoz }{ wicky }
{ bumph }{ fldxt }{ gnars }{ vejoz }{ wicky }
{ fldxt }{ rhumb }{ spang, pangs }{ vejoz }{ wicky }
{ bumph }{ fldxt }{ grosz }{ njave }{ wicky }
{ bhang }{ fldxt }{ rumpy }{ vejoz }{ wicks, swick }
{ bangy }{ fldxt }{ murph }{ vejoz }{ wicks, swick }
{ burga }{ fldxt }{ nymph }{ vejoz }{ wicks, swick }
{ brahm }{ fldxt }{ pungy }{ vejoz }{ wicks, swick }
{ fldxt }{ gryph }{ manqu }{ vejoz }{ wicks, swick }
{ bungy }{ fldxt }{ prahm, pharm }{ vejoz }{ wicks, swick }
{ bumph }{ fldxt }{ rangy, angry }{ vejoz }{ wicks, swick }
{ fldxt }{ gumby }{ njave }{ qophs }{ wrick }
{ bhang }{ fldxt }{ spumy }{ vejoz }{ wrick }
{ bangs }{ fldxt }{ humpy }{ vejoz }{ wrick }
{ bangy }{ fldxt }{ sumph, humps }{ vejoz }{ wrick }
{ bungs }{ fldxt }{ phyma, pamhy, yamph }{ vejoz }{ wrick }
{ bungy }{ fldxt }{ phasm, pashm }{ vejoz }{ wrick }
{ fldxt }{ nymph }{ quags }{ vejoz }{ wrick }
{ bumpy }{ fldxt }{ shang, sangh, hangs, gnash }{ vejoz }{ wrick }
{ fldxt }{ nymph }{ squab }{ vejoz }{ wrick }
{ benjy }{ fldxt }{ oghuz }{ vamps }{ wrick }
{ fldxt }{ jumby }{ qophs }{ vegan, ganev, evang }{ wrick }
{ fldxt }{ gumby }{ spark, parks }{ vejoz }{ winch }
{ busky }{ fldxt }{ gramp }{ vejoz }{ winch }
{ bezan }{ fldxt }{ grovy }{ jumps }{ whick }
{ bonze, benzo }{ fldxt }{ gravy }{ jumps }{ whick }
{ bangs }{ fldxt }{ rumpy }{ vejoz }{ whick }
{ bangy }{ fldxt }{ rumps }{ vejoz }{ whick }
{ fldxt }{ namby }{ sprug }{ vejoz }{ whick }
{ barms }{ fldxt }{ pungy }{ vejoz }{ whick }
{ barmy, ambry }{ fldxt }{ spung, pungs }{ vejoz }{ whick }
{ fldxt }{ gamps }{ runby, burny }{ vejoz }{ whick }
{ fldxt }{ grapy }{ numbs }{ vejoz }{ whick }
{ bungy }{ fldxt }{ sparm, ramps, prams }{ vejoz }{ whick }
{ bumps }{ fldxt }{ rangy, angry }{ vejoz }{ whick }
{ bumpy }{ fldxt }{ gnars }{ vejoz }{ whick }
{ bryum }{ fldxt }{ spang, pangs }{ vejoz }{ whick }
{ burps }{ fldxt }{ mangy }{ vejoz }{ whick }
{ fldxt }{ grosz }{ jumby }{ paven }{ whick }
{ bumpy }{ fldxt }{ grosz }{ njave }{ whick }
{ fldxt }{ jumby }{ navig }{ qophs }{ wreck }
{ fldxt }{ jumba }{ qophs }{ vying }{ wreck }
{ fldxt }{ ginzo }{ jumby }{ pshav }{ wreck }
{ fldxt }{ gryph }{ njave }{ quick }{ wombs }
{ chivw }{ fldxt }{ graze, gazer }{ jumpy }{ knobs, bonks }
{ chivw }{ fldxt }{ grump }{ jazey }{ knobs, bonks }
{ chivw }{ expdt }{ flank }{ grosz }{ jumby }
{ chivw }{ fjord }{ glaky }{ muntz }{ pbxes }
{ chivw }{ fjord }{ klutz }{ mangy }{ pbxes }
{ fldxt }{ ganch, chang }{ jowpy }{ mirvs }{ uzbek }
{ fldxt }{ jarvy }{ mowch }{ pings }{ uzbek }
{ fldxt }{ gimps }{ jarvy }{ nowch }{ uzbek }
{ chimp }{ fldxt }{ gowns }{ jarvy }{ uzbek }
{ glack }{ hdqrs }{ jowpy }{ muntz }{ vibex }
{ flack }{ hdqrs }{ jowpy }{ muntz }{ vibex }
{ dwarf }{ glyph }{ jocks }{ muntz }{ vibex }
{ flock }{ hdqrs }{ jumpy }{ twang }{ vibex }
{ chowk }{ fldxt }{ gravy }{ jumps }{ zineb, bizen }
{ fldxt }{ gravy }{ jocks }{ whump }{ zineb, bizen }
{ fldxt }{ grovy }{ jacks }{ whump }{ zineb, bizen }
{ fldxt }{ gucks }{ jarvy }{ whomp }{ zineb, bizen }
{ chump }{ fldxt }{ gowks }{ jarvy }{ zineb, bizen }
{ fldxt }{ grovy }{ jumps }{ whack, chawk }{ zineb, bizen }
{ chimp }{ fldxt }{ jarvy }{ swonk, snowk, knows }{ uzbeg }
{ chank }{ fldxt }{ jowpy }{ mirvs }{ uzbeg }
{ fldxt }{ jarvy }{ mowch }{ spink, pinks }{ uzbeg }
{ fldxt }{ jarvy }{ nowch }{ skimp }{ uzbeg }
{ flowk }{ hdqrs }{ imcnt }{ japyx }{ uzbeg }
{ chimb }{ fldxt }{ kyung, gunky }{ vejoz }{ wraps, warps }
{ fldxt }{ gumby }{ kinch, chink }{ vejoz }{ wraps, warps }
{ bichy }{ fldxt }{ gunks }{ vejoz }{ wramp }
{ bucks }{ fldxt }{ hying }{ vejoz }{ wramp }
{ bucky }{ fldxt }{ singh, nighs }{ vejoz }{ wramp }
{ busky }{ ching }{ fldxt }{ vejoz }{ wramp }
{ bingy }{ fldxt }{ shuck, hucks }{ vejoz }{ wramp }
{ bungs }{ fldxt }{ hicky }{ vejoz }{ wramp }
{ bungy }{ fldxt }{ shick, hicks }{ vejoz }{ wramp }
{ fldxt }{ johns }{ uzbeg }{ vicky }{ wramp }
{ fcomp }{ hdqrs }{ junky }{ vibex }{ waltz }
{ fjord }{ gucks }{ nymph }{ vibex }{ waltz }
{ fcomp }{ hdqrs }{ kyung, gunky }{ vibex }{ waltz }
{ chimb }{ fldxt }{ pungy }{ vejoz }{ warks }
{ bunch }{ fldxt }{ pigmy, gimpy }{ vejoz }{ warks }
{ bingy }{ chump }{ fldxt }{ vejoz }{ warks }
{ bungy }{ chimp }{ fldxt }{ vejoz }{ warks }
{ bumpy }{ ching }{ fldxt }{ vejoz }{ warks }
{ fldxt }{ gumby }{ pinch }{ vejoz }{ warks }
{ bumph }{ fldxt }{ gynic }{ vejoz }{ warks }
{ bench }{ fldxt }{ grosz }{ jimpy }{ quawk }
{ fldxt }{ gconv }{ herbs }{ jimpy }{ quawk }
{ benjy }{ chimp }{ fldxt }{ grosz }{ quawk }
{ bemix }{ fultz }{ gconv }{ hdqrs }{ pawky }
{ chivw }{ fldxt }{ grebo, gerbo }{ jumps }{ knyaz }
{ bumps }{ chivw }{ fldxt }{ jorge }{ knyaz }
{ chimb }{ fldxt }{ sprug }{ vejoz }{ wanky }
{ brigs }{ chump }{ fldxt }{ vejoz }{ wanky }
{ chimp }{ fldxt }{ grubs, burgs }{ vejoz }{ wanky }
{ fldxt }{ grimp }{ subch, chubs }{ vejoz }{ wanky }
{ blitz }{ fconv }{ gryph }{ judex }{ mawks }
{ bortz }{ chivw }{ dunks }{ flegm }{ japyx }
{ bumph }{ fldxt }{ gconv }{ jerky }{ swazi }
{ brick }{ fldxt }{ mungy }{ vejoz }{ whaps, pshaw }
{ bring }{ fldxt }{ mucky }{ vejoz }{ whaps, pshaw }
{ crink }{ fldxt }{ gumby }{ vejoz }{ whaps, pshaw }
{ bucks }{ fldxt }{ mingy }{ vejoz }{ wharp }
{ bingy }{ fldxt }{ mucks }{ vejoz }{ wharp }
{ fldxt }{ gumby }{ snick, nicks }{ vejoz }{ wharp }
{ bungs }{ fldxt }{ micky }{ vejoz }{ wharp }
{ bungy }{ fldxt }{ micks }{ vejoz }{ wharp }
{ brick }{ fldxt }{ pungy }{ vejoz }{ whams, shawm }
{ bungy }{ fldxt }{ prick }{ vejoz }{ whams, shawm }
{ fldxt }{ gconv }{ jerky }{ squiz }{ whamp }
{ benjy }{ fldxt }{ gucks }{ vizor }{ whamp }
{ burez }{ fldxt }{ jocks }{ vying }{ whamp }
{ cribs }{ fldxt }{ kyung, gunky }{ vejoz }{ whamp }
{ bucks }{ fldxt }{ ringy, girny }{ vejoz }{ whamp }
{ bucky }{ fldxt }{ rings, grins, girns }{ vejoz }{ whamp }
{ curby }{ fldxt }{ kings, ginks }{ vejoz }{ whamp }
{ bingy }{ fldxt }{ rucks }{ vejoz }{ whamp }
{ bungs }{ fldxt }{ ricky }{ vejoz }{ whamp }
{ bungy }{ fldxt }{ ricks }{ vejoz }{ whamp }
{ fldxt }{ grubs, burgs }{ nicky }{ vejoz }{ whamp }
{ fldxt }{ rugby }{ snick, nicks }{ vejoz }{ whamp }
{ briny, birny }{ fldxt }{ gucks }{ vejoz }{ whamp }
{ bunks }{ fldxt }{ gyric }{ vejoz }{ whamp }
{ brusk }{ fldxt }{ gynic }{ vejoz }{ whamp }
{ benjy }{ fldxt }{ grosz }{ quick }{ whamp }
{ fldxt }{ gconv }{ jerky }{ squib }{ whamp }
{ crumb }{ fldxt }{ gipsy }{ vejoz }{ whank }
{ curby }{ fldxt }{ gimps }{ vejoz }{ whank }
{ fldxt }{ gumby }{ scrip, crisp, crips }{ vejoz }{ whank }
{ bumps }{ fldxt }{ gyric }{ vejoz }{ whank }
{ fldxt }{ pigmy, gimpy }{ scrub, curbs }{ vejoz }{ whank }
{ brick }{ fldxt }{ vejoz }{ whump }{ yangs, gansy }
{ bumph }{ fldxt }{ vejoz }{ wrick }{ yangs, gansy }
{ chimb }{ expwy }{ fjord }{ klutz }{ vangs }
{ burez }{ chowk }{ fldxt }{ jimpy }{ vangs }
{ chowk }{ fldxt }{ jumby }{ prize }{ vangs }
{ chirm }{ fldxt }{ jowpy }{ uzbek }{ vangs }
{ busky }{ chimp }{ fldxt }{ vejoz }{ wrang }
{ fldxt }{ kumbi }{ psych }{ vejoz }{ wrang }
{ bumps }{ fldxt }{ hicky }{ vejoz }{ wrang }
{ bumpy }{ fldxt }{ shick, hicks }{ vejoz }{ wrang }
{ chivw }{ fldxt }{ jumby }{ perks }{ zogan, gazon }
{ bumps }{ chivw }{ fldxt }{ jerky }{ zogan, gazon }
{ bumpy }{ chivw }{ fldxt }{ jerks }{ zogan, gazon }
{ fldxt }{ jumps }{ verby }{ whick }{ zogan, gazon }
{ fldxt }{ jumpy }{ verbs }{ whick }{ zogan, gazon }
{ chivw }{ fldxt }{ jumpy }{ krebs, kerbs }{ zogan, gazon }
{ fldxt }{ jocks }{ verby }{ whump }{ zigan }
{ bumph }{ fldxt }{ jocks }{ wyver }{ zigan }
{ fldxt }{ jumby }{ qophs }{ wreck }{ zigan }
{ chowk }{ fldxt }{ jumps }{ verby }{ zigan }
{ chowk }{ fldxt }{ jumpy }{ verbs }{ zigan }
{ becks }{ fldxt }{ jumpy }{ vizor }{ whang }
{ becky }{ fldxt }{ jumps }{ vizor }{ whang }
{ fldxt }{ jumby }{ speck, pecks }{ vizor }{ whang }
{ brick }{ fldxt }{ spumy }{ vejoz }{ whang }
{ bucks }{ fldxt }{ primy }{ vejoz }{ whang }
{ bucky }{ fldxt }{ prism, prims }{ vejoz }{ whang }
{ crumb }{ fldxt }{ spiky, pisky }{ vejoz }{ whang }
{ curby }{ fldxt }{ skimp }{ vejoz }{ whang }
{ busky }{ crimp }{ fldxt }{ vejoz }{ whang }
{ bumps }{ fldxt }{ ricky }{ vejoz }{ whang }
{ bumpy }{ fldxt }{ ricks }{ vejoz }{ whang }
{ bryum }{ fldxt }{ spick, picks }{ vejoz }{ whang }
{ burps }{ fldxt }{ micky }{ vejoz }{ whang }
{ expdt }{ gconv }{ jumby }{ whilk }{ zarfs }
{ chivw }{ expdt }{ jumby }{ klong }{ zarfs }
{ exptl }{ gconv }{ hdqrs }{ jumby }{ kafiz }
{ exptl }{ gconv }{ hdqrs }{ jumby }{ wakif }
{ brock }{ fldxt }{ jimpy }{ vughs }{ wanze }
{ fldxt }{ ghbor, brogh, borgh }{ jumps }{ vicky }{ wanze }
{ bongs }{ chivw }{ fldxt }{ jumpy }{ karez }
{ chivw }{ fldxt }{ grosz }{ jumby }{ pekan, knape }
{ bring }{ fldxt }{ sumph, humps }{ vejoz }{ wacky, cawky }
{ bumph }{ fldxt }{ rings, grins, girns }{ vejoz }{ wacky, cawky }
{ fldxt }{ griph }{ numbs }{ vejoz }{ wacky, cawky }
{ fldxt }{ pings }{ rhumb }{ vejoz }{ wacky, cawky }
{ bring }{ fldxt }{ vejoz }{ whump }{ yacks, casky }
{ bumph }{ fldxt }{ vejoz }{ wring }{ yacks, casky }
{ bring }{ fldxt }{ humpy }{ vejoz }{ wacks, swack }
{ bingy }{ fldxt }{ murph }{ vejoz }{ wacks, swack }
{ bumph }{ fldxt }{ ringy, girny }{ vejoz }{ wacks, swack }
{ fldxt }{ gryph }{ minbu }{ vejoz }{ wacks, swack }
{ fjord }{ glyph }{ muntz }{ vibex }{ wacks, swack }
{ fldxt }{ given }{ jumby }{ qophs }{ wrack }
{ bonze, benzo }{ fldxt }{ jimpy }{ vughs }{ wrack }
{ bingy }{ fldxt }{ sumph, humps }{ vejoz }{ wrack }
{ fldxt }{ gibus }{ nymph }{ vejoz }{ wrack }
{ bumps }{ fldxt }{ hying }{ vejoz }{ wrack }
{ bumpy }{ fldxt }{ singh, nighs }{ vejoz }{ wrack }
{ fldxt }{ nymph }{ squib }{ vejoz }{ wrack }
{ fldxt }{ gumby }{ spink, pinks }{ vejoz }{ warch }
{ bungy }{ fldxt }{ skimp }{ vejoz }{ warch }
{ bunks }{ fldxt }{ pigmy, gimpy }{ vejoz }{ warch }
{ bumpy }{ fldxt }{ kings, ginks }{ vejoz }{ warch }
{ benjy }{ fldxt }{ gizmo }{ supvr }{ whack, chawk }
{ bring }{ fldxt }{ spumy }{ vejoz }{ whack, chawk }
{ bingy }{ fldxt }{ rumps }{ vejoz }{ whack, chawk }
{ fldxt }{ gumby }{ pirns }{ vejoz }{ whack, chawk }
{ bungs }{ fldxt }{ primy }{ vejoz }{ whack, chawk }
{ bungy }{ fldxt }{ prism, prims }{ vejoz }{ whack, chawk }
{ brims }{ fldxt }{ pungy }{ vejoz }{ whack, chawk }
{ bumps }{ fldxt }{ ringy, girny }{ vejoz }{ whack, chawk }
{ bumpy }{ fldxt }{ rings, grins, girns }{ vejoz }{ whack, chawk }
{ bryum }{ fldxt }{ pings }{ vejoz }{ whack, chawk }
{ burns }{ fldxt }{ pigmy, gimpy }{ vejoz }{ whack, chawk }
{ burps }{ fldxt }{ mingy }{ vejoz }{ whack, chawk }
{ fldxt }{ gimps }{ runby, burny }{ vejoz }{ whack, chawk }
{ fldxt }{ gripy }{ numbs }{ vejoz }{ whack, chawk }
{ fldxt }{ ginzo }{ jumps }{ verby }{ whack, chawk }
{ fldxt }{ ginzo }{ jumpy }{ verbs }{ whack, chawk }
{ chivw }{ fldxt }{ grype }{ junks }{ zambo }
{ chivw }{ fldxt }{ jerks }{ pungy }{ zambo }
{ chivw }{ fldxt }{ jerky }{ spung, pungs }{ zambo }
{ cheng }{ fldxt }{ jowpy }{ mirvs }{ uzbak }
{ dhikr }{ expwy }{ fultz }{ gconv }{ jambs }
{ fldxt }{ jocks }{ vying }{ whump }{ zebra, braze }
{ fldxt }{ gconv }{ jumpy }{ whisk }{ zebra, braze }
{ chowk }{ fldxt }{ jumps }{ vying }{ zebra, braze }
real	2m28.212s
*)
