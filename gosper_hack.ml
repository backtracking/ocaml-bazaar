
(* Gosper's Hack

   Iterates over all n-bit words with k bits sets, in lexicographic
   order and in linear time, with O(1) cheap operations to move from
   one word to the next.

   Said otherwise, iterates over all subsets of {0,1,...,n-1} of
   size k, where each subset is represented as a bit vector.

   Principle: Identify the rightmost segment of 1 bits,
                                  i
         w = ?????????000000111111100000
                            <- m ->

   Isolate its rightmost bit, at position i, with w&-w:
                                  i
         c = 000000000000000000000100000

   Add it to w, to get the following:
                                  i
         r = ?????????000001000000000000
                            <- m ->

   XOR r and w, to get the following:
                                  i
       r^w = 000000000000001111111100000
                           <  m+1 >

   Shift this to the right i+2 times (using shift and division by c):
                                  i
             00000000000000000000111111
                                 < m-1>

   Finally, OR this with r to get the expected word:

             ?????????00000100000111111
                                 < m-1>

   Works for 0 <= k <= n <= Sys.int_size.

   Gosper's Hack is explained in Hacker's Delight (Henri Warren,
   Addison-Wesley, 2013); see pages 14--15.
*)

let next w =
  let c = w land -w in
  let r = w + c in
  (((r lxor w) lsr 2) / c) lor r

let iter n k f =
  if n < 0 || n > Sys.int_size || k < 0 || k > n then invalid_arg "iter";
  let first = 1 lsl k - 1 in
  let last = first lsl (n - k) in
  let rec loop w = f w; if w <> last then loop (next w) in
  loop first
