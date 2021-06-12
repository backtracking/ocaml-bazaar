
(* Test fenwick trees *)

module type FT = sig
  type t
  val create: int -> t
  val add: t -> delta:int -> int -> unit
  val prefix_sum: t -> int -> int
  val between: t -> int -> int -> int
  val get: t -> int -> int
end

module Test(F: FT) = struct
  for len = 0 to 100 do
    let t = F.create len in
    assert (F.prefix_sum t len = 0);
    for i = 0 to len do
      F.add t ~delta:1 i;
      assert (F.prefix_sum t i = i + 1);
      assert (F.get t i = 1);
      assert (F.prefix_sum t len = i + 1)
    done;
    for i = 0 to len do
      assert (F.get t i = 1);
      F.add t ~delta:(-1) i;
      assert (F.prefix_sum t i = 0);
      assert (F.get t i = 0);
      assert (F.prefix_sum t len = len - i)
    done
  done
end

include Test(Fenwick)
