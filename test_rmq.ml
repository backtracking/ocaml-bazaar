
open Format
open RMQ

let debug = ref false
let dprintf =
  kasprintf (fun s -> if !debug then print_endline s else ())

module Test(F : functor (E: OrderedType) -> RMQ with type elt = E.t) = struct

  module X = F(Int)

  let () =
    for n = 1 to 100 do
      dprintf "n=%d@." n;
      let a = Array.init n Fun.id in
      let t = X.create a in
      for j = 0 to n - 1 do
        dprintf "  j=%d@." j;
        if j > 0 then
          assert (X.rmq t ~lo:0 ~hi:j = 0);
        assert (X.rmq t ~lo:j ~hi:n = j);
        for k = j + 1 to n do
          dprintf "    k=%d@." k;
          assert (X.rmq t ~lo:j ~hi:k = j)
        done
      done
    done

  (* use Make0 as a reference *)
  module R = Make0(Int)

  let () =
    Random.init 42; (* each implementation tested on the same values *)
    for n = 1 to 100 do
      dprintf "n=%d@." n;
      let a = Array.init n (fun _ -> Random.int n) in
      let r = R.create a in
      let t = X.create a in
      for lo = 0 to n - 1 do
        for hi = lo + 1 to n do
          assert (X.rmq t ~lo ~hi = R.rmq r ~lo ~hi)
        done
      done
    done

end

module T0 = Test(RMQ.Make0)
module T1 = Test(RMQ.Make1)
module T2 = Test(RMQ.Make2)
