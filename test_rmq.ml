
open Format

let debug = ref false
let dprintf =
  kasprintf (fun s -> if !debug then print_endline s else ())

module Test(X: RMQ.RMQ with type elt = int) = struct

  let () =
    for n = 9 to 100 do
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

end

module M0 = RMQ.Make0(Int)
module T0 = Test(M0)

module M1 = RMQ.Make1(Int)
module T1 = Test(M1)
