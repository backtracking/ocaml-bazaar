
module Test(X: RMQ.RMQ with type elt = int) = struct

  let () =
    for n = 1 to 100 do
      let a = Array.init n Fun.id in
      let t = X.create a in
      for j = 1 to n - 1 do
        assert (X.rmq t ~lo:0 ~hi:j = (0, 0));
        assert (X.rmq t ~lo:j ~hi:n = (j, j))
      done
    done

end

module M0 = RMQ.Make0(Int)
module T0 = Test(M0)
