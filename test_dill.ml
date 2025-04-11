
open Dill

let s = repeat 42
let () = assert (nth 0 s = 42)

let s = cons 89 s
let () = assert (nth (-1) s = 42)
let () = assert (nth 0    s = 89)
let () = assert (nth 1    s = 42)

let s = snoc 55 s
let () = assert (nth (-2) s = 42)
let () = assert (nth (-1) s = 55)
let () = assert (nth 0    s = 89)
let () = assert (nth 1    s = 42)

let ints = init (fun i -> i)
let () = for i = -5 to 5 do assert (nth i ints = i) done

let s = zip (+) ints (rsh ints)
let () = assert (nth 10 s = 19)

let fib = fix (fun fib ->
  cons 0 (cons 1 (weld (zip (-) (lsh (lsh fib)) (lsh fib))
                       (zip (+) (lsh fib)       fib     ))))

let () = assert (nth (-10) fib = -55)
let () = assert (nth ( -3) fib = 2)
let () = assert (nth ( -2) fib = -1)
let () = assert (nth ( -1) fib = 1)
let () = assert (nth    0  fib = 0)
let () = assert (nth    1  fib = 1)
let () = assert (nth    2  fib = 1)
let () = assert (nth    3  fib = 2)
let () = assert (nth   10  fib = 55)

let fib = rsh (fix (fun fib ->
  snoc 0 (cons 1 (weld (zip (-) (lsh fib) fib)
                       (zip (+) (rsh fib) fib)))))

let () = assert (nth (-10) fib = -55)
let () = assert (nth ( -3) fib = 2)
let () = assert (nth ( -2) fib = -1)
let () = assert (nth ( -1) fib = 1)
let () = assert (nth    0  fib = 0)
let () = assert (nth    1  fib = 1)
let () = assert (nth    2  fib = 1)
let () = assert (nth    3  fib = 2)
let () = assert (nth   10  fib = 55)
