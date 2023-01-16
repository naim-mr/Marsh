open Byoppl.Eval
open Byoppl.Distribution
open Byoppl.Lang

let () = print_endline "Hello, World!"

let funny_bernnoulli =
  model "funny_bernoulli"
    (letin "a"
       (sample (d_bernoulli (const 0.5)))
       (letin "b"
          (sample (d_bernoulli (const 0.5)))
          (letin "c"
             (sample (d_bernoulli (const 0.5)))
             (letin "()"
                (assume (and_c (eq "b" 1.) (eq "a" 1.)))
                (add (var "a") (add (var "c") (var "b")))))))

let uni =
  model "uniform_test"
    (letin "a"
       (sample (d_uniformd (const 0.) (const 2.)))
       (add (var "a") (var "a")))

let iftest =
  model "iftest"
    (letin "a"
       (sample (d_uniformd (const 0.) (const 2.)))
       (letin "b"
          (sample (d_bernoulli (const 0.4)))
          (ifthenelse (var "b") (add (var "a") (var "a")) (var "a"))))

let _ =
  Format.printf "@.-- Exact Funny bernoulli Sampling --@.";
  let dist = infer funny_bernnoulli in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%f %f@." x probs.(i)) values

let _ =
  Format.printf "@.-- Exact Uni Sampling --@.";
  let dist = infer uni in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%f %f@." x probs.(i)) values

let _ =
  Format.printf "@.-- Exact iftest Sampling --@.";
  let dist = infer iftest in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%f %f@." x probs.(i)) values
