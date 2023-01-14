type 'a support = { values : 'a array; probs : float array }

type 'a t = {
  sample : unit -> 'a;
  mean : (unit -> float) option;
  var : (unit -> float) option;
  support : 'a support option;
}

let draw dist = dist.sample ()

let get_support ?(shrink = false) dist =
  match shrink with
  | false -> Option.get dist.support
  | true ->
      let { values; probs } = Option.get dist.support in
      let values, probs = Utils.shrink ~values ~probs in
      { values; probs }

let make ~sample ?mean ?var ?support () = { sample; mean; var; support }

let support_p ~values ~probs =
  assert (Array.length values = Array.length probs);
  let support = { values; probs } in
  let sample () =
    let i = Owl_stats.categorical_rvs probs in
    values.(i)
  in
  make ~sample ~support ()

let bernoulli ~p =
  assert (0. <= p && p <= 1.);
  let sample () = float_of_int (Owl_stats.binomial_rvs ~p ~n:1) in
  let mean () = p in
  let var () = p *. (1. -. p) in
  let support = { values = [| 0.; 1. |]; probs = [| 1. -. p; p |] } in
  make ~sample ~support ~mean ~var ()

let uniform_discrete ~a ~b =
  let sample () = Owl_stats.uniform_rvs ~a ~b in
  let mean () = (a +. b) /. 2. in
  let n = b -. a +. 1. in
  let var () = ((n ** 2.) -. 1.) /. 12. in
  let support =
    {
      values = Array.init (int_of_float n) (fun x -> a +. float_of_int x);
      probs = Array.init (int_of_float n) (fun _ -> 1. /. n);
    }
  in
  make ~sample ~support ~mean ~var ()
