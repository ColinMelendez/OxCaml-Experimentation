open Core
open Hardcaml
open Hardcaml_test_harness

module I = struct
  type 'a t =
    { a : 'a [@bits 8]
    ; b : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { sum : 'a [@bits 8] } [@@deriving hardcaml]
end

let create _scope { I.a; b } = { O.sum = Signal.( +: ) a b }

module Bench = Cyclesim_harness.Make (I) (O)

let run ?(run_interactive = false) () =
  Bench.run ~run_interactive ~create (fun ~inputs ~outputs sim ->
    for i = 0 to 9 do
      inputs.a := Bits.of_int_trunc ~width:8 i;
      inputs.b := Bits.of_int_trunc ~width:8 i;
      Cyclesim.cycle sim;
      print_s
        [%message
          ""
            ~a:(!(inputs.a) : Bits.t)
            ~b:(!(inputs.b) : Bits.t)
            ~sum:(!(outputs.sum) : Bits.t)]
    done)
;;

let%expect_test "adder simulation" =
  run ();
  [%expect
    {|
    ((a 00000000) (b 00000000) (sum 00000000))
    ((a 00000001) (b 00000001) (sum 00000010))
    ((a 00000010) (b 00000010) (sum 00000100))
    ((a 00000011) (b 00000011) (sum 00000110))
    ((a 00000100) (b 00000100) (sum 00001000))
    ((a 00000101) (b 00000101) (sum 00001010))
    ((a 00000110) (b 00000110) (sum 00001100))
    ((a 00000111) (b 00000111) (sum 00001110))
    ((a 00001000) (b 00001000) (sum 00010000))
    ((a 00001001) (b 00001001) (sum 00010010))
    |}]
;;
