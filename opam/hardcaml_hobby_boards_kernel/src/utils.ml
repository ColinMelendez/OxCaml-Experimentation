open! Base
open Hardcaml
open Signal

let generate_clear { Clock_and_reset.I.clock_100; reset_n } =
  let reset_chain =
    reg_fb
      (Reg_spec.create ~clock:clock_100 ~reset:reset_n ~reset_level:Low ())
      ~reset_to:(Bits.ones 16)
      ~width:16
      ~f:(fun d -> sll d ~by:1)
  in
  reg (Reg_spec.create ~clock:clock_100 ()) (msb reset_chain)
;;

let sync_reg_spec (clocking : _ Clock_and_reset.I.t) =
  Reg_spec.create ~clock:clocking.clock_100 ~clear:(generate_clear clocking) ()
;;
