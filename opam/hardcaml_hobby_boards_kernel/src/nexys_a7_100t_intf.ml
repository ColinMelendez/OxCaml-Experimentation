open! Base
open Hardcaml
open Board

module type Rgb_led = sig
  module O : sig
    type 'a t =
      { red : 'a
      ; green : 'a
      ; blue : 'a
      }
    [@@deriving hardcaml]
  end

  include M_O(O).S
end

module type Nexys_a7_100t = sig
  module Clock_and_reset : Clock_and_reset.S with type 'a I.t = 'a Clock_and_reset.I.t

  module Buttons : sig
    module I : sig
      type 'a t =
        { up : 'a
        ; left : 'a
        ; right : 'a
        ; down : 'a
        ; center : 'a
        }
      [@@deriving hardcaml]
    end

    include M_I(I).S with type board := Board.t
  end

  module Switches : sig
    module I : Interface.S with type 'a t = 'a
    include M_I(I).S with type board := Board.t
  end

  module Leds : sig
    module O : Interface.S with type 'a t = 'a
    include M_O(O).S with type board := Board.t
  end

  module Rgb_led0 : Rgb_led with type board := Board.t
  module Rgb_led1 : Rgb_led with type board := Board.t

  module Seven_segment_display : sig
    module O : sig
      type 'a t =
        { set_n : 'a
        ; select_n : 'a
        }
      [@@deriving hardcaml]
    end

    include M_O(O).S with type board := Board.t
  end

  module Vga : sig
    module O : sig
      type 'a t =
        { red : 'a
        ; green : 'a
        ; blue : 'a
        ; horizontal_sync : 'a
        ; vertical_sync : 'a
        }
      [@@deriving hardcaml]
    end

    include M_O(O).S with type board := Board.t
  end

  module Uart : sig
    module I : sig
      type 'a t =
        { rxd : 'a
        ; cts : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { txd : 'a
        ; rts : 'a
        }
      [@@deriving hardcaml]
    end

    include M_IO(I)(O).S with type board := Board.t
  end

  module Microphone : sig
    module I : sig
      type 'a t = { data : 'a } [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { clock : 'a
        ; edge_select : 'a
        }
      [@@deriving hardcaml]
    end

    include M_IO(I)(O).S with type board := Board.t
  end

  module Audio_amplifier : sig
    module T : sig
      type 'a t = { pwm : 'a } [@@deriving hardcaml]
    end

    module O : sig
      type 'a t = { sd : 'a } [@@deriving hardcaml]
    end

    include M_OT(O)(T).S with type board := Board.t
  end

  module Ps2 : sig
    module T : sig
      type 'a t =
        { clock : 'a
        ; data : 'a
        }
      [@@deriving hardcaml]
    end

    include M_T(T).S with type board := Board.t
  end

  module Temperature_sensor : sig
    module T : sig
      type 'a t =
        { scl : 'a
        ; sda : 'a
        }
      [@@deriving hardcaml]
    end

    include M_T(T).S with type board := Board.t
  end

  module Accelerometer : sig
    module I : sig
      type 'a t =
        { miso : 'a
        ; int_ : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { sclk : 'a
        ; mosi : 'a
        ; cs_n : 'a
        }
      [@@deriving hardcaml]
    end

    include M_IO(I)(O).S with type board := Board.t
  end

  module Ethernet : sig
    module I : sig
      type 'a t =
        { crsdv : 'a
        ; rxerr : 'a
        ; rxd : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { txen : 'a
        ; txd : 'a
        ; rstn : 'a
        ; refclk : 'a
        }
      [@@deriving hardcaml]
    end

    include M_IO(I)(O).S with type board := Board.t
  end

  module Pmod_ja : sig
    module T : sig
      type 'a t =
        { pin1 : 'a
        ; pin2 : 'a
        ; pin3 : 'a
        ; pin4 : 'a
        ; pin7 : 'a
        ; pin8 : 'a
        ; pin9 : 'a
        ; pin10 : 'a
        }
      [@@deriving hardcaml]
    end

    include M_T(T).S with type board := Board.t
  end

  module Pmod_jb : sig
    module T : sig
      type 'a t =
        { pin1 : 'a
        ; pin2 : 'a
        ; pin3 : 'a
        ; pin4 : 'a
        ; pin7 : 'a
        ; pin8 : 'a
        ; pin9 : 'a
        ; pin10 : 'a
        }
      [@@deriving hardcaml]
    end

    include M_T(T).S with type board := Board.t
  end

  module Pmod_jc : sig
    module T : sig
      type 'a t =
        { pin1 : 'a
        ; pin2 : 'a
        ; pin3 : 'a
        ; pin4 : 'a
        ; pin7 : 'a
        ; pin8 : 'a
        ; pin9 : 'a
        ; pin10 : 'a
        }
      [@@deriving hardcaml]
    end

    include M_T(T).S with type board := Board.t
  end

  module Pmod_jd : sig
    module T : sig
      type 'a t =
        { pin1 : 'a
        ; pin2 : 'a
        ; pin3 : 'a
        ; pin4 : 'a
        ; pin7 : 'a
        ; pin8 : 'a
        ; pin9 : 'a
        ; pin10 : 'a
        }
      [@@deriving hardcaml]
    end

    include M_T(T).S with type board := Board.t
  end

  val generate_top : ?dir:string -> Board.t -> unit
end
