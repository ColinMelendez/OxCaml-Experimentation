open! Base
open! Hardcaml
open Board
module Clock_and_reset = Clock_and_reset

module Buttons = struct
  module I = struct
    type 'a t =
      { up : 'a [@rtlname "u"]
      ; left : 'a [@rtlname "l"]
      ; right : 'a [@rtlname "r"]
      ; down : 'a [@rtlname "d"]
      ; center : 'a [@rtlname "c"]
      }
    [@@deriving hardcaml ~rtlprefix:"button_"]
  end

  include
    Make_I
      (struct
        let core = "button"
      end)
      (I)
end

module Switches = struct
  module I = Types.Value (struct
      let port_name = "switches"
      let port_width = 16
    end)

  include
    Make_I
      (struct
        let core = "switches"
      end)
      (I)
end

module Leds = struct
  module O = Types.Value (struct
      let port_name = "leds"
      let port_width = 16
    end)

  include
    Make_O
      (struct
        let core = "leds"
      end)
      (O)
end

module Rgb_led (C : sig
    val index : int
  end) =
struct
  module O = struct
    type 'a t =
      { red : 'a [@rtlname "r"]
      ; green : 'a [@rtlname "g"]
      ; blue : 'a [@rtlname "b"]
      }
    [@@deriving hardcaml ~rtlprefix:[%string "rgb_led_%{C.index#Int}_"]]
  end

  include
    Make_O
      (struct
        let core = [%string "rgb_led_%{C.index#Int}"]
      end)
      (O)
end

module Rgb_led0 = Rgb_led (struct
    let index = 0
  end)

module Rgb_led1 = Rgb_led (struct
    let index = 1
  end)

module Seven_segment_display = struct
  module O = struct
    type 'a t =
      { set_n : 'a [@bits 8] [@rtlname "seven_seg_set"]
      ; select_n : 'a [@bits 8] [@rtlname "seven_seg_sel_n"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_O
      (struct
        let core = "seven_segment_display"
      end)
      (O)
end

module Vga = struct
  module O = struct
    type 'a t =
      { red : 'a [@bits 4] [@rtlname "vga_r"]
      ; green : 'a [@bits 4] [@rtlname "vga_g"]
      ; blue : 'a [@bits 4] [@rtlname "vga_b"]
      ; horizontal_sync : 'a [@rtlname "vga_hs"]
      ; vertical_sync : 'a [@rtlname "vga_vs"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_O
      (struct
        let core = "vga"
      end)
      (O)
end

module Uart = struct
  module I = struct
    type 'a t =
      { rxd : 'a [@rtlname "usb_uart_rxd"]
      ; cts : 'a [@rtlname "usb_uart_rts"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { txd : 'a [@rtlname "usb_uart_txd"]
      ; rts : 'a [@rtlname "usb_uart_cts"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_IO
      (struct
        let core = "uart"
      end)
      (I)
      (O)
end

module Microphone = struct
  module I = struct
    type 'a t = { data : 'a [@rtlname "mic_data"] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { clock : 'a [@rtlname "mic_clk"]
      ; edge_select : 'a [@rtlname "mic_lrsel"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_IO
      (struct
        let core = "microphone"
      end)
      (I)
      (O)
end

module Audio_amplifier = struct
  module T = struct
    type 'a t = { pwm : 'a [@rtlname "aud_pwm"] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { sd : 'a [@rtlname "aud_sd"] } [@@deriving hardcaml]
  end

  include
    Make_OT
      (struct
        let core = "audio_amplifier"
      end)
      (O)
      (T)
end

module Ps2 = struct
  module T = struct
    type 'a t =
      { clock : 'a [@rtlname "ps2_clk"]
      ; data : 'a [@rtlname "ps2_data"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_T
      (struct
        let core = "ps2"
      end)
      (T)
end

module Temperature_sensor = struct
  module T = struct
    type 'a t =
      { scl : 'a [@rtlname "temp_scl"]
      ; sda : 'a [@rtlname "temp_sda"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_T
      (struct
        let core = "temperature_sensor"
      end)
      (T)
end

module Accelerometer = struct
  module I = struct
    type 'a t =
      { miso : 'a [@rtlname "acl_miso"]
      ; int_ : 'a [@bits 2] [@rtlname "acl_int"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { sclk : 'a [@rtlname "acl_sclk"]
      ; mosi : 'a [@rtlname "acl_mosi"]
      ; cs_n : 'a [@rtlname "acl_csn"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_IO
      (struct
        let core = "accelerometer"
      end)
      (I)
      (O)
end

module Ethernet = struct
  module I = struct
    type 'a t =
      { crsdv : 'a [@rtlname "eth_crsdv"]
      ; rxerr : 'a [@rtlname "eth_rxerr"]
      ; rxd : 'a [@bits 2] [@rtlname "eth_rxd"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { txen : 'a [@rtlname "eth_txen"]
      ; txd : 'a [@bits 2] [@rtlname "eth_txd"]
      ; rstn : 'a [@rtlname "eth_rstn"]
      ; refclk : 'a [@rtlname "eth_refclk"]
      }
    [@@deriving hardcaml]
  end

  include
    Make_IO
      (struct
        let core = "ethernet"
      end)
      (I)
      (O)
end

module Pmod_ja = struct
  module T = struct
    type 'a t =
      { pin1 : 'a [@rtlname "1"]
      ; pin2 : 'a [@rtlname "2"]
      ; pin3 : 'a [@rtlname "3"]
      ; pin4 : 'a [@rtlname "4"]
      ; pin7 : 'a [@rtlname "7"]
      ; pin8 : 'a [@rtlname "8"]
      ; pin9 : 'a [@rtlname "9"]
      ; pin10 : 'a [@rtlname "10"]
      }
    [@@deriving hardcaml ~rtlprefix:"JA"]
  end

  include
    Make_T
      (struct
        let core = "pmod_ja"
      end)
      (T)
end

module Pmod_jb = struct
  module T = struct
    type 'a t =
      { pin1 : 'a [@rtlname "1"]
      ; pin2 : 'a [@rtlname "2"]
      ; pin3 : 'a [@rtlname "3"]
      ; pin4 : 'a [@rtlname "4"]
      ; pin7 : 'a [@rtlname "7"]
      ; pin8 : 'a [@rtlname "8"]
      ; pin9 : 'a [@rtlname "9"]
      ; pin10 : 'a [@rtlname "10"]
      }
    [@@deriving hardcaml ~rtlprefix:"JB"]
  end

  include
    Make_T
      (struct
        let core = "pmod_jb"
      end)
      (T)
end

module Pmod_jc = struct
  module T = struct
    type 'a t =
      { pin1 : 'a [@rtlname "1"]
      ; pin2 : 'a [@rtlname "2"]
      ; pin3 : 'a [@rtlname "3"]
      ; pin4 : 'a [@rtlname "4"]
      ; pin7 : 'a [@rtlname "7"]
      ; pin8 : 'a [@rtlname "8"]
      ; pin9 : 'a [@rtlname "9"]
      ; pin10 : 'a [@rtlname "10"]
      }
    [@@deriving hardcaml ~rtlprefix:"JC"]
  end

  include
    Make_T
      (struct
        let core = "pmod_jc"
      end)
      (T)
end

module Pmod_jd = struct
  module T = struct
    type 'a t =
      { pin1 : 'a [@rtlname "1"]
      ; pin2 : 'a [@rtlname "2"]
      ; pin3 : 'a [@rtlname "3"]
      ; pin4 : 'a [@rtlname "4"]
      ; pin7 : 'a [@rtlname "7"]
      ; pin8 : 'a [@rtlname "8"]
      ; pin9 : 'a [@rtlname "9"]
      ; pin10 : 'a [@rtlname "10"]
      }
    [@@deriving hardcaml ~rtlprefix:"JD"]
  end

  include
    Make_T
      (struct
        let core = "pmod_jd"
      end)
      (T)
end

let part_info =
  Xml_pins.Part_and_pins.t_of_sexp
    (Parsexp.Single.parse_string_exn Board_info.nexys_a7_100t_dot_sexp)
;;

let custom_constraints =
  Rope.concat
    ~sep:[%rope "\n"]
    [ [%rope "set_property BITSTREAM.GENERAL.COMPRESS true [current_design];"]
    ; [%rope "set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design];"]
    ; [%rope "set_property CFGBVS VCCO [current_design]"]
    ; [%rope "set_property CONFIG_VOLTAGE 3.3 [current_design]"]
    ; [%rope "create_clock -add -name clock_100 -period 10.00 [get_ports {clock_100}];"]
    ]
;;

let generate_top ?dir board =
  Xilinx_top.generate
    ?dir
    ~name:"nexys_a7_100t"
    ~part:(Xml_pins.Part_and_pins.part part_info)
    ~pins:(Xml_pins.Part_and_pins.pins part_info)
    ~custom_constraints
    board
;;
