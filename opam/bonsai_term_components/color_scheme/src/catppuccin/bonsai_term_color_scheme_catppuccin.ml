open! Core
open! Bonsai_term

module T = struct
  type t =
    | Rosewater
    | Flamingo
    | Pink
    | Mauve
    | Red
    | Maroon
    | Peach
    | Yellow
    | Green
    | Teal
    | Sky
    | Sapphire
    | Blue
    | Lavender
    | Text
    | Subtext1
    | Subtext0
    | Overlay2
    | Overlay1
    | Overlay0
    | Surface2
    | Surface1
    | Surface0
    | Base
    | Mantle
    | Crust
  [@@deriving sexp, equal, hash, compare, enumerate, string]
end

include T
include Hashable.Make_plain (T)
include Comparable.Make_plain (T)

let memo f = Core.Memo.general ~hashable f

module Rgb = struct
  type t =
    { r : int
    ; b : int
    ; g : int
    }
  [@@deriving globalize, sexp ~portable, equal ~portable, hash, compare]

  let to_color { r; g; b } = Bonsai_term.Attr.Color.rgb ~r ~g ~b
end

module Ansi_16 = struct
  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Light_black
    | Light_red
    | Light_green
    | Light_yellow
    | Light_blue
    | Light_magenta
    | Light_cyan
    | Light_white
    | Default
  [@@deriving sexp ~portable, equal ~portable, compare ~portable, hash]

  let to_attr_color = function
    | Black -> Bonsai_term.Attr.Color.Expert.black
    | Red -> Bonsai_term.Attr.Color.Expert.red
    | Green -> Bonsai_term.Attr.Color.Expert.green
    | Yellow -> Bonsai_term.Attr.Color.Expert.yellow
    | Blue -> Bonsai_term.Attr.Color.Expert.blue
    | Magenta -> Bonsai_term.Attr.Color.Expert.magenta
    | Cyan -> Bonsai_term.Attr.Color.Expert.cyan
    | White -> Bonsai_term.Attr.Color.Expert.white
    | Light_black -> Bonsai_term.Attr.Color.Expert.lightblack
    | Light_red -> Bonsai_term.Attr.Color.Expert.lightred
    | Light_green -> Bonsai_term.Attr.Color.Expert.lightgreen
    | Light_yellow -> Bonsai_term.Attr.Color.Expert.lightyellow
    | Light_blue -> Bonsai_term.Attr.Color.Expert.lightblue
    | Light_magenta -> Bonsai_term.Attr.Color.Expert.lightmagenta
    | Light_cyan -> Bonsai_term.Attr.Color.Expert.lightcyan
    | Light_white -> Bonsai_term.Attr.Color.Expert.lightwhite
    | Default -> Bonsai_term.Attr.Color.Expert.default
  ;;

  (* Approximate RGB values based on standard xterm defaults, for use in color-distance
     calculations and contexts that require RGB. *)
  let approximate_rgb = function
    | Black -> { Rgb.r = 0; g = 0; b = 0 }
    | Red -> { Rgb.r = 205; g = 0; b = 0 }
    | Green -> { Rgb.r = 0; g = 205; b = 0 }
    | Yellow -> { Rgb.r = 205; g = 205; b = 0 }
    | Blue -> { Rgb.r = 0; g = 0; b = 238 }
    | Magenta -> { Rgb.r = 205; g = 0; b = 205 }
    | Cyan -> { Rgb.r = 0; g = 205; b = 205 }
    | White -> { Rgb.r = 229; g = 229; b = 229 }
    | Light_black -> { Rgb.r = 127; g = 127; b = 127 }
    | Light_red -> { Rgb.r = 255; g = 0; b = 0 }
    | Light_green -> { Rgb.r = 0; g = 255; b = 0 }
    | Light_yellow -> { Rgb.r = 255; g = 255; b = 0 }
    | Light_blue -> { Rgb.r = 92; g = 92; b = 255 }
    | Light_magenta -> { Rgb.r = 255; g = 0; b = 255 }
    | Light_cyan -> { Rgb.r = 0; g = 255; b = 255 }
    | Light_white -> { Rgb.r = 255; g = 255; b = 255 }
    | Default -> { Rgb.r = 229; g = 229; b = 229 }
  ;;
end

module Color_value = struct
  type t =
    | Rgb of Rgb.t
    | Ansi_16 of Ansi_16.t
  [@@deriving sexp ~portable, equal ~portable, compare ~portable, hash]

  let to_attr_color = function
    | Rgb rgb -> Rgb.to_color rgb
    | Ansi_16 ansi -> Ansi_16.to_attr_color ansi
  ;;

  let to_approximate_rgb = function
    | Rgb rgb -> rgb
    | Ansi_16 ansi -> Ansi_16.approximate_rgb ansi
  ;;
end

let rgb (r, g, b) = Color_value.Rgb { Rgb.r; g; b }

module Flavor = struct
  type t =
    { rosewater : Color_value.t
    ; flamingo : Color_value.t
    ; pink : Color_value.t
    ; mauve : Color_value.t
    ; red : Color_value.t
    ; maroon : Color_value.t
    ; peach : Color_value.t
    ; yellow : Color_value.t
    ; green : Color_value.t
    ; teal : Color_value.t
    ; sky : Color_value.t
    ; sapphire : Color_value.t
    ; blue : Color_value.t
    ; lavender : Color_value.t
    ; text : Color_value.t
    ; subtext1 : Color_value.t
    ; subtext0 : Color_value.t
    ; overlay2 : Color_value.t
    ; overlay1 : Color_value.t
    ; overlay0 : Color_value.t
    ; surface2 : Color_value.t
    ; surface1 : Color_value.t
    ; surface0 : Color_value.t
    ; base : Color_value.t
    ; mantle : Color_value.t
    ; crust : Color_value.t
    ; is_dark : bool
    }
  [@@deriving sexp, equal, hash, compare]

  let color_value ~flavor = function
    | Rosewater -> flavor.rosewater
    | Flamingo -> flavor.flamingo
    | Pink -> flavor.pink
    | Mauve -> flavor.mauve
    | Red -> flavor.red
    | Maroon -> flavor.maroon
    | Peach -> flavor.peach
    | Yellow -> flavor.yellow
    | Green -> flavor.green
    | Teal -> flavor.teal
    | Sky -> flavor.sky
    | Sapphire -> flavor.sapphire
    | Blue -> flavor.blue
    | Lavender -> flavor.lavender
    | Text -> flavor.text
    | Subtext1 -> flavor.subtext1
    | Subtext0 -> flavor.subtext0
    | Overlay2 -> flavor.overlay2
    | Overlay1 -> flavor.overlay1
    | Overlay0 -> flavor.overlay0
    | Surface2 -> flavor.surface2
    | Surface1 -> flavor.surface1
    | Surface0 -> flavor.surface0
    | Base -> flavor.base
    | Mantle -> flavor.mantle
    | Crust -> flavor.crust
  ;;

  let color ~flavor c = Color_value.to_attr_color (color_value ~flavor c)
  let is_dark t = t.is_dark

  let map_color_values t ~f =
    { rosewater = f t.rosewater
    ; flamingo = f t.flamingo
    ; pink = f t.pink
    ; mauve = f t.mauve
    ; red = f t.red
    ; maroon = f t.maroon
    ; peach = f t.peach
    ; yellow = f t.yellow
    ; green = f t.green
    ; teal = f t.teal
    ; sky = f t.sky
    ; sapphire = f t.sapphire
    ; blue = f t.blue
    ; lavender = f t.lavender
    ; text = f t.text
    ; subtext1 = f t.subtext1
    ; subtext0 = f t.subtext0
    ; overlay2 = f t.overlay2
    ; overlay1 = f t.overlay1
    ; overlay0 = f t.overlay0
    ; surface2 = f t.surface2
    ; surface1 = f t.surface1
    ; surface0 = f t.surface0
    ; base = f t.base
    ; mantle = f t.mantle
    ; crust = f t.crust
    ; is_dark = t.is_dark
    }
  ;;
end

module Latte = struct
  let flavor : Flavor.t =
    { rosewater = rgb (220, 138, 120)
    ; flamingo = rgb (221, 120, 120)
    ; pink = rgb (234, 118, 203)
    ; mauve = rgb (136, 57, 239)
    ; red = rgb (210, 15, 57)
    ; maroon = rgb (230, 69, 83)
    ; peach = rgb (254, 100, 11)
    ; yellow = rgb (223, 142, 29)
    ; green = rgb (64, 160, 43)
    ; teal = rgb (23, 146, 153)
    ; sky = rgb (4, 165, 229)
    ; sapphire = rgb (32, 159, 181)
    ; blue = rgb (30, 102, 245)
    ; lavender = rgb (114, 135, 253)
    ; text = rgb (76, 79, 105)
    ; subtext1 = rgb (92, 95, 119)
    ; subtext0 = rgb (108, 111, 133)
    ; overlay2 = rgb (124, 127, 147)
    ; overlay1 = rgb (140, 143, 161)
    ; overlay0 = rgb (156, 160, 176)
    ; surface2 = rgb (172, 176, 190)
    ; surface1 = rgb (188, 192, 204)
    ; surface0 = rgb (204, 208, 218)
    ; base = rgb (239, 241, 245)
    ; mantle = rgb (230, 233, 239)
    ; crust = rgb (220, 224, 232)
    ; is_dark = false
    }
  ;;
end

module Frappe = struct
  let flavor : Flavor.t =
    { rosewater = rgb (242, 213, 207)
    ; flamingo = rgb (238, 190, 190)
    ; pink = rgb (244, 184, 228)
    ; mauve = rgb (202, 158, 230)
    ; red = rgb (231, 130, 132)
    ; maroon = rgb (234, 153, 156)
    ; peach = rgb (239, 159, 118)
    ; yellow = rgb (229, 200, 144)
    ; green = rgb (166, 209, 137)
    ; teal = rgb (129, 200, 190)
    ; sky = rgb (153, 209, 219)
    ; sapphire = rgb (133, 193, 220)
    ; blue = rgb (140, 170, 238)
    ; lavender = rgb (186, 187, 241)
    ; text = rgb (198, 208, 245)
    ; subtext1 = rgb (181, 191, 226)
    ; subtext0 = rgb (165, 173, 206)
    ; overlay2 = rgb (148, 156, 187)
    ; overlay1 = rgb (131, 139, 167)
    ; overlay0 = rgb (115, 121, 148)
    ; surface2 = rgb (98, 104, 128)
    ; surface1 = rgb (81, 87, 109)
    ; surface0 = rgb (65, 69, 89)
    ; base = rgb (48, 52, 70)
    ; mantle = rgb (41, 44, 60)
    ; crust = rgb (35, 38, 52)
    ; is_dark = true
    }
  ;;
end

module Macchiato = struct
  let flavor : Flavor.t =
    { rosewater = rgb (244, 219, 214)
    ; flamingo = rgb (240, 198, 198)
    ; pink = rgb (245, 189, 230)
    ; mauve = rgb (198, 160, 246)
    ; red = rgb (237, 135, 150)
    ; maroon = rgb (238, 153, 160)
    ; peach = rgb (245, 169, 127)
    ; yellow = rgb (238, 212, 159)
    ; green = rgb (166, 218, 149)
    ; teal = rgb (139, 213, 202)
    ; sky = rgb (145, 215, 227)
    ; sapphire = rgb (125, 196, 228)
    ; blue = rgb (138, 173, 244)
    ; lavender = rgb (183, 189, 248)
    ; text = rgb (202, 211, 245)
    ; subtext1 = rgb (184, 192, 224)
    ; subtext0 = rgb (165, 173, 203)
    ; overlay2 = rgb (147, 154, 183)
    ; overlay1 = rgb (128, 135, 162)
    ; overlay0 = rgb (110, 115, 141)
    ; surface2 = rgb (91, 96, 120)
    ; surface1 = rgb (73, 77, 100)
    ; surface0 = rgb (54, 58, 79)
    ; base = rgb (36, 39, 58)
    ; mantle = rgb (30, 32, 48)
    ; crust = rgb (24, 25, 38)
    ; is_dark = true
    }
  ;;
end

module Mocha = struct
  let flavor : Flavor.t =
    { rosewater = rgb (245, 224, 220)
    ; flamingo = rgb (242, 205, 205)
    ; pink = rgb (245, 194, 231)
    ; mauve = rgb (203, 166, 247)
    ; red = rgb (243, 139, 168)
    ; maroon = rgb (235, 160, 172)
    ; peach = rgb (250, 179, 135)
    ; yellow = rgb (249, 226, 175)
    ; green = rgb (166, 227, 161)
    ; teal = rgb (148, 226, 213)
    ; sky = rgb (137, 220, 235)
    ; sapphire = rgb (116, 199, 236)
    ; blue = rgb (137, 180, 250)
    ; lavender = rgb (180, 190, 254)
    ; text = rgb (205, 214, 244)
    ; subtext1 = rgb (186, 194, 222)
    ; subtext0 = rgb (166, 173, 200)
    ; overlay2 = rgb (147, 153, 178)
    ; overlay1 = rgb (127, 132, 156)
    ; overlay0 = rgb (108, 112, 134)
    ; surface2 = rgb (88, 91, 112)
    ; surface1 = rgb (69, 71, 90)
    ; surface0 = rgb (49, 50, 68)
    ; base = rgb (30, 30, 46)
    ; mantle = rgb (24, 24, 37)
    ; crust = rgb (17, 17, 27)
    ; is_dark = true
    }
  ;;
end

module Flavor_name = struct
  type t =
    | Mocha
    | Macchiato
    | Frappe
    | Latte
  [@@deriving sexp ~portable, equal ~portable, compare, enumerate, string]

  let to_flavor = function
    | Mocha -> Mocha.flavor
    | Macchiato -> Macchiato.flavor
    | Frappe -> Frappe.flavor
    | Latte -> Latte.flavor
  ;;
end

let color ~flavor c = Flavor.color ~flavor c
let to_color_value flavor = memo (Flavor.color_value ~flavor)

let to_rgb flavor =
  let f = to_color_value flavor in
  fun c -> Color_value.to_approximate_rgb (f c)
;;
