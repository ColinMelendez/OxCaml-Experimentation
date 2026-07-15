open! Core
open! Import
module Log_global = Global

module T = struct
  type time = Time_float.t

  module Instance = struct
    type t = Log.t
    type return_type = unit

    let would_log = Log.would_log
    let message = Log.structured_message
    let default = ()
  end
end

include T

module Global = struct
  type return_type = unit

  let default = ()
  let would_log = Log_global.would_log
  let message = Log_global.structured_message
end

module Portable = struct
  type time = Time_float.t

  module Instance = struct
    type t = Log.t
    type return_type = unit

    let would_log = Log.would_log
    let message = Log.enqueue_structured_message
    let default = ()
  end

  module Global = struct
    type return_type = unit

    let default = ()

    let would_log level =
      match Log_global.Private.current_published_log () with
      | None ->
        (* The unforced global log is always at the default [Info] level because:
           [Global.Make] creates it that way, and the only way to change the level is via
           [Log.Global.set_level] which forces and publishes the global log. *)
        Level.as_or_more_verbose_than ~log_level:`Info ~msg_level:level
      | Some log -> Log.would_log log level
    ;;

    let message = Log_global.enqueue_structured_message
  end
end

module No_global = struct
  module Ppx_log_syntax = struct
    include T

    module Global = struct
      type return_type = [ `Do_not_use_because_it_will_not_log ]

      let default = `Do_not_use_because_it_will_not_log
      let would_log _ = false
      let message ?level:_ ?time:_ ?tags:_ _ _ = `Do_not_use_because_it_will_not_log
    end

    module Portable = struct
      type time = Time_float.t

      module Instance = Portable.Instance

      module Global = struct
        type return_type = [ `Do_not_use_because_it_will_not_log ]

        let default = `Do_not_use_because_it_will_not_log
        let would_log _ = false
        let message ?level:_ ?time:_ ?tags:_ _ _ = `Do_not_use_because_it_will_not_log
      end
    end
  end
end
