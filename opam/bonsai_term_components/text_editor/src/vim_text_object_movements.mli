open! Core
open Zed

(** This [Vim_text_object_movements] module contains helpers for walking / constructing
    "text objects". *)

val is_word_char : Zed_char.t -> bool
val is_whitespace : Zed_char.t -> bool
val is_inline_whitespace : Zed_char.t -> bool
val is_newline : Zed_char.t -> bool
val skip_forward_while : rope:Zed_rope.t -> position:int -> f:(Zed_char.t -> bool) -> int
val skip_backward_while : rope:Zed_rope.t -> position:int -> f:(Zed_char.t -> bool) -> int

val group_start_backward
  :  rope:Zed_rope.t
  -> position:int
  -> in_group:(Zed_char.t -> bool)
  -> int

val group_end_forward_inclusive
  :  rope:Zed_rope.t
  -> position:int
  -> in_group:(Zed_char.t -> bool)
  -> int

(** [text_object_of_vim_command] solely gives us the start / stop location for the
    "textobject" that the vim text object command is targetting. Other functions are
    responsible for the actual implementation of the action (only [d]elete, [c]hange),
    though in the future we can support other kinds of actions. *)
val text_object_of_vim_command
  :  zed_context:'a Zed_edit.context
  -> Vim_text_object_command.t
  -> Text_object.t option
