open! Core
open! Import

include
  (val String_id.make
         ~caller_identity:String_id.legacy_identity
         ~module_name:__MODULE__
         ~include_default_validation:true
         ())
