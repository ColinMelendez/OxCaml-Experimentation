open! Base
open! Import

module type Empty = sig end

module type Scheduler = sig
  val parallel : (Parallel.t @ local -> 'a) @ once shareable -> 'a
end

module Sequential : Scheduler
module Parallel : Scheduler
module With_parallel : Scheduler
module Test_schedulers (Test_scheduler : functor (_ : Scheduler) -> Empty) : Empty
