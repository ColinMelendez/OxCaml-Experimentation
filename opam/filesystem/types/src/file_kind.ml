open! Core
include File_kind_intf

type t =
  | Block_device
  | Character_device
  | Directory
  | Fifo
  | Regular
  | Socket
  | Symlink
[@@deriving
  compare ~portable ~localize
  , enumerate
  , equal ~portable ~localize
  , hash ~portable
  , quickcheck ~portable
  , sexp_of ~portable]

let to_async_file_kind = function
  | Block_device -> `Block
  | Character_device -> `Char
  | Directory -> `Directory
  | Fifo -> `Fifo
  | Regular -> `File
  | Symlink -> `Link
  | Socket -> `Socket
;;

let of_async_file_kind = function
  | `Block -> Block_device
  | `Char -> Character_device
  | `Directory -> Directory
  | `Fifo -> Fifo
  | `File -> Regular
  | `Link -> Symlink
  | `Socket -> Socket
;;
