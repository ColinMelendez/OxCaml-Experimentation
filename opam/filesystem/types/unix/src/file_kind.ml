open! Core
include Filesystem_types.File_kind
include File_kind_intf

let to_unix_file_kind : t -> Core_unix.file_kind = function
  | Block_device -> S_BLK
  | Character_device -> S_CHR
  | Directory -> S_DIR
  | Fifo -> S_FIFO
  | Regular -> S_REG
  | Socket -> S_SOCK
  | Symlink -> S_LNK
;;

let of_unix_file_kind : Core_unix.file_kind -> t = function
  | S_BLK -> Block_device
  | S_CHR -> Character_device
  | S_DIR -> Directory
  | S_FIFO -> Fifo
  | S_REG -> Regular
  | S_SOCK -> Socket
  | S_LNK -> Symlink
;;
