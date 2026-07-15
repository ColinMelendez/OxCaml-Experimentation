open! Core
include Filesystem_types.File_stats
include File_stats_intf
module File_permissions = Filesystem_types.File_permissions

module Int64_u = struct
  external of_int64
    :  (int64[@local_opt])
    -> (int64#[@unboxed])
    @@ portable
    = "%unbox_int64"

  external to_int64 : (int64#[@unboxed]) -> (int64[@local_opt]) @@ portable = "%box_int64"
end

let to_unix_stats
  { host_device
  ; inode
  ; kind
  ; permissions
  ; hard_links
  ; user_id
  ; group_id
  ; file_device
  ; size
  ; access_time
  ; modify_time
  ; status_time
  }
  : Core_unix.stats
  =
  { st_dev = host_device
  ; st_ino = inode
  ; st_kind = kind |> File_kind.to_unix_file_kind
  ; st_perm = permissions |> File_permissions.to_int
  ; st_nlink = hard_links
  ; st_uid = user_id
  ; st_gid = group_id
  ; st_rdev = file_device
  ; st_size = size |> Int64_u.to_int64
  ; st_atime = access_time |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_sec
  ; st_mtime = modify_time |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_sec
  ; st_ctime = status_time |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_sec
  }
;;

let of_unix_stats
  ({ st_dev
   ; st_ino
   ; st_kind
   ; st_perm
   ; st_nlink
   ; st_uid
   ; st_gid
   ; st_rdev
   ; st_size
   ; st_atime
   ; st_mtime
   ; st_ctime
   } :
    Core_unix.stats)
  =
  { host_device = st_dev
  ; inode = st_ino
  ; kind = st_kind |> File_kind.of_unix_file_kind
  ; permissions = st_perm |> File_permissions.of_int_exn
  ; hard_links = st_nlink
  ; user_id = st_uid
  ; group_id = st_gid
  ; file_device = st_rdev
  ; size = st_size |> Int64_u.of_int64
  ; access_time = st_atime |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch
  ; modify_time = st_mtime |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch
  ; status_time = st_ctime |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch
  }
;;
