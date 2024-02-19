time_stamp <- function(time = Sys.time()) {
  format(time, time_stamp_format, tz = "UTC")
}

posixct_time <- function(time_stamp) {
  as.POSIXct(x = time_stamp, format = time_stamp_format, tz = "UTC")
}

time_stamp_short <- function(time = Sys.time()) {
  format(time, "%H:%M %OS2")
}

time_seconds <- function() {
  if_any(
    tar_runtime$fun %in% c("tar_make_future"),
    as.numeric(proc.time()["elapsed"]),
    time_seconds_local()
  )
}

time_seconds_local <- function() {
  as.numeric(proc.time()["elapsed"])
}

time_stamp_format <- "%Y-%m-%d %H:%M:%S.%OS"
