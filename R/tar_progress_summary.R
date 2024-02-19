#' @title Summarize target progress.
#' @export
#' @family progress
#' @description Summarize the progress of a run of the pipeline.
#' @return A data frame with one row and the following
#'   optional columns that can be selected with `fields`.
#'   (`time` is omitted by default.)
#'   * `dispatched`: number of targets that were sent off to run and
#'     did not (yet) finish. These targets may not actually be running,
#'     depending on the status and workload of parallel workers.
#'   * `completed`: number of targets that completed without
#'     error or cancellation.
#'   * `errored`: number of targets that threw an error.
#'   * `canceled`: number of canceled targets (see [tar_cancel()]).
#'   * `since`: how long ago progress last changed (`Sys.time() - time`).
#'   * `time`: the time when the progress last changed
#'     (modification timestamp of the `_targets/meta/progress` file).
#' @inheritParams tar_validate
#' @param fields Optional, names of progress data columns to read.
#'   Set to `NULL` to read all fields.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, x, pattern = map(x)),
#'     tar_target(z, stopifnot(y < 1.5), pattern = map(y), error = "continue")
#'   )
#' }, ask = FALSE)
#' try(tar_make())
#' tar_progress_summary()
#' })
#' }
tar_progress_summary <- function(
  fields = c(
    "skipped",
    "dispatched",
    "completed",
    "errored",
    "canceled",
    "since"
  ),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_progress_summary", store)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  time <- file.mtime(path_progress(path_store = store))
  progress <- progress_init(path_store = store)
  progress <- tibble::as_tibble(progress$database$read_condensed_data())
  progress <- progress[progress$type != "pattern",, drop = FALSE] # nolint
  out <- tibble::tibble(
    skipped = sum(progress$progress == "skipped"),
    dispatched = sum(progress$progress == "dispatched"),
    completed = sum(progress$progress == "completed"),
    errored = sum(progress$progress == "errored"),
    canceled = sum(progress$progress == "canceled"),
    since = units_seconds(difftime(Sys.time(), time, units = "secs")),
    time = time_stamp(time)
  )
  fields_quosure <- rlang::enquo(fields)
  fields <- tar_tidyselect_eval(fields_quosure, colnames(out)) %|||%
    colnames(out)
  out[, fields, drop = FALSE]
}

