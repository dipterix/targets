#' @title Delete metadata.
#' @export
#' @family metadata
#' @description Delete the project metadata files from the local file system,
#'   the cloud, or both.
#' @inheritParams tar_meta_sync
#' @param delete Character of length 1, which location to delete the files.
#'   Choose `"local"` for local files, `"cloud"` for files on the cloud,
#'   or `"all"` to delete metadata files from both the local file system
#'   and the cloud.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_make()
#' tar_meta_delete()
#' })
#' }
tar_meta_delete <- function(
  meta = TRUE,
  progress = TRUE,
  process = TRUE,
  verbose = TRUE,
  delete = "all",
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_lgl(meta)
  tar_assert_scalar(meta)
  tar_assert_none_na(meta)
  tar_assert_lgl(progress)
  tar_assert_scalar(progress)
  tar_assert_none_na(progress)
  tar_assert_lgl(process)
  tar_assert_scalar(process)
  tar_assert_none_na(process)
  tar_assert_lgl(verbose)
  tar_assert_scalar(verbose)
  tar_assert_none_na(verbose)
  tar_assert_chr(delete)
  tar_assert_scalar(delete)
  tar_assert_none_na(delete)
  tar_assert_nzchar(delete)
  tar_assert_in(delete, c("all", "local", "cloud"))
  if (delete %in% c("all", "local")) {
    tar_assert_scalar(store)
    tar_assert_chr(store)
    tar_assert_none_na(store)
    tar_assert_nzchar(store)
    if (meta) {
      unlink(path_meta(store))
    }
    if (progress) {
      unlink(path_progress(store))
    }
    if (process) {
      unlink(path_process(store))
    }
  }
  if (delete %in% c("all", "cloud")) {
    tar_assert_script(script)
    options <- tar_option_script(script = script)
    old_repository_meta <- tar_options$get_repository_meta()
    old_resources <- tar_options$get_resources()
    on.exit({
      tar_options$set_repository_meta(old_repository_meta)
      tar_options$set_resources(old_resources)
    })
    tar_options$set_repository_meta(options$repository_meta)
    tar_options$set_resources(options$resources)
    if (meta) {
      database_meta(path_store = tempfile())$delete_cloud(verbose = verbose)
    }
    if (progress) {
      database_progress(path_store = tempfile())$delete_cloud(
        verbose = verbose
      )
    }
    if (process) {
      database_process(path_store = tempfile())$delete_cloud(verbose = verbose)
    }
  }
  invisible()
}
