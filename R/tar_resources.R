#' @title Target resources
#' @export
#' @family resources
#' @description Create a `resources` argument for [tar_target()]
#'   or [tar_option_set()].
#' @section Resources:
#'   Functions [tar_target()] and [tar_option_set()]
#'   each takes an optional `resources` argument to supply
#'   non-default settings of various optional backends for data storage
#'   and high-performance computing. The `tar_resources()` function
#'   is a helper to supply those settings in the correct manner.
#'
#'   In `targets` version 0.12.2 and above, resources are inherited one-by-one
#'   in nested fashion from `tar_option_get("resources")`.
#' @return A list of objects of class `"tar_resources"` with
#'   non-default settings of various optional backends for data storage
#'   and high-performance computing.
#' @param fst Output of function `tar_resources_fst()`.
#'   Non-default arguments to `fst::read_fst()` and
#'   `fst::write_fst()` for `fst`-based storage formats.
#'   Applies to all formats ending with `"fst"` in the name.
#'   For details on formats, see the `format` argument of [tar_target()].
#' @param future Output of function `tar_resources_future()`.
#' @param network Output of function `tar_resources_network()`.
#'   Settings to configure how to handle unreliable network connections
#'   in the case of uploading, downloading, and checking data
#'   in situations that rely on network file systems or HTTP/HTTPS requests.
#'   Examples include retries and timeouts for internal storage management
#'   operations for `storage = "worker"` or `format = "file"`
#'   (on network file systems),
#'   `format = "url"`. These settings do not
#'   apply to actions you take in the custom R command of the target.
#' @param url Output of function `tar_resources_url()`.
#'   Non-default settings for storage formats ending with the `"_url"` suffix.
#'   These settings include the `curl` handle for extra control over HTTP
#'   requests. For details on formats, see the `format` argument of
#'   [tar_target()].
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   resources = tar_resources(
#'     future = tar_resources_future(resources = list(n_cores = 1))
#'   )
#' )
tar_resources <- function(
  fst = tar_option_get("resources")$fst,
  future = tar_option_get("resources")$future,
  network = tar_option_get("resources")$network,
  url = tar_option_get("resources")$url
) {
  envir <- environment()
  names <- names(formals(tar_resources))
  names <- names[!names %in% c("...")]
  out <- list()
  for (name in names) {
    value <- envir[[name]]
    class <- paste0("tar_resources_", name)
    message <- paste0(
      name,
      " argument to tar_resources() must be output from tar_resources_",
      name,
      "() or NULL."
    )
    if (!is.null(value)) {
      tar_assert_inherits(value, class, message)
      out[[name]] <- value
      resources_validate(value)
    }
  }
  out
}
