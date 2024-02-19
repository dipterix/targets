tar_test("validate default options", {
  x <- options_init()
  expect_silent(x$validate())
})

tar_test("tidy_eval", {
  x <- options_init()
  expect_equal(x$get_tidy_eval(), TRUE)
  x$set_tidy_eval(FALSE)
  expect_equal(x$get_tidy_eval(), FALSE)
  x$reset()
  expect_equal(x$get_tidy_eval(), TRUE)
  expect_error(x$set_tidy_eval("bad"), class = "tar_condition_validate")
})

tar_test("packages", {
  x <- options_init()
  x$set_packages("x")
  expect_equal(x$get_packages(), "x")
  x$reset()
  expect_equal(x$get_packages(), options_init()$get_packages())
  expect_error(x$set_packages(123), class = "tar_condition_validate")
})

tar_test("imports", {
  x <- options_init()
  expect_equal(x$get_imports(), character(0))
  x$set_imports("x")
  expect_equal(x$get_imports(), "x")
  x$reset()
  expect_equal(x$get_imports(), character(0))
  expect_error(x$set_imports(123), class = "tar_condition_validate")
})

tar_test("library", {
  x <- options_init()
  expect_equal(x$get_library(), NULL)
  x$set_library("x")
  expect_equal(x$get_library(), "x")
  x$reset()
  expect_equal(x$get_library(), NULL)
  expect_error(x$set_library(123), class = "tar_condition_validate")
})

tar_test("envir", {
  x <- options_init()
  expect_equal(x$get_envir(), globalenv())
  envir <- new.env()
  x$set_envir(envir)
  expect_equal(x$get_envir(), envir)
  x$reset()
  expect_equal(x$get_envir(), globalenv())
  expect_error(x$set_envir(123), class = "tar_condition_validate")
})

tar_test("repository", {
  x <- options_init()
  expect_equal(x$get_repository(), "local")
  x$reset()
  expect_equal(x$get_repository(), "local")
  expect_error(x$set_repository(123), class = "tar_condition_validate")
})

tar_test("repository_meta", {
  x <- options_init()
  expect_equal(x$get_repository_meta(), "local")
  x$reset()
  expect_equal(x$get_repository_meta(), "local")
  expect_error(x$set_repository_meta(123), class = "tar_condition_validate")
})

tar_test("iteration", {
  x <- options_init()
  expect_equal(x$get_iteration(), "vector")
  x$set_iteration("list")
  expect_equal(x$get_iteration(), "list")
  x$reset()
  expect_equal(x$get_iteration(), "vector")
  expect_error(x$set_iteration("invalid"), class = "tar_condition_validate")
})

tar_test("error", {
  x <- options_init()
  expect_equal(x$get_error(), "stop")
  x$set_error("continue")
  expect_equal(x$get_error(), "continue")
  x$reset()
  expect_equal(x$get_error(), "stop")
  expect_error(x$set_error("invalid"), class = "tar_condition_validate")
})

tar_test("deprecated error = \"workspace\"", {
  x <- options_init()
  expect_warning(
    x$set_error("workspace"),
    class = "tar_condition_deprecate"
  )
})

tar_test("memory", {
  x <- options_init()
  expect_equal(x$get_memory(), "persistent")
  x$set_memory("transient")
  expect_equal(x$get_memory(), "transient")
  x$reset()
  expect_equal(x$get_memory(), "persistent")
  expect_error(x$set_memory("invalid"), class = "tar_condition_validate")
})

tar_test("garbage_collection", {
  x <- options_init()
  expect_equal(x$get_garbage_collection(), FALSE)
  x$set_garbage_collection(TRUE)
  expect_equal(x$get_garbage_collection(), TRUE)
  x$reset()
  expect_equal(x$get_garbage_collection(), FALSE)
  expect_error(x$set_garbage_collection(0), class = "tar_condition_validate")
})

tar_test("deployment", {
  x <- options_init()
  expect_equal(x$get_deployment(), "worker")
  x$set_deployment("main")
  expect_equal(x$get_deployment(), "main")
  x$reset()
  expect_equal(x$get_deployment(), "worker")
  expect_error(x$set_deployment("invalid"), class = "tar_condition_validate")
})

tar_test("priority", {
  x <- options_init()
  expect_equal(x$get_priority(), 0)
  x$set_priority(1)
  expect_equal(x$get_priority(), 1)
  x$reset()
  expect_equal(x$get_priority(), 0)
  expect_error(x$set_priority(-1), class = "tar_condition_validate")
})

tar_test("backoff", {
  x <- options_init()
  expect_equal(x$get_backoff()$max, 0.1)
  x$set_backoff(backoff_init(max = 1))
  expect_equal(x$get_backoff()$max, 1)
  x$reset()
  expect_equal(x$get_backoff()$max, 0.1)
  expect_error(x$set_backoff("nope"), class = "tar_condition_validate")
})

tar_test("deprecated backoff", {
  x <- options_init()
  expect_equal(x$get_backoff()$max, 0.1)
  suppressWarnings(
    expect_warning(
      x$set_backoff(1),
      class = "tar_condition_deprecate"
    )
  )
  expect_equal(x$get_backoff()$max, 1)
  x$reset()
  expect_equal(x$get_backoff()$max, 0.1)
  expect_error(x$set_backoff("nope"), class = "tar_condition_validate")
})

tar_test("storage", {
  x <- options_init()
  expect_equal(x$get_storage(), "main")
  x$set_storage("worker")
  expect_equal(x$get_storage(), "worker")
  x$reset()
  expect_equal(x$get_storage(), "main")
  expect_error(x$set_storage("invalid"), class = "tar_condition_validate")
})

tar_test("retrieval", {
  x <- options_init()
  expect_equal(x$get_retrieval(), "main")
  x$set_retrieval("worker")
  expect_equal(x$get_retrieval(), "worker")
  x$reset()
  expect_equal(x$get_retrieval(), "main")
  expect_error(x$set_retrieval("invalid"), class = "tar_condition_validate")
})

tar_test("cue", {
  x <- options_init()
  exp_default <- as.list(tar_cue())
  exp_new <- as.list(tar_cue(mode = "never"))
  expect_equal(as.list(x$get_cue()), exp_default)
  x$set_cue(tar_cue(mode = "never"))
  expect_equal(as.list(x$get_cue()), exp_new)
  x$reset()
  expect_equal(as.list(x$get_cue()), exp_default)
  expect_error(x$set_cue("invalid"), class = "tar_condition_validate")
})

tar_test("debug", {
  x <- options_init()
  expect_equal(x$get_debug(), character(0))
  x$set_debug("x")
  expect_equal(x$get_debug(), "x")
  x$reset()
  expect_equal(x$get_debug(), character(0))
  expect_error(x$set_debug(123), class = "tar_condition_validate")
})

tar_test("workspaces", {
  x <- options_init()
  expect_equal(x$get_workspaces(), character(0))
  x$set_workspaces(workspaces = "x")
  expect_equal(x$get_workspaces(), "x")
  x$reset()
  expect_equal(x$get_workspaces(), character(0))
  expect_error(x$set_workspaces(123), class = "tar_condition_validate")
})

tar_test("workspace_on_error", {
  x <- options_init()
  expect_equal(x$get_workspace_on_error(), FALSE)
  x$set_workspace_on_error(workspace_on_error = TRUE)
  expect_equal(x$get_workspace_on_error(), TRUE)
  x$reset()
  expect_equal(x$get_workspace_on_error(), FALSE)
  expect_error(x$set_workspace_on_error(123), class = "tar_condition_validate")
})

tar_test("seed", {
  x <- options_init()
  expect_equal(x$get_seed(), 0L)
  x$set_seed(seed = 57L)
  expect_equal(x$get_seed(), 57L)
  x$set_seed(seed = NA_integer_)
  expect_equal(x$get_seed(), NA_integer_)
  x$set_seed(seed = NA_real_)
  expect_equal(x$get_seed(), NA_integer_)
  x$set_seed(seed = NA)
  expect_equal(x$get_seed(), NA_integer_)
  x$reset()
  expect_equal(x$get_seed(), 0L)
  expect_error(x$set_seed(NULL), class = "tar_condition_validate")
  expect_error(x$set_seed("abc"), class = "tar_condition_validate")
  expect_error(x$set_seed(seq_len(4)), class = "tar_condition_validate")
})


tar_test("trust_object_timestamps", {
  x <- options_init()
  expect_equal(x$get_trust_object_timestamps(), TRUE)
  x$set_trust_object_timestamps(FALSE)
  expect_equal(x$get_trust_object_timestamps(), FALSE)
  x$reset()
  expect_equal(x$get_trust_object_timestamps(), TRUE)
  expect_error(
    x$set_trust_object_timestamps(0),
    class = "tar_condition_validate"
  )
})

tar_test("tar_option_export", {
  skip_cran()
  script <- path_script_default()
  tar_script(tar_target(x, 1), script = script)
  out <- tar_option_script(script = script)
  expect_true(is.list(out))
  names <- c(
    "packages",
    "imports",
    "library",
    "format",
    "repository",
    "repository_meta",
    "iteration",
    "error",
    "memory",
    "garbage_collection",
    "resources"
  )
  expect_true(all(names %in% names(out)))
})
