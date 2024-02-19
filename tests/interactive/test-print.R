tar_test("print stem", {
  # resources <- tar_resources(
  #   clustermq = tar_resources_clustermq(
  #     template = list(cpu = 1, mem = 2)
  #   )
  # )
  x <- tar_target(x, {
    a <- 1
    b
  })
  print(x)
  expect_equal(1, 1)
})

tar_test("print map", {
  print(tar_target(x, 1, pattern = map(y, z)))
  expect_equal(1, 1)
})

tar_test("print cross", {
  print(tar_target(x, 1, cross(y, z)))
  expect_equal(1, 1)
})

tar_test("print empty pipeline", {
  print(pipeline_init())
  expect_equal(1, 1)
})

tar_test("print pipeline with 1 target", {
  print(suppressWarnings(pipeline_init(list(tar_target(x, 1, cross(y, z))))))
  expect_equal(1, 1)
})

tar_test("print pipeline with 2 targets", {
  print(
    pipeline_init(
      list(
        tar_target(x, 1),
        tar_target(y, 1)
      )
    )
  )
  expect_equal(1, 1)
})

tar_test("print cue", {
  print(tar_cue())
  expect_equal(1, 1)
})


tar_test("print fst resources", {
  print(resources_fst_init(compress = 50))
  expect_equal(1, 1)
})

tar_test("print future resources", {
  print(resources_future_init(resources = list(a = 1)))
  expect_equal(1, 1)
})

tar_test("print url resources", {
  skip_if_not_installed("curl")
  print(resources_url_init(handle = curl::new_handle()))
  expect_equal(1, 1)
})
