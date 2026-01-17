test_that("save_vars handles index conversion and extra variables", {
  origin_names <- c("age", "bmi", "hyp", "chl")
  missing_vars <- c("hyp", "bmi", "chl")

  res <- save_vars(
    save.vars = c(1, 2, 3, 4),
    origin.names = origin_names,
    missing.vars = missing_vars
  )

  expect_equal(res, "age")
})

test_that("save_vars throws error on missing required variables", {
  origin_names <- c("age", "bmi", "hyp", "chl")
  missing_vars <- c("hyp", "bmi", "chl")

  expect_error(
    save_vars(save.vars = "age", origin_names, missing_vars),
    "does not contains all of them"
  )
})
