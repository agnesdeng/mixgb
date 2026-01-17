test_that("check_data passes clean data without intervention", {
  # Data with no issues
  df_clean <- data.frame(
    x = factor(c("A", "B", "A", "B")),
    y = c(1, 2, 3, 4)
  )

  # Should run without error and return the data as-is
  expect_no_error(res <- check_data(df_clean, verbose = FALSE))
  expect_equal(res, df_clean)
})

test_that("check_data handles single-level columns (User chooses to remove)", {
  df_single <- data.frame(
    keep = factor(c("A", "B")),
    drop_me = factor(c("Z", "Z")) # Only 1 level
  )

  # Mocking YOUR internal .user_action to return TRUE (Simulate "Yes, remove")
  with_mocked_bindings(
    {
      res <- check_data(df_single, verbose = FALSE)
      expect_false("drop_me" %in% colnames(res))
      expect_true("keep" %in% colnames(res))
      expect_equal(ncol(res), 1)
    },
    .user_action = function(...) TRUE
  )
})

test_that("check_data detects ID columns via default max_levels (User chooses Stop)", {
  # 10 rows. Default max_levels is 5 (50%).
  # 'id_col' has 10 levels, which is > 5.
  df_id <- data.frame(
    id_col = factor(letters[1:10]),
    val = 1:10
  )

  # Mocking .user_action to return FALSE (Simulate "Stop, I want to inspect")
  # We expect an error because your check_ID function calls stop/cli_abort on FALSE
  with_mocked_bindings(
    {
      expect_error(check_data(df_id, verbose = FALSE))
    },
    .user_action = function(...) FALSE
  )
})

test_that("check_data stops when user chooses to inspect constant variables", {
  df_problem <- data.frame(
    constant = factor(rep("A", 5))
  )

  # Simulate user selecting "Stop" (FALSE) for single level check
  with_mocked_bindings(
    {
      expect_error(check_data(df_problem, verbose = FALSE))
    },
    .user_action = function(...) FALSE
  )
})

test_that("check_data handles data.frame vs data.table correctly", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    a = factor(c("A", "A")), # single level
    b = 1:2
  )

  # Mock .user_action to return TRUE to allow the check to remove the column
  with_mocked_bindings(
    {
      res <- check_data(dt, verbose = FALSE)
      expect_s3_class(res, "data.table")
      # Check that the column was removed by reference or subsetting
      expect_equal(ncol(res), 1)
      expect_named(res, "b")
    },
    .user_action = function(...) TRUE
  )
})
