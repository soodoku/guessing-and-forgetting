test_that("R code is lint-free", {
  paths <- c("R", "scripts", "tests")
  lints <- purrr::map(paths, ~ lintr::lint_dir(project_file(.x))) |>
    purrr::flatten()
  expect_length(lints, 0)
})
