test_that("the deposited input files are unchanged", {
  expect_true(verify_sources())
})

test_that("the poll manifest and data agree", {
  expect_equal(nrow(poll_manifest), 23)
  item_counts <- purrr::map_int(poll_manifest$file, function(file) {
    poll <- read_poll(file)
    expect_equal(ncol(poll$pre), length(lucky_probabilities[[file]]))
    ncol(poll$pre)
  })
  expect_equal(sum(item_counts), 177)
})
