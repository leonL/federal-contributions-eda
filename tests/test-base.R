source('../R/lib/base.R', chdir=TRUE)
source('config.R')

context('io')

test_that('get_contributions_csv caches the csv file on initial read', {
  expect_true(is.null(io$contributions_csv))
  io$get_contributions_csv()
  expect_false(is.null(io$contributions_csv))
  expect_output(io$get_contributions_csv(), "cache")
})

test_that('get_summaries_csv caches the csv file on initial read', {
  expect_true(is.null(io$summaries_csv))
  io$get_summaries_csv()
  expect_false(is.null(io$summaries_csv))
  expect_output(io$get_summaries_csv(), "cache")
})

test_that('get_riding_shps caches the csv file on initial read', {
  expect_true(is.null(io$riding_shps))
  io$get_riding_shps()
  expect_false(is.null(io$riding_shps))
  expect_output(io$get_riding_shps(), "cache")
})
