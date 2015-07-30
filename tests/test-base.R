source('../R/lib/base.R', chdir=TRUE)
source('config.R')

context('io')

test_that('read_contributions_csv caches the csv file on initial read', {
  expect_true(is.null(io$contributions_csv))
  io$read_contributions_csv()
  expect_false(is.null(io$contributions_csv))
  expect_output(io$read_contributions_csv(), "cache")
})