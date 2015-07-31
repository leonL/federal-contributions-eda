source('../R/lib/eda.R', chdir=TRUE)
source('config.R')

context('sub_sets')

test_that('filter_donee_type returns the expected subset', {
  df <- io$get_contributions_csv()
  result <- sub_sets$filter_donee_type(df, TRUE)
  expect_equal(result$id, c(1, 2))
})

context('party_donations')

test_that('all returns only the donations given directly to parties', {
  result <- party_donations$all()
  expect_false(any(c(1, 2) %in% result$id))
})

test_that('donor_totals_by_year_party returns expected totals', {
  result <- party_donations$donor_totals_by_year_party()
  result <- filter(result, party == 'Conservative', contributor_id == 256200)
  expect_equal(result$contrib.total, 200)
})

test_that('riding_totals_by_year_party returns expected totals', {
  result <- party_donations$riding_totals_by_year_party()
  result_t <- filter(result, party == 'Conservative', contributor.riding_id == 10002)
  expect_equal(result_t$contrib.total, 200)
  result_n <- filter(result, party == 'Conservative', contributor.riding_id == 10001, contrib.year == 2011)
  expect_equal(result_n$contrib.n, 3)
})

test_that('riding_totals_for_year_party returns a correctly filtered subset', {
  result <- party_donations$riding_totals_for_year_party(2013, 'Conservative')
  result_10001 <- filter(result, contributor.riding_id == 10001)
  expect_equal(result_10001$contrib.total, 502)
  expect_equal(result_10001$contrib.n, 2)
})