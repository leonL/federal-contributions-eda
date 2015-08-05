source('../R/lib/eda.R', chdir=TRUE)
source('config.R')

context('util')

test_that('filter_donee_type returns the expected subset', {
  df <- io$get_contributions_csv()
  result <- util$filter_donee_type(df, TRUE)
  expect_equal(result$id, c(1, 2))
})

test_that('classify_donation_totals returns the correct bracket codes', {
  result <- util$classify_donation_totals(c(200, 200.01, 300, 399.99, 420, 599, 600, 1000000))
  expect_equal(result, c(NA, 'C', 'D', 'D', 'E', 'E', 'F', 'F'))
})

context('party_donations')

test_that('all returns only the donations given directly to parties', {
  result <- party_donations$all()
  expect_false(any(c(1, 2) %in% result$id))
})

test_that('donor_smry_by_year_party returns expected totals', {
  result <- party_donations$donor_smry_by_year_party()
  result <- filter(result, party == 'Conservative', contributor_id == 1)
  expect_equal(result$contrib.total, 1100)
  expect_equal(result$contrib.bracket, 'F')
})

test_that('riding_totals_by_year_party returns expected totals', {
  result <- party_donations$riding_totals_by_year_party()
  result_t <- filter(result, party == 'Conservative', contributor.riding_id == 10002)
  expect_equal(result_t$contrib.total, 200)
  result_n <- filter(result, party == 'Conservative', contributor.riding_id == 10001, contrib.year == 2011)
  expect_equal(result_n$contrib.n, 3)
})

test_that('riding_totals_by_year_party_bracket returns expected totals', {
  result <- party_donations$riding_totals_by_year_party_bracket()
  result_2011_F <- filter(result, contrib.year == 2011, contrib.bracket == 'F')
  expect_equal(result_2011_F$contrib.n, 2)
  expect_equal(result_2011_F$contrib.total, 2200)
})

test_that('riding_totals_for_year_party returns a correctly filtered subset', {
  result <- party_donations$riding_totals_for_year_party(2013, 'Conservative')
  result_10001 <- filter(result, contributor.riding_id == 10001)
  expect_equal(result_10001$contrib.total, 502)
  expect_equal(result_10001$contrib.n, 2)
})

test_that('riding_totals_for_year_party_bracket returns a correctly filtered subset', {
  result <- party_donations$riding_totals_for_year_party_bracket(2011, 'Conservative', 'F')
  expect_equal(result$contrib.n, 2)
  expect_equal(result$contrib.total, 2200)
})

context('lflt_plots')

test_that('riding_shps_with data returns a SpatialPolygonsDataFrame with coorect totals', {
  data <- party_donations$riding_totals_for_year_party(2013, 'Conservative') %>%
      rename(FEDUID=contributor.riding_id)
  result <- lflt_plots$riding_shps_with_data(data)
  expect_true('SpatialPolygonsDataFrame' %in% class(result))
  result_10001 <- result[result$FEDUID == 10001, ]
  expect_equal(result_10001$contrib.total, 502)
  expect_equal(result_10001$contrib.n, 2)
})