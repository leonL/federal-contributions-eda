source('../R/lib/eda.R', chdir=TRUE)
source('config.R')

context('util')

test_that('init_summaries_df returns monetray values in dollars', {
  result <- util$init_summaries_df()
  result_id1 <- filter(result, id == 1)
  expect_equal(result_id1$total_contributions, 1000.27)
})

test_that('init_long_summaries_df returns correctly transposed summary data', {
  result <- util$init_long_summaries_df()
  result_2004_lib_B <- filter(result, contrib.year == 2004, contrib.bracket == 'B', party == "Liberal", !donee.riding_level)
  expect_equal(result_2004_lib_B$n_contributors, 50)
})

test_that('filter_donee_type returns the expected subset', {
  df <- io$get_contributions_csv()
  result <- util$filter_donee_type(df, TRUE)
  expect_equal(result$id, c(1, 2))
})

test_that('classify_donation_totals returns the correct bracket codes', {
  result <- util$classify_donation_totals(c(200, 200.01, 300, 399.99, 420, 599, 600, 1000000))
  expect_equal(result, c(NA, 'C', 'D', 'D', 'E', 'E', 'F', 'F'))
})

test_that('map_summary_count_labels_to_bracket_codes returns a valid mapping', {
  v <- c('abc.200_or_less', 'xxx.20_or_less', 'yyy.200_or_less')
  result <- util$map_summary_count_labels_to_bracket_codes(v)
  expect_equal(result, c('B', 'A', 'B'))
})

context('party_donations')

test_that('all returns only the donations given directly to parties', {
  result <- party_donations$all()
  expect_false(any(c(1, 2) %in% result$id))
})

test_that('summaries returns only those for donations to parties', {
  result <- party_donations$summaries()
  expect_false(4 %in% result$id)
})

test_that('summaries_long returns only those for donations to parties', {
  result <- party_donations$summaries_long()
  expect_false(4 %in% result$id)
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

context('party_totals')

test_that('by_year returns expected summary', {
  result <- party_totals$by_year()
  result_2004 <- filter(result, contrib.year == 2004)
  expect_equal(result_2004$grand.total_contributions, 1220.39)
})

test_that('for_onymous_brackets_by_party_year returns the expected summary', {
  result <- party_totals$for_onymous_brackets_by_party_year()
  result_NDP_2014_F <- filter(result, party == 'NDP', contrib.year == 2014, contrib.bracket == 'F')
  expect_equal(result_NDP_2014_F$total_contributions, 2100)
})

test_that('for_onymous_by_party_year_from_individual_contirbs returns the expected summary', {
  result <- party_totals$for_onymous_by_party_year_from_individual_contirbs()
  result_NDP_2014 <- filter(result, party == 'NDP', contrib.year == 2014)
  expect_equal(result_NDP_2014$n_contributors, 2)
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