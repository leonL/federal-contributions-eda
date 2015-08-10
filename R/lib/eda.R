source('base.R')
source('eda/util.R')

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)
library(tidyr, quietly=TRUE, warn.conflicts=FALSE)
library(leaflet, quietly=TRUE, warn.conflicts=FALSE)

party_donations <- new.env(parent=util)
with(party_donations, {

  all <- function() {
    if (is.null(all_donations)) {
      set <- get_contributions_csv()
      all_donations <<- filter_donee_type(set)
    }
    all_donations
  }; all_donations <- NULL

  summaries <- function() {
    if (is.null(all_summaries)) {
      set <- init_summaries_df()
      all_summaries <<- filter_donee_type(set) %>% drop_riding_summary_cols()
    }
    return(all_summaries)
  }; all_summaries <- NULL

  summaries_long <- function() {
    if (is.null(all_summaries_long)) {
      set <- init_long_summaries_df()
      all_summaries_long <<- filter_donee_type(set) %>% drop_riding_summary_cols()
    }
    return(all_summaries_long)
  }; all_summaries_long <- NULL

  donor_smry_by_year_party <- function() {
    smry <- group_by(all(), contributor_id, contributor.riding_id, party, contrib.year) %>%
      summarise(contrib.total = sum(contrib.amount), contrib.max = max(contrib.amount),
                  contrib.bracket = classify_donation_totals(contrib.total))
    as.data.frame(smry)
  }

  riding_totals_by_year_party <- function() {
    smry <- group_by(all(), contributor.riding_id, party, contrib.year) %>%
      summarise(contrib.total = sum(contrib.amount), contrib.n = n_distinct(contributor_id))
    as.data.frame(smry)
  }

  riding_totals_by_year_bracket <- function() {
    smry <- group_by(donor_smry_by_year_party(),
              contributor.riding_id, contrib.year, contrib.bracket) %>%
              summarise(contrib.total = sum(contrib.total), contrib.n = n_distinct(contributor_id))
    as.data.frame(smry)
  }

  riding_totals_by_year_party_bracket <- function() {
    smry <- group_by(donor_smry_by_year_party(),
              contributor.riding_id, party, contrib.year, contrib.bracket) %>%
              summarise(contrib.total = sum(contrib.total), contrib.n = n_distinct(contributor_id))
    as.data.frame(smry)
  }

  riding_totals_for_year_party <- function(year, party_name) {
    filter(riding_totals_by_year_party(), contrib.year == year, party == party_name) %>%
      select(-party, -contrib.year)
  }

  riding_totals_for_year_bracket <- function(year, bracket) {
    filter(riding_totals_by_year_bracket(),
            contrib.year == year, contrib.bracket == bracket) %>%
      select(-contrib.year, -contrib.bracket)
  }

  riding_totals_for_year_party_bracket <- function(year, party_name, bracket) {
    filter(riding_totals_by_year_party_bracket(),
            contrib.year == year, party == party_name, contrib.bracket == bracket) %>%
      select(-party, -contrib.year, -contrib.bracket)
  }
})

source('eda/party_totals.R')
source('eda/maps.R')