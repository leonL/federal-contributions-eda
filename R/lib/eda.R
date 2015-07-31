source('base.R')

library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

sub_sets <- new.env()
with(sub_sets, {

  filter_donee_type <- function(df, riding_level = FALSE) {
    filter(df, donee.riding_level==riding_level)
  }
})

party_donations <- new.env(parent=sub_sets)
with(party_donations, {

  all <- function() {
    if (is.null(all_donations)) {
      set <- io$get_contributions_csv()
      all_donations <<- filter_donee_type(set)
    }
    all_donations
  }; all_donations <- NULL

  donor_totals_by_year_party <- function() {
    smry <- group_by(all(), contributor_id, party, contrib.year) %>%
      summarize(contrib.total=sum(contrib.amount), contrib.max=max(contrib.amount))
    as.data.frame(smry)
  }

  riding_totals_by_year_party <- function(party, year) {
    smry <- group_by(all(), contributor.riding_id, party, contrib.year) %>%
      summarize(contrib.total=sum(contrib.amount), contrib.n=n_distinct(contributor_id))
    as.data.frame(smry)
  }

  riding_totals_for_year_party <- function(year, party_name) {
    filter(riding_totals_by_year_party(), contrib.year == year, party == party_name) %>%
      select(-party, -contrib.year)
  }
})

riding_donations <- new.env(parent=sub_sets)