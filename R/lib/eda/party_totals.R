party_totals <- new.env(parent=party_donations)
with(party_totals, {

  for_onymous_by_party_year_from_summary <- function() {
    select(summaries(), party, contrib.year, total_contributions.summary = total_contributions.over_200,
            n_contributors.summary = n_contributors.over_200)
  }

  for_onymous_by_party_year_from_individual_contirbs <- function() {
    set <- group_by(for_onymous_brackets_by_party_year(), party, contrib.year) %>%
      summarise(total_contributions = sum(total_contributions), n_contributors = sum(n_contributors))
    as.data.frame(set)
  }

  summary_to_individ_comparison_table <- function() {
    smry_to_2014 <- filter(for_onymous_by_party_year_from_summary(), contrib.year != 2015)
    df <- merge(smry_to_2014, for_onymous_by_party_year_from_individual_contirbs())
    df <- mutate(df, n.diff = n_contributors.summary - n_contributors,
                  n.accuracy = n_contributors / n_contributors.summary,
                  total.diff = total_contributions.summary - total_contributions,
                  total.accuracy = total_contributions / total_contributions.summary) %>%
          arrange(total.accuracy)
  }

  for_onymous_brackets_by_party_year <- function() {
    set <- group_by(donor_smry_by_year_party(), contrib.bracket, party, contrib.year) %>%
      summarise(total_contributions = sum(contrib.total), n_contributors = n_distinct(contributor_id)) %>%
        filter(!is.na(contrib.bracket))
    as.data.frame(set)
  }

  for_brackets_by_party_year <- function() {
    rbind(for_onymous_brackets_by_party_year(), summaries_long())
  }

  by_year <- function() {
    set <- group_by(summaries(), contrib.year) %>%
      summarise(grand.total_contributions = sum(total_contributions),
                grand.n_contributors = sum(n_contributors),
                grand.total_contributions.over_200 = sum(total_contributions.over_200),
                grand.total_contributions.200_or_less = sum(total_contributions.200_or_less),
                grand.total_contributions.20_or_less = sum(total_contributions.20_or_less),
                grand.n_contributors.over_200 = sum(n_contributors.over_200),
                grand.n_contributors.200_or_less = sum(n_contributors.200_or_less),
                grand.n_contributors.20_or_less = sum(n_contributors.20_or_less))
    as.data.frame(set)
  }
})