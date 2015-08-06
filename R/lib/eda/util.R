with(util, {

  init_summaries_df <- function() {
    if (is.null(summaries_df)) {
      set <- get_summaries_csv()
      isMonetary <- grep("total_contributions", names(set))
      set[isMonetary] <- set[isMonetary] / 100
      summaries_df <<- merge(set, party_tag_name_mapping) %>%
        mutate(donee.riding_level = !federal_contribution, contrib.year=year) %>%
          select(-party_name, -federal_contribution, -year)
    }
    summaries_df
  }; summaries_df <- NULL

  init_long_summaries_df <- function() {
    if (is.null(long_summaries_df)) {
      simple_summaries <- select(init_summaries_df(), -total_contributions, -total_contributions.over_200,
                            -n_contributors, -n_contributors.over_200)
      total_contribs <-
        gather(select(simple_summaries, -total_contributions.200_or_less, -total_contributions.20_or_less),
                        contrib.bracket, n_contributors, n_contributors.200_or_less, n_contributors.20_or_less)
      total_contribs$contrib.bracket <- map_summary_count_labels_to_bracket_codes(total_contribs$contrib.bracket)

      n_contribs <-
        gather(select(simple_summaries, -n_contributors.200_or_less, -n_contributors.20_or_less),
                        contrib.bracket, total_contributions, total_contributions.200_or_less, total_contributions.20_or_less)
      n_contribs$contrib.bracket <- map_summary_count_labels_to_bracket_codes(n_contribs$contrib.bracket)

      long_summaries_df <<- merge(total_contribs, n_contribs)
    }
    return(long_summaries_df)
  }; long_summaries_df <- NULL

  filter_donee_type <- function(df, riding_level = FALSE) {
    filter(df, donee.riding_level==riding_level)
  }

  drop_riding_summary_cols <- function(set) {
    select(set, -donee.riding_level, -riding.name, -riding.id)
  }

  classify_donation_totals <- function(totals) {
    chr <- vector(mode="character", length=length(totals))
    chr[totals > 200 & totals < 300] <- 'C'
    chr[totals >= 300 & totals < 400] <- 'D'
    chr[totals >= 400 & totals < 600] <- 'E'
    chr[totals >= 600] <- 'F'
    chr[totals <= 200] <- NA
    return(chr)
  }

  map_summary_count_labels_to_bracket_codes <- function(vec) {
    map <- character(length(vec))
    isBracketA <- grepl('20_or_less', vec)
    map[isBracketA] <- 'A'; map[!isBracketA] <- 'B'
    map
  }

  party_tag_name_mapping <- data.frame(party_name = party_names, party = party_tags)
})