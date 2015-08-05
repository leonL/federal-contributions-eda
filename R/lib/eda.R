source('base.R')

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)
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

lflt_plots <- new.env(parent=party_donations)
with(lflt_plots, {

  riding_shps_with_data <- function(riding_data) {
    shps <- get_riding_shps()
    shps@data <- join(shps@data, riding_data)
    shps@data[is.na(shps@data)] <- 0
    invisible(shps)
  }

  pal <- function() {
    colorNumeric(palette = 'Blues', domain = riding_totals_by_year_bracket()[['contrib.']])
  }

  riding_choropleth_map <- function(riding_data, col, p) {
    sp_data <- riding_shps_with_data(riding_data)
    # pal <- colorNumeric(palette = 'Oranges', domain = sp_data[[col]])
    map <- leaflet(sp_data) %>% addTiles() %>%
      addPolygons(stroke = TRUE,
                  weight = 1,
                  color = 'black',
                  fillOpacity = 0.7,
                  smoothFactor = 0.5,
                  fillColor = ~p(sp_data[[col]]))
    return(map)
  }
})

with(util, {

  filter_donee_type <- function(df, riding_level = FALSE) {
    filter(df, donee.riding_level==riding_level)
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
})