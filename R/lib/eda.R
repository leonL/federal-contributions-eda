source('base.R')

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)
library(leaflet, quietly=TRUE, warn.conflicts=FALSE)

party_donations <- new.env(parent=util)
with(party_donations, {

  all <- function() {
    if (is.null(all_donations)) {
      set <- io$get_contributions_csv()
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

  riding_totals_for_year_party <- function(year, party_name) {
    filter(riding_totals_by_year_party(), contrib.year == year, party == party_name) %>%
      select(-party, -contrib.year)
  }
})

lft_plots <- new.env(parent=util)
with(lft_plots, {

  riding_shps_with_totals_for_year_party <- function(year, party_name) {
    shps <- io$get_riding_shps()
    totals <- party_donations$riding_totals_for_year_party(year, party_name) %>%
      rename(FEDUID=contributor.riding_id)
    shps@data <- join(shps@data, totals)
    shps@data[is.na(shps@data)] <- 0
    invisible(shps)
  }

  map_total_for_year_party_by_riding <- function(year, party_name) {
    sp_data <- riding_shps_with_totals_for_year_party(year, party_name)
    pal <- colorNumeric(palette = 'Oranges', domain = sp_data$contrib.total)
    map <- leaflet(sp_data) %>% addTiles() %>%
      addPolygons(stroke = FALSE,
                  fillOpacity = 0.7,
                  smoothFactor = 0.5,
                  fillColor = ~pal(contrib.total))
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