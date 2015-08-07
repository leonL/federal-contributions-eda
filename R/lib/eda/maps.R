lflt_plots <- new.env(parent=party_donations)
with(lflt_plots, {

  riding_shps_with_data <- function(riding_data) {
    shps <- get_riding_shps()
    shps@data <- join(shps@data, riding_data)
    shps@data[is.na(shps@data)] <- 0
    invisible(shps)
  }

  pal <- function(colour, vec) {
    colorNumeric(palette = colour, domain = c(0, max(vec)))
  }

  riding_choropleth_map <- function(riding_data, col, p) {
    sp_data <- riding_shps_with_data(riding_data)
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