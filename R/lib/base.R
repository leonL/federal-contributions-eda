library(rgdal, quietly=TRUE, warn.conflicts=FALSE)

# Constants & Config

if (!exists("k")) { k <- new.env() }
with(k, {

  party_tags <- c('Bloc', 'Conservative', 'Green', 'Liberal', 'NDP')

  party_names <- c("Bloc Québécois", "Conservative Party of Canada",
      "Green Party of Canada", "Liberal Party of Canada", "New Democratic Party")

  party_file_name_prefix <- c('Bloc Québécois', 'Conservative Party', 'Green Party',
      'Liberal Party', 'New Democratic Party')

  party_labels <- data.frame(name=party_names, filePrefix=party_file_name_prefix, tag=party_tags, row.names=party_tags, stringsAsFactors=FALSE)

  all_contribution_years <- as.character(c(2004:2014))

  src_data_file_names <- c(contributions = 'all_contributions.csv', summaries = 'summaries.csv')

  data_path <- '../data'

  # data subpaths dynamically defined to handle intial changes to data_path (e.g. for tests)
  data_src_path <- function() {
    if (is.null(src_path)) {
      src_path <<- paste(data_path, 'source', sep = '/')
    }
    return(src_path)
  }; src_path <- NULL

  data_output_path <- function() {
    if (is.null(output_path)) {
      output_path <<- paste(data_path, 'output', sep = '/')
    }
    return(output_path)
  }; output_path <- NULL
})

# Input & Output helpers

if (!exists("io")) { io <- new.env(parent=k) }
with(io, {

  read_src_csv <- function(filename, subfolder) {
    file <- paste(k$data_src_path(), subfolder, filename, sep = '/')
    flog.info("Reading %s ...", file)
    read.csv(file, as.is=TRUE, encoding="UTF-8")
  }

  read_src_ogr <- function(filename, subfolder) {
    path <- paste(k$data_src_path(), subfolder, sep = '/')
    flog.info("Reading %s/%s ...", path, filename)
    readOGR(path, filename, stringsAsFactors = FALSE)
  }

  get_contributions_csv <- function() {
    if (is.null(contributions_csv)) {
      contributions_csv <<-
        read_src_csv(src_data_file_names['contributions'], 'contributions')
    } else { test$text('cache') }
    return(contributions_csv)
  }; contributions_csv <- NULL

  get_summaries_csv <- function() {
    if (is.null(summaries_csv)) {
      summaries_csv <<-
        read_src_csv('summaries.csv', 'contributions')
    } else { test$text('cache') }
    return(summaries_csv)
  }; summaries_csv <- NULL

  get_riding_shps <- function() {
    if (is.null(riding_shps)) {
      riding_shps <<-
        read_src_ogr('ElectoralDistrictBoundaries', 'map_boundaries/electoral_districts')
    } else { test$text('cache') }
    invisible(riding_shps)
  }; riding_shps <- NULL

})

# Utility functions

util <- new.env(parent=io)

# Unit Test Helpers

if (!exists("test")) { test <- new.env() }
with(test, {

  running <- FALSE

  text <- function(txt) {
    if (running) { print(txt) }
  }
})

# Logging...

library(futile.logger, quietly=TRUE, warn.conflicts=FALSE)