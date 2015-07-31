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

if (!exists("io")) { io <- new.env() }
with(io, {

  read_src_csv <- function(filename, subfolder) {
    file <- paste(k$data_src_path(), subfolder, filename, sep = '/')
    flog.info("Reading %s ...", file)
    read.csv(file, as.is=TRUE, encoding="UTF-8")
  }

  get_contributions_csv <- function() {
    if (is.null(contributions_csv)) {
      contributions_csv <<-
        read_src_csv(k$src_data_file_names['contributions'], 'contributions')
    } else {
      test$text('cache')
    }
    return(contributions_csv)
  }; contributions_csv <- NULL

})

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