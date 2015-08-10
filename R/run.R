source('lib/eda.R', chdir=TRUE)

library(ggplot2)

# summary and aggregated totals comparison table
comparison_df <- party_totals$summary_to_individ_comparison_table()

# stacked area charts of grand totals by brackets (faceted by party)
bracket_totals <- party_totals$for_brackets_by_party_year()
bracket_totals <- filter(bracket_totals, contrib.year != 2015) %>% arrange(contrib.bracket)
ggplot(bracket_totals, aes(x=contrib.year, y=total_contributions, fill=contrib.bracket)) + geom_area() + facet_grid(. ~ party)
ggplot(bracket_totals, aes(x=contrib.year, y=n_contributors, fill=contrib.bracket)) + geom_area() + facet_grid(. ~ party)

# example of leaflet plot
riding_smry_2014_NDP <- party_donations$riding_totals_for_year_party(2014, 'NDP') %>% rename(FEDUID=contributor.riding_id)
oranges <- lflt_plots$pal('Oranges', riding_smry_2014_NDP$contrib.total)
lflt_plots$riding_choropleth_map(riding_smry_2014_NDP, 'contrib.total', oranges)
