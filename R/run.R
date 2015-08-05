source('lib/eda.R', chdir=TRUE)

party_donations$riding_totals_for_year_bracket(2014, 'C') %>% rename(FEDUID=contributor.riding_id)
lflt_plots$riding_choropleth_map()