
library(tidyverse)

cv19_text <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

# Download data and drop fips since we aren't merging with census
cv19 <- read_csv(url(cv19_text)) %>% 
  select(-fips)

# format data so that each state has a row for each date
# originally states only had rows for dates when cases > 0

# blank will be merged in at each date
blank <- cv19 %>% 
  ungroup() %>% 
  select(state) %>% 
  distinct()

# create a dataset for each date
out <- split(cv19, f = cv19$date)

# after merging, non-existent case values will be NA
# we will assume those are 0
na_to_zero <- function(x){
  x[is.na(x)] <- 0
  x
}

# merge, set NA to zero and fill in dates
# and, as a bonus, make the names more clear
out <- map_dfr(out, 
  .f = ~right_join(.x, blank) %>% 
    mutate_at(vars(cases, deaths), na_to_zero) %>% 
    tidyr::fill(date)
) %>% 
  rename(cases_total = cases, deaths_total = deaths)

# merge in the state population data
# these have abbr instead of state names
state_pop <- usmap::countypop %>% 
  group_by(abbr) %>% 
  summarise(pop_2015 = sum(pop_2015))

# need to make a key to link abbr with state names  
state_key <- datasets::state.abb %>%
  set_names(datasets::state.name) %>%
  enframe(name = 'state', value = 'abbr') %>%
  add_row(state = 'District of Columbia', abbr = 'DC')

state_pop <- state_pop %>% 
  left_join(state_key) %>%
  select(state, pop_2015)

# save to data directory
out %>% 
  left_join(state_pop) %>% 
  write_csv('data/usa_covid19.csv')

