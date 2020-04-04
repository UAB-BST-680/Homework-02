
library(tidyverse)

cv19_text <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

cv19 <- read_csv(url(cv19_text)) %>% 
  select(-fips)

blank <- cv19 %>% 
  ungroup() %>% 
  select(state) %>% 
  distinct()

out <- split(cv19, f = cv19$date)

na_to_zero <- function(x){
  
  x[is.na(x)] <- 0
  x
  
}

out <- map_dfr(out, 
  .f = ~right_join(.x, blank) %>% 
    mutate(across(c(cases, deaths), na_to_zero)) %>% 
    tidyr::fill(date)
) %>% 
  rename(cases_total = cases, deaths_total = deaths)
  
state_pop <- usmap::countypop %>% 
  group_by(abbr) %>% 
  summarise(pop_2015 = sum(pop_2015))
  
state_key <- datasets::state.abb %>%
  set_names(datasets::state.name) %>%
  enframe(name = 'state', value = 'abbr') %>%
  add_row(state = 'District of Columbia', abbr = 'DC')

state_pop <- state_pop %>% 
  left_join(state_key) %>%
  select(state, pop_2015)

out %>% 
  left_join(state_pop) %>% 
  write_csv('data/usa_covid19.csv')

