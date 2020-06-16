
library(tidyverse)
library(gt)
library(patchwork)
library(gganimate)

cv19 <- read_csv('data/usa_covid19.csv')

# exercise 1 --------------------------------------------------------------

e1_soln <- cv19 %>% 
  group_by(state) %>%
  mutate(
    cases_new = cases_total - lag(cases_total, default = 0),
    deaths_new = deaths_total - lag(deaths_total, default = 0)
  )

write_rds(e1_soln, 'solutions/01_solution.rds')

# exercise 2 --------------------------------------------------------------

e2_soln <- e1_soln %>% 
  group_by(date) %>% 
  summarise_at(vars(ends_with('new')), sum) %>% 
  filter(date >= '2020-03-01')

write_rds(e2_soln, 'solutions/02_solution.rds')


# exercise 3 --------------------------------------------------------------

p_cases = ggplot(e2_soln) + 
  aes(x = date, y = cases_new) + 
  geom_bar(stat = 'identity', color = 'grey', fill = 'red') + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  labs(x = '', y = 'Number of new COVID19 cases')

p_deaths <- ggplot(e2_soln) + 
  aes(x = date, y = deaths_new) + 
  geom_bar(stat = 'identity', color = 'grey', fill = 'red') + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  labs(x = '', y = 'Number of new COVID19 related deaths')

e3_soln <- p_cases + p_deaths + 
  plot_annotation(title = 'COVID-19 cases and deaths in the United States')

ggsave(plot = e3_soln, filename = '03_solution.png', device = 'png', 
       dpi = 300, height = 5, width = 7, units = 'in', path = 'solutions')

# exercise 4 -------------------------------------------------------------- 

cv19_states <- e1_soln %>% 
  group_by(state) %>% 
  arrange(desc(date)) %>% 
  slice(1:14) %>% 
  summarize(
    cases_total    = cases_total[1],
    deaths_total   = deaths_total[1],
    cases_per100k  = cases_total[1] / pop_2015[1] * 100000,
    deaths_per100k = deaths_total[1] / pop_2015[1] * 100000,
    cases_mar      = mean(cases_new[1:7]) / mean(cases_new[8:14]),
    deaths_mar     = mean(deaths_new[1:7]) / mean(deaths_new[8:14]) 
  )

domain <- cv19_states %>% 
  ungroup() %>% 
  arrange(desc(deaths_per100k)) %>%
  slice(1:10) %>% 
  pull(deaths_per100k) %>% 
  range(na.rm=TRUE)

e4_soln <- cv19_states %>%
  ungroup() %>% 
  arrange(desc(deaths_per100k)) %>%
  slice(1:10) %>% 
  select(
    state,
    cases_total,
    cases_per100k,
    cases_mar,
    deaths_total,
    deaths_per100k,
    deaths_mar
  ) %>%
  ungroup() %>%
  gt(rowname_col = 'state') %>%
  cols_label(
    cases_total = 'Total count',
    cases_per100k = 'Rate per 100k',
    cases_mar = 'Moving average ratio',
    deaths_total = 'Total count',
    deaths_per100k = 'Rate per 100k',
    deaths_mar = 'Moving average ratio'
  ) %>% 
  fmt_number(columns = vars(cases_total, deaths_total), decimals = 0) %>% 
  fmt_number(columns = vars(cases_per100k, cases_mar,
    deaths_per100k, deaths_mar), decimals = 1) %>% 
  cols_align(align = 'center') %>% 
  data_color(
    columns = vars(deaths_per100k),
    colors = scales::col_numeric(
      palette = c("white", "orange"),
      domain = domain)
  ) %>% 
  tab_spanner(label = 'Cases', 
    columns = vars(cases_total, cases_per100k, cases_mar)) %>% 
  tab_spanner(label = 'Deaths', 
    columns = vars(deaths_total, deaths_per100k, deaths_mar)) %>% 
  tab_header(
    title = 'Ten states in the US with highest death rates due to COVID-19',
    subtitle = paste("Data presented for:", max(cv19$date))
  )

write_rds(e4_soln, 'solutions/04_solution.rds')


# exercise 5 --------------------------------------------------------------

ranked_by_date <- cv19 %>% 
  group_by(date) %>% 
  arrange(date, -cases_total) %>% 
  mutate(rank = 1:n()) %>% 
  filter(rank <= 10) %>% 
  mutate(cases_total = log(cases_total+1))

e5_soln <- ggplot(ranked_by_date) +
  aes(xmin = 0 , xmax = cases_total) +  
  aes(ymin = rank - .45, ymax = rank + .45, y = rank) +  
  facet_wrap(~ date) +  
  geom_rect(alpha = .7) +  
  aes(fill = state) +
  scale_fill_viridis_d(option = "magma", direction = -1) +  
  scale_x_continuous(
    limits = c(-2.5, max(ranked_by_date$cases_total))
  ) +
  geom_text(
    col = "gray13",  
    hjust = "right", 
    aes(label = state, x = -1/2)
  ) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Log total number of COVID19 cases',
    title = "US states with highest case counts on {frame_time}") +  
  labs(y = "") +  
  theme_classic() +
  theme(legend.position = '') +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  facet_null() +
  aes(group = state) + 
  transition_time(date) + 
  ease_aes('linear')

e5_soln <- animate(e5_soln,
  nframes = 1000,
  fps = 60,
  width = 96 * 8,
  height = 96 * 6
)

anim_save(filename = '05_solution.gif', path = 'solutions')





