source("R/functions.R")

countries <- c(
  "England and Wales",
  "Scotland",
  "Ireland",
  "Switzerland",
  "Denmark",
  "France",
  "Spain",
  "Portugal",
  "Greece"
)

allPops <- countries %>% 
  purrr::map_dfr(~ readpop(.x)) %>% 
  group_pops()



allPops %>% 
  graph_pops() +
  facet_wrap(~ Country, scales = "free")
