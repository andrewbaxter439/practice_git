library(tidyverse)
library(SPHSUgraphs)

readpop <- function(country) {
  
  ccodes <- read_tsv("country_codes.txt")
  
  code <- ccodes %>% 
    filter(Country == country) %>% 
    pull(Code)
  
  filename <- paste("import/Population/",
                    code, ".Population.txt",sep="")
  
  pop <- read.fwf(filename, widths = c(7,13,20,16,16),sep="", skip=2,
                  header=TRUE)
  
  mutate(pop, Country = country, Age=as.numeric(as.character(Age)),
         Year=as.numeric(as.character(Year)))
  
}

group_pops <- function(pop_file) {
  
  popAgeGrps <- tibble(Age=factor(c(13:15, 15:17, 15:19)),
                       agegrp=factor(c(1,1,1,2,2,2,3,3,3,3,3),
                                     labels=c("Under 16", "Under 18", "Under 20"))) 
  pop_file %>% 
    merge(popAgeGrps) %>%
    group_by(Country, Year, agegrp) %>% 
    summarise(sumPops = sum(Total)) # create summary
  
}

graph_pops <- function(pop_group, year = c(1980, 2012)) {
  
  pop_group %>% 
    filter(Year > year[1], Year < year[2]) %>% 
    ggplot(aes(x = Year, y = sumPops, fill = agegrp, colour = agegrp)) +
    geom_area(position = position_stack(reverse = TRUE), alpha = 0.8) +
    theme_sphsu_light()
  
}