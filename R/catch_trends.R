# Is the catch.ts file catching as much as it should?
# If not, we may need measures like borrowing from other cells, etc.
# In the early stages, this will help us understand whether groups do not decline because they are too productive
# or because not enough gets caught

# at first do this as total, then focus on individual boxes (map where things get undercaught and borrowed from eventually)
# make some maps for the appendix / validation purposes
# may then compare to fish tickets data to make sure that removals occur in space in a meaningful way

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(data.table)

# read in model groups
atlantis_fg <- read.csv('data/GOA_Groups.csv')

# open a recent run
this_run <- 851
this_dir <- paste0('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/out_', this_run, '-2')
# get catch txt file
catch_file <- paste0('outputGOA0', this_run, '_testCatch.txt')
catch_output <- read.table(paste(this_dir, catch_file, sep = '/'), header = TRUE)

catch_output <- catch_output %>%
  select(Time, atlantis_fg[atlantis_fg$IsImpacted == 1,]$Code) %>%
  filter(Time > 365*40) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'Catch') %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  mutate(Time = floor((Time / (365)) - 40 + 1990)) %>%
  select(Time, Name, Catch) %>%
  rename(Year = Time) # get catch.ts files - treat these as true catch or best approximation thereof


# get catch.ts files
catch_files <- c(list.files('C:/Users/Alberto Rovellini/Documents/GOA/Catch_data/output/AKFIN/', full.names = T), 
                 list.files('C:/Users/Alberto Rovellini/Documents/GOA/Catch_data/output/DFO/', full.names = T))

# extract column names
columns <- readLines(catch_files[1])
columns <- columns[grepl('long_name',columns)]
columns <- gsub('## COLUMN..long_name ', '', columns)
columns <- gsub('## COLUMN...long_name ', '', columns)

origin <- as.Date('1991-01-01')

catch_by_box <- list()

for(i in 1:length(catch_files)){
  
  this_catch <- read.table(catch_files[i], skip = 309)
  colnames(this_catch) <- columns
  
  this_box <- as.numeric(gsub('.ts', '', gsub('.*catch', '', catch_files[i])))
  
  this_catch_long <- this_catch %>%
    pivot_longer(-Time, values_to = 'Catch_mgN_day', names_to = 'Code') %>%
    mutate(Box = this_box,
           Date = origin + Time,
           Year = year(Date),
           Month = month(Date),
           Catch_mt_day = Catch_mgN_day * 20 * 5.7 * 60 * 60 * 24 / 1e9,
           Catch_mt_month = Catch_mt_day * 30) %>%
    select(Box, Year, Month, Code, Catch_mt_month)
  
  catch_by_box[[i]] <- this_catch_long
}

catch_by_box <- rbindlist(catch_by_box) 

# aggregate to total by year and across boxes
catch_obs <- catch_by_box %>%
  group_by(Year, Box, Code) %>%
  summarise(Catch_mt_year = sum(Catch_mt_month)) %>%
  group_by(Year, Code) %>%
  summarise(Catch = sum(Catch_mt_year)) %>%
  ungroup() %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  select(-Code)

# compare
catch_comp <- catch_obs %>% 
  left_join(catch_output, by = c('Year', 'Name')) %>%
  rename(obs = Catch.x, pred = Catch.y) %>%
  pivot_longer(c(obs, pred), names_to = 'Type', values_to = 'Catch')

catch_comp %>%
  #filter(Year < 1996) %>%
  ggplot(aes(x = Year, y = Catch, color = Type))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~Name, scales = 'free')

# shows struggle with migrating species
# odd phenomenon for some groups of some catch at t1 and then flattens out - investigate
# I suspect F is still operating then (possibly for part of the year only)

# showcasing problem with Capelin if we have it as 0 in the catch - it will explode
