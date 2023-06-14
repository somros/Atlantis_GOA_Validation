# Is the catch.ts file catching as much as it should?
# In other words, is the output as big as the input?
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

# deal with time
origin_year <- 1990 # this should be the start of the model run, as per init.nc
origin <- as.Date(paste0(origin_year, '-01-01')) # what year did the catch start?
imposecatchstart <- 2920
catch_init <- (imposecatchstart / 365) + 1 # First year of catch.ts in the model 
catch_init <- catch_init + 1 # catch.txt output file has an empty row at the start

# open a recent run
this_run <- 1208
this_dir <- paste0('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/out_', this_run)
# get catch txt file
catch_file <- paste0('outputGOA0', this_run, '_testCatch.txt')
catch_output <- read.table(paste(this_dir, catch_file, sep = '/'), header = TRUE)

catch_output <- catch_output %>%
  select(Time, atlantis_fg[atlantis_fg$IsImpacted == 1,]$Code) %>%
  filter(Time >= 365 * catch_init) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'Catch') %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  mutate(Time = floor((Time / (365))) - catch_init + origin_year) %>%
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
  rename(input = Catch.x, output = Catch.y) %>%
  pivot_longer(c(input, output), names_to = 'Type', values_to = 'Catch')

catch_comp %>%
  #filter(Year < 1996) %>%
  ggplot(aes(x = Year, y = Catch, color = Type))+
  geom_point()+
  geom_line()+
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  theme_bw()+
  facet_wrap(~Name, scales = 'free')

# shows struggle with migrating species
# showcasing problem with Capelin if we have it as 0 in the catch - it will explode (solution is set the first years constant)
