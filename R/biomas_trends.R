# Alberto Rovellini
# 02/09/2023
# Validating biomass trends

library(dplyr)
library(tidyr)
library(ggplot2)

# read in model groups
atlantis_fg <- read.csv('data/GOA_Groups.csv')

# open a recent run
this_run <- 851
this_dir <- paste0('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/out_', this_run, '-2')
# get biomass txt file
biom_file <- paste0('outputGOA0', this_run, '_testBiomIndx.txt')
biom_output <- read.table(paste(this_dir, biom_file, sep = '/'), header = TRUE)

# Read observation (or stock assessment outputs)
# These are built in stitching_SDMs.Rmd 

ak <- read.csv('data/biomass_expanded_ak.csv')
bc <- read.csv('data/biomass_expanded_bc.csv')

source('R/key_names_to_codes.R')

# clean
ak <- ak %>% 
  select(Year, Species, TB) %>% 
  left_join(key_ak, by = 'Species') %>% 
  select(-Species) %>%
  drop_na() %>%
  group_by(Year, Group) %>%
  summarize(TB = sum(TB, na.rm = T)) %>%
  ungroup() %>%
  rename(TB_ak = TB)
  
bc <- bc %>% 
  select(Year, Species, TB) %>% 
  left_join(key_bc, by = 'Species') %>% 
  select(-Species) %>%
  drop_na() %>%
  group_by(Year, Group) %>%
  summarize(TB = sum(TB, na.rm = T)) %>%
  ungroup() %>%
  rename(TB_bc = TB)

# join and sum - where BC is not available just use AK for now
# TODO: fix this - either figure out an expansion factor or get more data for BC for recent years

obs <- ak %>%
  left_join(bc, by = c('Year','Group')) %>%
  filter(Year >= 1990) %>%
  rowwise() %>%
  mutate(both = ifelse(is.na(TB_ak) | is.na(TB_bc), 'n', 'y')) %>%
  mutate(across(TB_ak:TB_bc, ~replace_na(.,0))) %>%
  mutate(TB = TB_ak + TB_bc) %>%
  select(Year, Group, TB, both) %>%
  rename(Name = Group)

# obs %>%
#   ggplot(aes(x = Year, y = TB))+
#   geom_line()+
#   geom_point(aes(color = both))+
#   theme_bw()+
#   facet_wrap(~Group, scales = 'free')

# get model output
biom_output <- biom_output %>%
  select(Time, atlantis_fg$Code) %>%
  filter(Time > 365*40) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'TB') %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  mutate(Time = floor((Time / (365)) - 40 + 1990)) %>%
  select(Time, Name, TB) %>%
  rename(Year = Time)
  
# compare
biom_comp <- obs %>%
  left_join(biom_output, by = c('Year','Name')) %>%
  rename(obs = TB.x, pred = TB.y) %>%
  pivot_longer(c(pred,obs), names_to = 'Type', values_to = 'mt')


# plot
biom_comp %>%
  ggplot(aes(x = Year, y = mt, color = Type))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~Name, scales = 'free')
