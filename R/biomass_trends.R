# Alberto Rovellini
# 02/09/2023
# Validating biomass trends

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# read in model groups
atlantis_fg <- read.csv('data/GOA_Groups.csv')

# open a recent run
this_run <- 1275
this_dir <- paste0('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/out_', this_run)
# get biomass txt file
biom_file <- paste0('outputGOA0', this_run, '_testBiomIndx.txt')
biom_output <- read.table(paste(this_dir, biom_file, sep = '/'), header = TRUE)

# some run properties to change
imposecatchstart <- 2920 # number of days before catch.ts kicks in, as in force.prm file
# imposecatchstart <- 2920 # number of days before catch.ts kicks in, as in force.prm file
burn_in <- imposecatchstart / 365 

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

# What species do we have in the ak data frame?
# ak %>% pull(Group) %>% unique() %>% sort()
# 
# [1] "Arrowtooth_flounder"     "Cod"                     "Deep_demersal"           "Dogfish"                
# [5] "Flatfish_deep"           "Flatfish_shallow"        "Flathead_sole"           "Halibut"                
# [9] "Pacific_ocean_perch"     "Pollock"                 "Rex_sole"                "Rockfish_demersal_shelf"
# [13] "Rockfish_pelagic_shelf"  "Rockfish_slope"          "Sablefish"               "Sculpins"               
# [17] "Skate_big"               "Skate_longnose"          "Skate_other"             "Thornyhead"        
  
bc <- bc %>% 
  select(Year, Species, TB) %>% 
  left_join(key_bc, by = 'Species') %>% 
  select(-Species) %>%
  drop_na() %>%
  group_by(Year, Group) %>%
  summarize(TB = sum(TB, na.rm = T)) %>%
  ungroup() %>%
  rename(TB_bc = TB)

# What species do we have in the bc data frame?
# bc %>% pull(Group) %>% unique() %>% sort()
# 
# [1] "Arrowtooth_flounder" "Cod"                 "Flatfish_shallow"    "Halibut"             "Pacific_ocean_perch"
# [6] "Pollock"             "Rockfish_slope"      "Sablefish"           "Thornyhead"   

# differences?
# setdiff((ak %>% pull(Group) %>% unique() %>% sort()), (bc %>% pull(Group) %>% unique() %>% sort()))
# 
# [1] "Deep_demersal"           "Dogfish"                 "Flatfish_deep"           "Flathead_sole"          
# [5] "Rex_sole"                "Rockfish_demersal_shelf" "Rockfish_pelagic_shelf"  "Sculpins"               
# [9] "Skate_big"               "Skate_longnose"          "Skate_other"       

# join and sum - where BC is not available just use AK for now
# TODO: fix this - either figure out an expansion factor or get more data for BC for recent years

obs <- ak %>%
  full_join(bc, by = c('Year','Group')) %>%
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
biom_output_hindcast <- biom_output %>%
  select(Time, atlantis_fg$Code) %>%
  #filter(Time > 365 * burn_in) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'TB') %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  mutate(Time = floor((Time / (365)) - burn_in + 1990)) %>%
  select(Time, Name, TB) %>%
  rename(Year = Time)
  
# compare
biom_comp <- obs %>%
  left_join(biom_output_hindcast, by = c('Year','Name')) %>%
  rename(Observed = TB.x, Predicted = TB.y) %>%
  pivot_longer(c(Predicted, Observed), names_to = 'Type', values_to = 'mt')

# plot t0 too.
t0 <- biom_output %>%
  select(Time, atlantis_fg$Code) %>%
  filter(Time == 0) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'TB') %>%
  left_join(atlantis_fg %>% select(Code, Name), by = 'Code') %>%
  filter(Name %in% biom_comp$Name)

# plot
plot_biom <- biom_comp %>%
  filter(Year < 2018) %>% # this is because several assessment stop around then so obs is incomplete
  ggplot(aes(x = Year, y = mt, color = Type))+
  geom_point(size = 2)+
  geom_line(linewidth = 1)+
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  geom_hline(data = t0, aes(yintercept = TB), color = 'orange', linetype = 'dashed', linewidth = 1.4)+
  theme_bw()+
  facet_wrap(~Name, scales = 'free')
plot_biom

ggsave(paste('output/biomass', this_run, 'png', sep = '.'), plot_biom, width = 12, height = 10)

# plot relative biomass index over the time series
# But this hides issues with scale and we need to be upfront
biom_comp_rel <- biom_comp %>%
  left_join((biom_comp %>% filter(Year == min(Year)) %>% rename(mt_start = mt) %>% select(Name, Type, mt_start))) %>%
  mutate(biom_rel = mt / mt_start)

plot_biom_rel <- biom_comp_rel %>%
  filter(Year < 2018) %>% # this is because several assessment stop around then so obs is incomplete
  ggplot(aes(x = Year, y = biom_rel, color = Type))+
  geom_point(size = 2)+
  geom_line(linewidth = 1)+
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  geom_hline(yintercept = 1, color = 'orange', linetype = 'dashed', linewidth = 1.4)+
  theme_bw()+
  facet_wrap(~Name, scales = 'free')
plot_biom_rel

ggsave(paste('output/biomass_rel', this_run, 'png', sep = '.'), plot_biom_rel, width = 12, height = 10)
