# Alberto Rovellini
# 2/13/2023
# Check that total biomass and biomass by box outputs are identical

# COnclusion from running this script:
# BoxBiomass does not capture benthic groups and migrating groups
# Everything else seems identical between the BiomIndex and BoxBiomass.txt files

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

b1 <- biom_output %>%
  select(Time:DR) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt') %>%
  mutate(Type = 'all')

# by box
biom_box_file <- paste0('outputGOA0', this_run, '_testBoxBiomass.txt')
biom_box_output <- read.table(paste(this_dir, biom_box_file, sep = '/'), header = TRUE)

b2 <- biom_box_output %>%
  group_by(Time) %>%
  summarise_at(vars(KWT:DR), sum, na.rm = T) %>%
  ungroup() %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt') %>%
  mutate(Type = 'box') 

b3 <- rbind(b1,b2)

p <- b3 %>%
  ggplot(aes(x =Time, y = mt, color = Type))+
  geom_line()+
  facet_wrap(~Code, scales = 'free', ncol = 4)

ggsave('compare_all_vs_box.png', p, width = 8, height = 16)

