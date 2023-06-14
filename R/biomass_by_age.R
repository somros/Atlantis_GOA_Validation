# Alberto Rovellini
# 2/14/2023
# Check biomass by age class

library(dplyr)
library(tidyr)
library(ggplot2)

# read in model groups
atlantis_fg <- read.csv('data/GOA_Groups.csv')

# open a recent run
this_run <- 865
this_dir <- paste0('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/out_', this_run)
# get biomass txt file
biom_age_file <- paste0('outputGOA0', this_run, '_testAgeBiomIndx.txt')
biom_age_output <- read.table(paste(this_dir, biom_age_file, sep = '/'), header = TRUE)

# plot
biom_age_output %>%
  pivot_longer(-Time, names_to = 'Code.Class', values_to = 'mt') %>%
  separate_wider_delim(Code.Class, delim = '.', names = c('Code','Class')) %>%
  filter(Code == 'COD', Time < 730, Class > 4) %>%
  ggplot(aes(x = Time, y = mt, color = Class))+
  geom_line(linewidth = 2)+
  theme_bw()+
  facet_wrap(~Code, scales = 'free')

# biom_age_output %>%
#   pivot_longer(-Time, names_to = 'Code.Class', values_to = 'mt') %>%
#   separate_wider_delim(Code.Class, delim = '.', names = c('Code','Class')) %>%
#   filter(Code == 'COD', Time == 0) %>%
#   pull(mt) %>% 
#   sum()

# compare to total
# get biomass txt file
# biom_file <- paste0('outputGOA0', this_run, '_testBiomIndx.txt')
# biom_output <- read.table(paste(this_dir, biom_file, sep = '/'), header = TRUE)
# 
# b1 <- biom_output %>%
#   select(Time, KWT:DR) %>%
#   pivot_longer(-Time, names_to = 'Code', values_to = 'mt') %>%
#   mutate(Type = 'Total')
# 
# b2 <- biom_age_output %>%
#   pivot_longer(-Time, names_to = 'Code.Class', values_to = 'mt') %>%
#   separate_wider_delim(Code.Class, delim = '.', names = c('Code','Class')) %>%
#   group_by(Time, Code) %>%
#   summarise(mt = sum(mt)) %>%
#   ungroup() %>%
#   mutate(Type = 'Age')
# 
# rbind(b1, b2) %>%
#   #filter(Type == 'Age') %>%
#   ggplot(aes(x = Time, y = mt, color = Type))+
#   geom_line(linewidth = 1)+
#   theme_bw()+
#   facet_wrap(~Code, scales = 'free')

# total and by age biomass output appear to be the same
