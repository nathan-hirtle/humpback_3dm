############## this file calculates the proportion of volume in each segment of the whale for adults and juveniles seperately, then uses a t-test adjusted for multiple comparisons to determine differences in proportions ###########################

library(tidyverse)


`%ni%` <- Negate(`%in%`)


full_model_data <- read_csv('data_minimized/L2/20220510_full-models.csv') %>% mutate(
  
  Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
  Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID)
  
) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1')) # remove the 2 repeats


## transform data ##
full_model_data <- pivot_longer(full_model_data, cols = `0`:`17`, names_to = 'segment', values_to = 'segment_volume') %>% 
  mutate(segment=as.numeric(segment)+1)


full_model_data <- full_model_data %>% group_by(Animal_ID) %>% mutate(total_vol = sum(segment_volume)) %>% mutate(
  
  prop_seg_vol = segment_volume/total_vol
  
)

full_model_data %>% group_by(rep_class) %>% summarise(mean(total_vol), sd(total_vol))

# make sure it sums to one
#sum(full_model_data$prop_seg_vol[1:18])

full_model_data <- full_model_data %>% rename('Repr. class' = rep_class)


library(rstatix)

full_model_data <- full_model_data %>% ungroup()

full_model_data$segment_fac <- as.factor(full_model_data$segment)
full_model_data$Animal_ID <- as.factor(full_model_data$Animal_ID)
full_model_data <- full_model_data %>% rename(rep_class = `Repr. class`)



foo <- full_model_data %>%
  group_by(segment) %>%
  t_test(
    prop_seg_vol ~ rep_class) %>% 
  adjust_pvalue(method = "bonferroni")

# which segments are different between adults and juveniles?
filter(foo, p.adj < 0.05)

# only the 18th








