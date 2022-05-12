library(tidyverse)


########### load adult segment volumes first ###################

# these are just the width measurement combinations used for indexing and animal IDs
adult_best_model_widths <- read.csv('data_minimized/L3/0309_adult_minmodels.csv')

true_vols <- read_csv("data_minimized/L2/all_adults_full_models_old-version.csv")
#true_vols <- read_csv("data/L1/width_combination_results/all_adults_full_models.csv")

true_vols$Animal_ID <- unique(adult_best_model_widths$Animal_ID)

colnames(true_vols)[1:18] <- 1:18

true_vols <- pivot_longer(true_vols, cols = `1`:`18`, names_to = 'segment')

# calculate total body volume
true_vols <- true_vols %>% group_by(Animal_ID) %>% mutate(body_volume = sum(value))

# rename columns for clarity
true_vols <- true_vols %>% rename(
  
  true_segment_volume = 'value',
  true_body_volume = 'body_volume'
)  %>% mutate(
  
  rep_class = 'adult',
  segment = as.numeric(segment)
)


##### load JUVENILE stuff next  ###########
`%ni%` <- Negate(`%in%`)

all_juvie_data <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv') %>% mutate(
  
  Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
  Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID)
  
) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1') & rep_class=='juvenile') # remove the 2 repeats


juvie_data_w_repeats <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv') %>% mutate(
  
  Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
  Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID)
  
) %>% filter(rep_class=='juvenile')


juv_full_model_vols <- read_csv('data_minimized/L2/juv-full-models_old-version.csv') %>% mutate(
  
  Animal_ID = juvie_data_w_repeats$Animal_ID
  
) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1'))


#juv_full_model_vols$Animal_ID <- all_juvie_data$Animal_ID

colnames(juv_full_model_vols)[1:18] <- 1:18

juv_full_model_vols <- pivot_longer(juv_full_model_vols, cols = `1`:`18`, names_to = 'segment')

# calculate total body volume
juv_full_model_vols <- juv_full_model_vols %>% group_by(Animal_ID) %>% mutate(body_volume = sum(value)) %>% mutate(
  segment = as.numeric(segment),
  rep_class = 'juvenile'
) %>% rename(
  
  true_segment_volume = 'value',
  true_body_volume = 'body_volume'
)


#############################################
# combine them

all_full_models <- bind_rows(true_vols, juv_full_model_vols)

############################################


#### load elliptical segment volumes from FULL models ####

full_elliptical_seg_vols <- read_csv('data_minimized/L2/20220426_full-elliptical-model-seg-volumes.csv')

colnames(full_elliptical_seg_vols)[1:18] <- 1:18

full_elliptical_seg_vols <- full_elliptical_seg_vols %>% mutate(Animal_ID = ifelse(Animal_ID==lead(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID)) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1'))


full_elliptical_seg_vols <- pivot_longer(full_elliptical_seg_vols, cols = `1`:`18`, names_to = 'segment') %>% mutate(
  
  segment=as.numeric(segment)) %>% rename(
    
    candidate_segment_vol = 'value'
  )

full_elliptical_seg_vols <- left_join(full_elliptical_seg_vols, all_full_models, by = c('Animal_ID', 'segment', 'rep_class'))

full_elliptical_seg_vols <- full_elliptical_seg_vols %>% mutate(
  
  segment_error = candidate_segment_vol - true_segment_volume,
  prop_seg_error = abs(segment_error/true_body_volume)
  
) # prop is absolute

# add up error
full_elliptical_seg_vols_cse <- full_elliptical_seg_vols %>%
  group_by(Animal_ID, rep_class) %>%
  summarise(total_prop_error=sum(prop_seg_error)) %>% mutate(
    
    model= 'full'
  )


write_csv(full_elliptical_seg_vols_cse, file = 'data_minimized/L2/20220426_full-elliptical-models.csv')




#### load elliptical segment volumes from MINIMALLY SCALED models ####


elliptical_seg_vols <- read_csv('data_minimized/L2/20220426_elliptical-model-seg-volumes.csv')

colnames(elliptical_seg_vols)[1:18] <- 1:18

elliptical_seg_vols <- pivot_longer(elliptical_seg_vols, cols = `1`:`18`, names_to = 'segment') %>% mutate(
  
  segment=as.numeric(segment)) %>% rename(
    
    candidate_segment_vol = 'value'
  )

elliptical_seg_vols <- left_join(elliptical_seg_vols, all_full_models, by = c('Animal_ID', 'segment', 'rep_class'))

elliptical_seg_vols <- elliptical_seg_vols %>% mutate(
         
         segment_error = candidate_segment_vol - true_segment_volume,
         prop_seg_error = abs(segment_error/true_body_volume)
         
         ) # prop is absolute
  
  # add up error
elliptical_cse <- elliptical_seg_vols %>%
    group_by(Animal_ID, model, rep_class) %>%
    summarise(total_prop_error=sum(prop_seg_error))

ggplot(elliptical_cse, aes(Animal_ID, total_prop_error, color=rep_class)) + geom_boxplot()

ggplot(elliptical_cse, aes(rep_class, total_prop_error, color=rep_class)) + geom_boxplot()


write_csv(elliptical_cse, file = 'data_minimized/L3/20220426_optimal-elliptical-models.csv')
