###### this file calculates the error for the minimal 3D models and elliptical models, and also the full elliptical model relative to the full 3D model ###############

library(tidyverse)


elliptical_cse <- read_csv('data_minimized/L3/20220426_optimal-elliptical-models.csv')

elliptical_cse <- elliptical_cse %>% mutate(method = 'elliptical')

elliptical_cse$model <- as.character('minimal')



full_elliptical_cse <- read_csv('data_minimized/L3/20220426_full-elliptical-models.csv')

full_elliptical_cse <- full_elliptical_cse %>% mutate(
  
  method = 'elliptical')

full_elliptical_cse$model <- as.character('full')

accepted_adult_models <- read_csv('data_minimized/L5/20220426_optimal-adult-models.csv')

accepted_adult_models <- accepted_adult_models %>% mutate(
  
  method = '3DM')

accepted_adult_models$model <- as.character('minimal')

accepted_juv_models <- read_csv('data_minimized/L5/20220426_optimal-juv-models.csv') 

accepted_juv_models <-  accepted_juv_models %>% 
  
  select(-c('cs_error', 'perfect_vol')) %>%
  
  rename(total_prop_error = 'prop_error') %>%
  
  mutate(
    
    method = '3DM')

accepted_juv_models$model <- as.character('minimal')


#fake_no_error_df <- data.frame(Animal_ID = rep('fake',2), model = rep('full',2), rep_class = c('adult', 'juvenile'), total_prop_error = rep(0,2), method = rep('3DM', 2))

all_methods_cse <- bind_rows(full_elliptical_cse, elliptical_cse, accepted_adult_models, accepted_juv_models)#, fake_no_error_df)


ggplot(all_methods_cse, aes(rep_class, total_prop_error, color = method, linetype = model)) + geom_boxplot() + geom_hline(yintercept=0.05) + coord_cartesian(ylim=c(0,0.25)) + theme_bw()


#### making the 2 variables only one variable

all_methods_cse <- all_methods_cse %>% 
  mutate(model = stringr::str_to_title(model),
         method = ifelse(method=='elliptical', 'Elliptical', method)) %>%
  mutate(`model and method` = paste0(model, ',\n', method)) 

# filter outliers
smry_df <- all_methods_cse %>% group_by(Animal_ID, `model and method`, rep_class) %>% summarise(mean_error_perc = mean(total_prop_error)*100) %>% mutate(rep_class = stringr::str_to_title(rep_class)) %>% filter(mean_error_perc < 15)







