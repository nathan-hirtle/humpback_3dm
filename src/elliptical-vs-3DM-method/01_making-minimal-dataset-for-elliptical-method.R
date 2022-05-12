
library(tidyverse)

bmesh_run_1 <- read_csv("data_minimized/L2/0308_adult_0to30k.csv")

bmesh_run_2 <- read_csv("data_minimized/L2/0308_adult_30to60k.csv")

bmesh_run_3 <- read_csv("data_minimized/L2/0308_adult_60to90k.csv")

bmesh_run_4 <- read_csv("data_minimized/L2/0308_adult_90ktoend.csv")

segment_vols= rbind(bmesh_run_1, bmesh_run_2, bmesh_run_3, bmesh_run_4)

segment_vols$iteration=1:nrow(segment_vols) 


seg_vols_longer <- pivot_longer(segment_vols, cols = `0`:`17`, names_to = 'segment')

seg_vols_longer$segment <- as.numeric(seg_vols_longer$segment)+1

full_model <- filter(seg_vols_longer, iteration==max(iteration))$value

seg_vol_error <- seg_vols_longer %>% 
  mutate(error=value-full_model)

cum_seg_error <- seg_vol_error %>% 
  group_by(iteration) %>%
  summarise(cs_error=sum(abs(error))) # we want absolute error


n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)

test_df$iteration <- 1:nrow(test_df)

width_presence <- test_df %>% mutate(n_widths = rowSums(across(Var1:Var17)))


cse_by_nwidths <- left_join(width_presence, cum_seg_error, by='iteration')



# convert to proportional error

cse_by_nwidths <- cse_by_nwidths %>% mutate(prop_error=cs_error/sum(full_model))

cse_by_nwidths$rep_class <- 'adult'

adult_cse <- cse_by_nwidths


# extract ALL the models with < 5% error with 2-5 widths
adult_min_models <- filter(adult_cse, n_widths %in% c(2,3,4,5) & prop_error < 0.05)

#nrow(filter(juv_cse, prop_error < 0.05))

adult_best_width_combos <- filter(width_presence, iteration %in% adult_min_models$iteration)



adult_accepted_models2 <- read_csv('data_minimized/L5/20220426_optimal-adult-models.csv')


head(adult_accepted_models2)

adult_best_model_widths <- read.csv('data_minimized/L3/0309_adult_minmodels.csv')

# assign the model # to the actual width values
adult_best_model_widths$model <- rep(adult_best_width_combos$iteration, length(unique(adult_best_model_widths$Animal_ID)))

# filter the widths that were 'good enough' for adults
adult_accepted_widths <- filter(adult_best_model_widths, model %in% adult_accepted_models2$model) %>% rename (
  
  TL = 'WL'
)


############## juvenile stuff ################
bmesh_run_1 <- read_csv("data_minimized/L2/0119_juv_75k.csv")

bmesh_run_2 <- read_csv("data_minimized/L2/0119_juv_end.csv")

no_width_data <- read_csv('data_minimized/L2/0119_juv_no-widths.csv')

segment_vols <-  bind_rows(no_width_data, bmesh_run_1, bmesh_run_2)

segment_vols$iteration <- segment_vols$iteration+1


seg_vols_longer <- pivot_longer(segment_vols, cols = `0`:`17`, names_to = 'segment')

seg_vols_longer$segment <- as.numeric(seg_vols_longer$segment)+1

full_model <- filter(seg_vols_longer, iteration==max(iteration))$value

seg_vol_error <- seg_vols_longer %>% 
  mutate(error=value-full_model)

cum_seg_error <- seg_vol_error %>% 
  group_by(iteration) %>%
  summarise(cs_error=sum(abs(error))) # we want absolute error


# calculate number of widths in each iteration
n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)

test_df$iteration <- 1:nrow(test_df)

width_presence <- test_df %>% mutate(n_widths = rowSums(across(Var1:Var17)))



cse_by_nwidths <- left_join(width_presence, cum_seg_error, by='iteration')

# convert to proportional error

cse_by_nwidths <- cse_by_nwidths %>% mutate(prop_error=cs_error/sum(full_model))


# extract ALL the models with < 5% error with 5-6 widths
juv_min_models <- filter(cse_by_nwidths, n_widths %in% c(5,6) & prop_error < 0.05)

nrow(filter(juv_min_models))

juv_best_width_combos <- filter(width_presence, iteration %in% juv_min_models$iteration)

##################



# load all the ~500k width combos that made it through the first round of tests
juv_candidate_model_widths <- read_csv('data_minimized/L3/0120_candidate-adult-models_juv-data.csv')

`%ni%` <- Negate(`%in%`)

juvie_data_w_repeats <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv') %>% mutate(
  
  Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
  Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID)
  
) %>% filter(rep_class=='juvenile')



juv_candidate_model_widths$Animal_ID <- rep(juvie_data_w_repeats$Animal_ID, each=nrow(juv_best_width_combos))



juv_candidate_model_widths <- juv_candidate_model_widths %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1'))


full_model_data <- read_csv('data_minimized/L2/juv-full-models_old-version.csv') %>% mutate(
  
  Animal_ID = juvie_data_w_repeats$Animal_ID
  
) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1'))





# assign model numbers
juv_candidate_model_widths$model <- rep(juv_min_models$iteration, length(unique(full_model_data$Animal_ID)))


juv_accepted_models <- read_csv('data_minimized/L5/20220426_optimal-juv-models.csv')

# assign rep class
juv_accepted_widths <- filter(juv_candidate_model_widths, model %in% juv_accepted_models$model) %>% mutate(
  
  rep_class = 'juvenile'
  
) 

# rename columns to match adult dataset
colnames(juv_accepted_widths)[3:19] <- paste0('V', 4:20)

# combine
both_acc_widths <- bind_rows(adult_accepted_widths, juv_accepted_widths)

# make column names more clear
colnames(both_acc_widths)[4:20] <- paste('width_', sep = '', seq(5,85,5))

write_csv(both_acc_widths, file = 'data_minimized/L6/20220426_widths-for-ellipse-scaling.csv')
