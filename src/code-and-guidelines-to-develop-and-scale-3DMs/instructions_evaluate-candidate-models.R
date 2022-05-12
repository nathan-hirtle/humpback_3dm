
################## this file evaluates candidate models that were used to scale all whales in the dataset to identify those that are 'optimal models'  ###################


library(tidyverse)


###################### ADULT PROCESSING ################################


###################### the below code is included only to recreate intermediate variables ##########


# the below file is not included in this repo
segment_vols <- read_csv("src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L4/mean_adult_combos.csv")


seg_vols_longer <- pivot_longer(segment_vols, cols = `0`:`17`, names_to = 'segment')

seg_vols_longer$segment <- as.numeric(seg_vols_longer$segment)+1

full_model <- filter(seg_vols_longer, model==max(model))$value

seg_vol_error <- seg_vols_longer %>% 
  mutate(error=value-full_model)

cum_seg_error <- seg_vol_error %>% 
  group_by(model) %>%
  summarise(cs_error=sum(abs(error))) # we want absolute error


n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)

test_df$model <- 1:nrow(test_df)

width_presence <- test_df %>% mutate(n_widths = rowSums(across(Var1:Var17)))


cse_by_nwidths <- left_join(width_presence, cum_seg_error, by='model')


# convert to proportional error

adult_cse <- cse_by_nwidths %>% mutate(prop_error=cs_error/sum(full_model))


# extract ALL the models with < 5% error with 2-5 widths
adult_min_models <- filter(adult_cse, n_widths %in% c(2,3,4,5) & prop_error < 0.05)


adult_best_width_combos <- filter(width_presence, model %in% adult_min_models$model) # gives the combinations only, no data


################################# end recreation of intermediate variables ################



# these are just the width measurement combinations used for indexing later
#adult_best_model_widths <- read.csv('data_minimized/L3/0309_adult_minmodels.csv')



true_vols <- read_csv("src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L2/example_volumes.csv")


colnames(true_vols)[1:18] <- 1:18

true_vols <- pivot_longer(true_vols, cols = `1`:`18`, names_to = 'segment')

# calculate total body volume
true_vols <- true_vols %>% group_by(Animal_ID) %>% mutate(body_volume = sum(value))

# rename columns for clarity
true_vols <- true_vols %>% rename(
  
  true_segment_volume = 'value',
  true_body_volume = 'body_volume'
)



# these are the actual volume estimates from the CANDIDATE models
# this file not included in this repo
all_adults_candidate_models <- read_csv("src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L6/adult_candidate_model_volumes.csv")


colnames(all_adults_candidate_models)[1:18] <- 1:18


all_adults_candidate_models <- pivot_longer(all_adults_candidate_models, cols = `1`:`18`, names_to = 'segment')

all_adults_candidate_models <-  all_adults_candidate_models %>% rename(
  
  candidate_segment_vol = 'value'
)

# append the correct segment and total body volumes
all_adults_candidate_models <- left_join(all_adults_candidate_models, true_vols, by = c('Animal_ID', 'segment'))

# calculate segment volume error
all_adults_candidate_models <- all_adults_candidate_models %>% group_by(Animal_ID, model) %>%   
  mutate(segment_error = candidate_segment_vol - true_segment_volume,
         prop_seg_error = abs(segment_error/true_body_volume)) # prop is absolute

# add up error
adult_cum_seg_error <- all_adults_candidate_models %>%
  group_by(Animal_ID, model) %>%
  summarise(total_prop_error=sum(prop_seg_error))

# the threshold of 5% here is specific to humpback whales, but can be adapted to fit a specific goal
adult_accepted_models <- adult_cum_seg_error %>% group_by(model) %>% filter(., quantile(total_prop_error, 0.95) < 0.05)

# this writes the data out
# this file not included in this repo
write_csv(adult_accepted_models, "src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L7/adult_optimal_models.csv")

################################ JUVENILE PROCESSING #########################################

###################### the below code is included only to recreate intermediate variables ##########


# the below file is not included in this repo
segment_vols <- read_csv("src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L4/mean_juv_combos.csv")


seg_vols_longer <- pivot_longer(segment_vols, cols = `0`:`17`, names_to = 'segment')

seg_vols_longer$segment <- as.numeric(seg_vols_longer$segment)+1

full_model <- filter(seg_vols_longer, model==max(model))$value

seg_vol_error <- seg_vols_longer %>% 
  mutate(error=value-full_model)

cum_seg_error <- seg_vol_error %>% 
  group_by(model) %>%
  summarise(cs_error=sum(abs(error))) # we want absolute error


n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)

test_df$model <- 1:nrow(test_df)

width_presence <- test_df %>% mutate(n_widths = rowSums(across(Var1:Var17)))


cse_by_nwidths <- left_join(width_presence, cum_seg_error, by='model')


# convert to proportional error

juv_cse <- cse_by_nwidths %>% mutate(prop_error=cs_error/sum(full_model))


# extract ALL the models with < 5% error with 2-5 widths
juv_min_models <- filter(juv_cse, n_widths %in% c(2,3,4,5) & prop_error < 0.05)


juv_best_width_combos <- filter(width_presence, model %in% juv_min_models$model) # gives the combinations only, no data


################################# end recreation of intermediate variables ################



true_vols <- read_csv("src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L2/example_volumes.csv")


colnames(true_vols)[1:18] <- 1:18

true_vols <- pivot_longer(true_vols, cols = `1`:`18`, names_to = 'segment')

# calculate total body volume
true_vols <- true_vols %>% group_by(Animal_ID) %>% mutate(body_volume = sum(value))

# rename columns for clarity
true_vols <- true_vols %>% rename(
  
  true_segment_volume = 'value',
  true_body_volume = 'body_volume'
)



# these are the actual volume estimates from the CANDIDATE models
# this file not included in this repo
all_juv_candidate_models <- read_csv("src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L6/juv_candidate_model_volumes.csv")


colnames(all_juv_candidate_models)[1:18] <- 1:18


all_juv_candidate_models <- pivot_longer(all_juv_candidate_models, cols = `1`:`18`, names_to = 'segment')

all_juv_candidate_models <-  all_juv_candidate_models %>% rename(
  
  candidate_segment_vol = 'value'
)

# append the correct segment and total body volumes
all_juv_candidate_models <- left_join(all_juv_candidate_models, true_vols, by = c('Animal_ID', 'segment'))

# calculate segment volume error
all_juv_candidate_models <- all_juv_candidate_models %>% group_by(Animal_ID, model) %>%   
  mutate(segment_error = candidate_segment_vol - true_segment_volume,
         prop_seg_error = abs(segment_error/true_body_volume)) # prop is absolute

# add up error
adult_cum_seg_error <- all_juv_candidate_models %>%
  group_by(Animal_ID, model) %>%
  summarise(total_prop_error=sum(prop_seg_error))

# the threshold of 5% here is specific to humpback whales, but can be adapted to fit a specific goal
adult_accepted_models <- adult_cum_seg_error %>% group_by(model) %>% filter(., quantile(total_prop_error, 0.95) < 0.05)

# this writes the data out
# this file not included in this repo
write_csv(adult_accepted_models, "src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L7/adult_optimal_models.csv")
