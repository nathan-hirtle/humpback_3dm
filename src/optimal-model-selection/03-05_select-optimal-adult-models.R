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

adult_best_width_combos <- filter(width_presence, iteration %in% adult_min_models$iteration) # gives the combinations only, no data


##########

# this loop takes a while to run
# code below saves the output as a .csv which is loaded below 
# commented out to save time when reprocessing the data

# adult_morphs <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv') %>% filter(rep_class=='adult') %>% select(-'Image')
# 
# best_models <- data.frame()
# 
# for (i in 1:15) { # for each whale
# 
# df1 <- data.frame()
# 
# for (k in 1:nrow(adult_best_width_combos)) {
# 
# v1 <- c()
# 
# for (j in 1:17) { # for each width for one whale
# 
#    v1[j] <- ifelse(adult_best_width_combos[k,j], adult_morphs[i,j+3], NA)
# 
# 
# }
# 
# df1[k,1:3] <- adult_morphs[i,c(1,2,3)]
# 
# df1[k,4:20] <- v1
# 
# }
# 
# 
# best_models <- rbind(best_models, df1)
# 
#   }

# this writes the data out
# write.csv(best_models, file = 'data_minimized/L3/0309_adult_minmodels.csv', row.names = F)






# these are just the width measurement combinations used for indexing later
adult_best_model_widths <- read.csv('data_minimized/L3/0309_adult_minmodels.csv')



# these are actual volume estimates from FULL models
# NOTE: since this analysis was performed, the final models were updated such
# that the FULL model volumes are slightly different. The candidate models
# here are being compared to the full models generated at that point in time
# so that it is an "apples to apples" comparison.

true_vols <- read_csv("data_minimized/L2/all_adults_full_models_old-version.csv")

true_vols$Animal_ID <- unique(adult_best_model_widths$Animal_ID)

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
all_adults_best_models_1 <- read_csv("data_minimized/L4/0309_best_mods_adult_1.csv")

all_adults_best_models_2 <- read_csv("data_minimized/L4/0309_best_mods_adult_2.csv")


all_adults_best_models <- rbind(all_adults_best_models_1, all_adults_best_models_2)

colnames(all_adults_best_models)[1:18] <- 1:18

all_adults_best_models$Animal_ID <- adult_best_model_widths$Animal_ID

all_adults_best_models$model <- rep(adult_best_width_combos$iteration, 15)

all_adults_best_models <- pivot_longer(all_adults_best_models, cols = `1`:`18`, names_to = 'segment')

all_adults_best_models <-  all_adults_best_models %>% rename(

  candidate_segment_vol = 'value'
)

# append the correct segment and total body volumes
all_adults_best_models <- left_join(all_adults_best_models, true_vols, by = c('Animal_ID', 'segment'))

# calculate segment volume error
all_adults_best_models <- all_adults_best_models %>% group_by(Animal_ID, model) %>%   # 4539 models for each animal
  mutate(segment_error = candidate_segment_vol - true_segment_volume,
         prop_seg_error = abs(segment_error/true_body_volume)) # prop is absolute

# add up error
adult_cum_seg_error <- all_adults_best_models %>%
  group_by(Animal_ID, model) %>%
  summarise(total_prop_error=sum(prop_seg_error))

adult_accepted_models2 <- adult_cum_seg_error %>% group_by(model) %>% filter(., quantile(total_prop_error, 0.95) < 0.05)

adult_accepted_models2$rep_class <- 'adult'

# this writes the data out
write_csv(adult_accepted_models2, 'data_minimized/L5/20220426_optimal-adult-models.csv')
