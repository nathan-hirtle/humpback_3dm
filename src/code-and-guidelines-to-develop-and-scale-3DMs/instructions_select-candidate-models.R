
###this file identifies candidate models to be used in the next step of optimal model selection.
###the specific number of widths required and error threshold is up to the user to decide.
### the code as written selects models with 2-5 widths and <5% error as sufficient.

library(tidyverse)

############################## ADULT PROCESSING ############################################


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


##########

# this loop generates the best width combinations identified above for each whale in your dataset. 
# it is very inefficient and takes a while to run.

adult_data <- read_csv('src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L0/example-MorphometriX-output.csv') %>% filter(rep_class=='adult') %>% select(-'Image')

 
best_models <- data.frame()

for (i in 1:nrow(adult_data)) { # for each whale

 df1 <- data.frame()

 for (k in 1:nrow(adult_best_width_combos)) {

 v1 <- c()

 for (j in 1:17) { # for each width for one whale

    v1[j] <- ifelse(adult_best_width_combos[k,j], adult_data[i,j+4], NA)


 }

 df1[k,1:3] <- adult_data[i,c(1,2,4)] # append metadata

 df1[k,4:20] <- v1

 }


 best_models <- rbind(best_models, df1)

   }

# this writes the data out
# the below file is not included in this repo
write.csv(best_models, file = 'src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L5/adult_candidate_models.csv', row.names = F)

### the above file should then be fed to the Blender file 'instructions_scale-models_output-SA-V', script 'all_adult_combos'.

### make sure to change the file names to refer to the correct dataset.


######################################## JUVENILE PROCESSING ###################################

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


##########

# this loop generates the best width combinations identified above for each whale in your dataset. 
# it is very inefficient and takes a while to run.

juv_data <- read_csv('src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L0/example-MorphometriX-output.csv') %>% filter(rep_class=='juv') %>% select(-'Image')


best_models <- data.frame()

for (i in 1:nrow(juv_data)) { # for each whale
  
  df1 <- data.frame()
  
  for (k in 1:nrow(juv_best_width_combos)) {
    
    v1 <- c()
    
    for (j in 1:17) { # for each width for one whale
      
      v1[j] <- ifelse(juv_best_width_combos[k,j], juv_data[i,j+4], NA)
      
      
    }
    
    df1[k,1:3] <- juv_data[i,c(1,2,4)] # append metadata
    
    df1[k,4:20] <- v1
    
  }
  
  
  best_models <- rbind(best_models, df1)
  
}

# this writes the data out
# the below file is not included in this repo
write.csv(best_models, file = 'src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L5/juv_candidate_models.csv', row.names = F)

### the above file should then be fed to the Blender file 'instructions_scale-models_output-SA-V', script 'all_juvenile_combos'.

### make sure to change the file names to refer to the correct dataset.







