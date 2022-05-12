library(tidyverse)


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



##### make the subset dataframe to feed into blender
`%ni%` <- Negate(`%in%`)


width_data <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv') %>% mutate(
  
  Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
  Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID)
  
) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1')) # remove the 2 repeats




juv_morphs <- filter(width_data, rep_class=='juvenile') %>% select(-Image)



best_models <- data.frame()

# this loop takes a while to run
# code below saves the output as a .csv which is loaded below 
# commented out to save time when reprocessing the data

# for (i in 1:nrow(juv_morphs)) { # for each whale
#   
#   df1 <- data.frame()
#   
#   for (k in 1:nrow(juv_best_width_combos)) {
#     
#     v1 <- c()
#     
#     for (j in 1:17) { # for each width for one whale
#       
#       v1[j] <- ifelse(juv_best_width_combos[k,j], juv_morphs[i,j+3], NA)
#       
#       
#     }
#     
#     df1[k,1:3] <- juv_morphs[i,c(1,2,3)]
#     
#     df1[k,4:20] <- v1
#     
#   }
#   
#   
#   best_models <- rbind(best_models, df1)
#   
# }

# this writes the data out to get fed into blender
#write.csv(best_models, file = 'data_minimized/L3/0120_candidate-adult-models_juv-data.csv', row.names = F)




####### next step #######

candidate_models <- read_csv('data_minimized/L4/0121_all-juvie-candidate-models.csv')


juvie_data_w_repeats <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv') %>% mutate(
  
  Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
  Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID)
  
) %>% filter(rep_class=='juvenile')

#all_juvie_data <- read_csv('data/L1/0120_all-juvie-data.csv')

candidate_models$Animal_ID <- rep(juvie_data_w_repeats$Animal_ID, each=nrow(juv_best_width_combos))

candidate_models <- candidate_models %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1'))

# these are actual volume estimates from FULL models
# NOTE: since this analysis was performed, the final models were updated such
# that the FULL model volumes are slightly different. The candidate models
# here are being compared to the full models generated at that point in time
# so that it is an "apples to apples" comparison.

full_model_data <- read_csv('data_minimized/L2/juv-full-models_old-version.csv') %>% mutate(
  
  Animal_ID = juvie_data_w_repeats$Animal_ID
  
) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1'))



colnames(full_model_data)[1:18] <- 1:18

full_model_data <- pivot_longer(full_model_data, cols = `1`:`18`, names_to = 'segment')

# calculate total body volume
full_model_data <- full_model_data %>% group_by(Animal_ID) %>% mutate(body_volume = sum(value)) %>% mutate(
  segment = as.numeric(segment)
) %>% rename(
  
  true_segment_volume = 'value',
  true_body_volume = 'body_volume'
)



model_and_nwidths_df <- juv_best_width_combos %>% slice(rep(row_number(), 41)) %>% # number of measurements
  mutate(Animal_ID = rep(juv_morphs$Animal_ID, each=nrow(juv_best_width_combos))) %>% rename(
    model = 'iteration')# add the IDs of the models

# just group by iteration and number of widths
candidate_models <- bind_cols(candidate_models, select(model_and_nwidths_df, c(model, n_widths))) 



colnames(candidate_models)[1:18] <- 1:18

candidate_models <- pivot_longer(candidate_models, cols = `1`:`18`, names_to = 'segment') %>% mutate(segment=as.numeric(segment)) %>% rename(
  
  candidate_segment_vol = 'value'
)

candidate_models <-  left_join(candidate_models, full_model_data, by = c('Animal_ID', 'segment'))


candidate_models <- candidate_models %>% group_by(Animal_ID, model) %>% 
  mutate(segment_error = candidate_segment_vol - true_segment_volume,
         prop_seg_error = abs(segment_error/true_body_volume)) # prop is absolute

# add up error
juv_cum_seg_error <- candidate_models %>%
  group_by(Animal_ID, model) %>%
  summarise(total_prop_error=sum(prop_seg_error))


juv_accepted_models <- juv_cum_seg_error %>% group_by(model) %>% filter(quantile(total_prop_error, 0.95) < 0.05)


juv_accepted_models$rep_class <- 'juvenile'



write_csv(juv_accepted_models, 'data_minimized/L5/20220426_optimal-juv-models.csv')