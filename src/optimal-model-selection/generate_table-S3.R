library(tidyverse)



###### for adult_best_width_combos ###
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



# make indices of model combinations 
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


# these are just the width measurement combinations
adult_best_model_widths <- read.csv('data_minimized/L3/0309_adult_minmodels.csv')


all_adults_full_models <- read_csv("data_minimized/L2/20220510_full-models.csv") %>% filter(rep_class =='adult') 

#all_adults_full_models$Animal_ID <- unique(adult_best_model_widths$Animal_ID)

all_adults_full_models$model <- as.factor('full')


# these are the actual volume estimates from the scaled models
all_adults_best_models_1 <- read_csv("data_minimized/L4/0309_best_mods_adult_1.csv")

all_adults_best_models_2 <- read_csv("data_minimized/L4/0309_best_mods_adult_2.csv")


all_adults_best_models <- rbind(all_adults_best_models_1, all_adults_best_models_2)

all_adults_best_models$Animal_ID <- adult_best_model_widths$Animal_ID

all_adults_best_models$model <- rep(adult_best_width_combos$iteration, 15)


adult_all_models <- rbind(all_adults_best_models, select(all_adults_full_models, -c('TL', 'Image', 'rep_class')))

adult_all_models_long <- pivot_longer(adult_all_models, cols = `0`:`17`, names_to = 'segment')


adult_full_models <- filter(adult_all_models_long, model=='full')


adult_seg_vol_error <- adult_all_models_long %>% group_by(Animal_ID, model) %>%   # 4539 models for each animal
  mutate(error=value-adult_full_models$value[which(adult_full_models$Animal_ID==Animal_ID)]) %>%
  mutate(prop_seg_error=error/adult_full_models$value[which(adult_full_models$Animal_ID==Animal_ID)])


adult_cum_seg_error <- adult_seg_vol_error %>% 
  group_by(Animal_ID, model) %>%
  summarise(cs_error=sum(abs(error))) # we want abs error

adult_full_models_tot_vol <- adult_full_models %>% group_by(Animal_ID) %>% summarise(perfect_vol=sum(value))

adult_cum_seg_error$perfect_vol <- rep(adult_full_models_tot_vol$perfect_vol, each=nrow(adult_best_width_combos)+1)

adult_cum_seg_error <- adult_cum_seg_error %>% mutate(prop_error=cs_error/perfect_vol)


#ggplot(cum_seg_error, aes(model, prop_error)) + geom_boxplot()

# this is too restrictive; aim for 95% instead
# adult_accepted_models <- adult_cum_seg_error %>% group_by(model) %>% filter(., max(prop_error) < 0.05 & model !='full')


# this is better
adult_accepted_models2 <- adult_cum_seg_error %>% group_by(model) %>% filter(., quantile(prop_error, 0.95) < 0.05 & model !='full')


adult_accepted_models2$dataset <- 'ours'

adult_accepted_models2$rep_class <- 'Adult'


adult_accepted_combos <- filter(width_presence, iteration %in% adult_accepted_models2$model)


############# make table for adults #######

accepted_smry <- adult_accepted_models2 %>% group_by(model) %>% summarise(avg_prop_error=mean(prop_error))

# identifying the models that were accepted
width_presence <- width_presence %>% rename(model = iteration)

width_presence$model <- as.character(width_presence$model)

models_under_5 <- inner_join(width_presence, accepted_smry, by = 'model')

adult_modunder5 <- models_under_5

foo <- adult_modunder5[,1:17] %>% summarise(n=colSums(.))
foo$width <- seq(5,85,5)

ggplot(foo, aes(as.factor(width), n)) + geom_bar(stat = 'identity')

models_under_5_sort <- models_under_5 %>%  arrange(across(Var1:Var17, desc))

for (i in 1:17) {
  colnames(models_under_5_sort)[i] <- i*5
  
}

# function to make lists of necessary widths
foo <- apply(models_under_5_sort[,1:17], 1, function(x) names(which(x > 0)))

df1 <- data.frame(model=models_under_5_sort$model, col1=rep(NA,nrow(models_under_5_sort)), col2=round(models_under_5_sort$avg_prop_error,3))

for (i in 1:nrow(models_under_5_sort)) {
  
  df1$col1[i] <- paste0(foo[,i], collapse = ', ') 
  
}

library(flextable)

ft2 <- flextable(data=df1) %>% set_header_labels(model='Model', col1='Widths in Model', col2='Mean Proportional Error')

ft2 <- width(ft2, width=1.5) %>% align(., j=1, align = 'justify', part = 'body') # bottom line is present but not always visible in viewport

set_caption(ft2, 'Five-width models which resulted in < 5% error for 95% of adults')


##### JUVENILE STUFF ##############


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


### finally making table?

accepted_smry <- juv_accepted_models %>% group_by(model) %>% summarise(avg_prop_error=mean(total_prop_error)) %>% mutate(model = as.character(model))

# identifying the models that were accepted
width_presence <- width_presence %>% rename(model = iteration)

width_presence$model <- as.character(width_presence$model)

models_under_5 <- inner_join(width_presence, accepted_smry, by = 'model')

juv_modunder5 <- models_under_5

foo <- juv_modunder5[,1:17] %>% summarise(n=colSums(.))
foo$width <- seq(5,85,5)

ggplot(foo, aes(as.factor(width), n)) + geom_bar(stat = 'identity')

models_under_5_sort <- models_under_5 %>%  arrange(across(Var1:Var17, desc))

for (i in 1:17) {
  colnames(models_under_5_sort)[i] <- i*5
  
}

bwc_2 <- juv_best_width_combos 

colnames(bwc_2)[1:17] <- seq(5, 85, 5)

which_widths <- filter(bwc_2, iteration %in% unique(juv_accepted_models$model)) %>% arrange(across(`5`:`85`, desc))


### make table prep

foo <- apply(which_widths[,1:17], 1, function(x) names(which(x > 0)))

df1 <- data.frame(model=which_widths$iteration, col1=rep(NA,nrow(which_widths)), col2=round(models_under_5_sort$avg_prop_error,3))

for (i in 1:nrow(which_widths)) {
  
  df1$col1[i] <- paste0(foo[,i], collapse = ', ') 
  
}


library(flextable)


ft3 <- flextable(data=df1) %>% set_header_labels(model='Model', col1='Widths in Model', col2= 'Mean Proportional Error')

ft3 <- width(ft3, width=2) %>% align(., j=1, align = 'justify', part = 'body') # bottom line is present but not always visible in viewport
ft3



ft2_mod <- delete_part(ft2, 'header') %>% add_header_row(., values='Adult', colwidths = 3) %>%
  add_header_row(., values=c('Model', 'Widths in Model', 'Mean Proportional \n Error')) %>%
  theme_booktabs(.) %>%
  align(., align = 'center', part = 'header') 

autofit(ft2_mod)

ft3_mod <- delete_part(ft3, 'header') %>% add_header_row(., values='Juvenile', colwidths = 3) %>%
  theme_booktabs(.) %>%
  align(., align = 'center', part = 'header')



autofit(ft3_mod)

save_as_docx(autofit(ft2_mod), path = 'figs/0202_adult-table.docx')
save_as_docx(autofit(ft3_mod), path = 'figs/0202_juv-table.docx')


