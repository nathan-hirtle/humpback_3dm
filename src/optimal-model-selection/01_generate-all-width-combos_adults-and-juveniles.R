


library(tidyverse)

### load in data first

`%ni%` <- Negate(`%in%`)


width_data <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv') %>% mutate(
  
  Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
  Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID)
  
) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1')) # remove the 2 repeats




all_juvie_data <- filter(width_data, rep_class=='juvenile')

#write_csv(all_juvie_data, 'data/L1/0120_all-juvie-data.csv')



# mean length?

mean(all_juvie_data$WL)

avg_juv_morphs <- all_juvie_data %>% summarise(across(.cols=`5`:`85`, .fns = mean))

n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)


juvie_combos <- as.matrix(test_df)

#ifelse(as.numeric(test_df[20,])==1, avg_juv_morphs[1,], NA)

for (i in 1:nrow(test_df)) {
  
  tmp <- as.numeric(ifelse(as.numeric(test_df[i,])==1, avg_juv_morphs[1,], NA))
  
  juvie_combos[i,] <- tmp
  
  
}

############## end juvenile processing ####################



################# adult processing ######################

# subset the data
adult_data <- width_data %>%
  filter(rep_class=='adult')

mean(adult_data$WL)

avg_adult_morphs <- adult_data %>% summarise(across(.cols=`5`:`85`, .fns = mean))

n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)


adult_combos <- as.matrix(test_df)

#ifelse(as.numeric(test_df[20,])==1, avg_juv_morphs[1,], NA)

for (i in 1:nrow(test_df)) {
  
  tmp <- as.numeric(ifelse(as.numeric(test_df[i,])==1, avg_adult_morphs[1,], NA))
  
  adult_combos[i,] <- tmp
  
  
}

############### end adult processing #####################



# write out all 131072 combos for mean JUVENILE measurements to csv
write_csv(as.data.frame(juvie_combos), file = 'data_minimized/L1/mean_juvenile-morph_combos.csv')

# write out all 131072 combos for mean ADULT measurements to csv
write_csv(as.data.frame(adult_combos), file = 'data_minimized/L1/mean_adult-morph_combos.csv')
