

############# this file generates all possible combinations of width measurements for each reproductive class ######################




library(tidyverse)

### load in data first

juvie_data <- read_csv('src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L0/example-MorphometriX-output.csv') %>% filter(rep_class=='juvenile')



avg_juv_morphs <- juvie_data %>% summarise(across(.cols=`5`:`85`, .fns = mean))

n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)


juvie_combos <- as.matrix(test_df)



for (i in 1:nrow(test_df)) {
  
  tmp <- as.numeric(ifelse(as.numeric(test_df[i,])==1, avg_juv_morphs[1,], NA))
  
  juvie_combos[i,] <- tmp
  
  
}

############## end juvenile processing ####################

# assign mean length, rep class, and model #
juvie_combos <- data.frame(juvie_combos) %>% mutate(rep_class = 'juvenile',
                                        WL = mean(juvie_data$WL),
                                        model = 1:nrow(juvie_combos))



################# adult processing ######################

adult_data <- read_csv('src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L0/example-MorphometriX-output.csv') %>% filter(rep_class=='adult')



avg_adult_morphs <- adult_data %>% summarise(across(.cols=`5`:`85`, .fns = mean))

n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)


adult_combos <- as.matrix(test_df)



for (i in 1:nrow(test_df)) {
  
  tmp <- as.numeric(ifelse(as.numeric(test_df[i,])==1, avg_adult_morphs[1,], NA))
  
  adult_combos[i,] <- tmp
  
  
}


# assign mean length, rep class, and model #
adult_combos <- data.frame(adult_combos) %>% mutate(rep_class = 'adult',
                                                    WL = mean(adult_data$WL),
                                                    model = 1:nrow(adult_combos))

############### end adult processing #####################



# write out all 131072 combos for mean JUVENILE measurements to csv
write_csv(juvie_combos, file = 'src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L3/mean_juvenile-morph_combos.csv')

# write out all 131072 combos for mean ADULT measurements to csv
write_csv(adult_combos, file = 'src/code-and-guidelines-to-develop-and-scale-3DMs/example_data/L3/mean_adult-morph_combos.csv')
