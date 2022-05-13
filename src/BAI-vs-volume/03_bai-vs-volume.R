
#### this file standardized dorsal body area and body volume for 1 whale with poor body condition and another with high body condition. It then compares the difference in the respective standardized body condition metrics between the two whales, incorporating the standard deviation calculated from Bierlich et al. 2021. #################


library(tidyverse)



# lets only address volume betweeen 20-85% TL
vols <- read_csv('data_minimized/L2/20220511_skinny-fat-whale-comp_vol.csv') %>% 
  select(-c(`0`, `2`, `4`, `6`, `34`))

# transform data steps
vols <- pivot_longer(vols, cols = `8`:`32`, names_to = 'segment')

vols$segment <- (as.numeric(vols$segment)+2)/2

# calculate total volume
vols <-  vols %>% group_by(Animal_ID, rep_class) %>% mutate(total_vol = sum(value))



vols <- vols %>% 
  mutate(Animal_ID = ifelse(Animal_ID=='TL0026', 'TL0026_enlarged', Animal_ID)) %>%
  mutate(Animal_ID = factor(Animal_ID))  #%>% filter(error_method=='no_error')

# assign factor levels correctly...
levels(vols$Animal_ID) <- c('Poor BC', 'High BC')



# standardize to TL; TL = 13.830
# normalize BCI to mean value
vol_smry <- vols %>% group_by(Animal_ID) %>% summarise(seg_sum = sum(value)) %>%
  mutate(TL = 13.830) %>%
  mutate(BCI = 100*seg_sum/(0.65*TL)^3) %>%
  ungroup() %>%
  mutate(normalized_BCI = BCI/mean(BCI))





#### SA stuff

# lets only address SA betweeen 20-85% TL
SA <- read_csv('data_minimized/L2/20220511_skinny-fat-whale-comp_SA.csv') %>% select(-c(`1`, `3`, `5`, `7`, `35`))

SA <- pivot_longer(SA, cols = `9`:`33`, names_to = 'segment')

SA$segment <- (as.numeric(SA$segment)+1)/2

# calculate total volume
SA <-  SA %>% group_by(Animal_ID, rep_class) %>% mutate(total_vol = sum(value))


SA <- SA %>% 
  mutate(Animal_ID = ifelse(Animal_ID=='TL0026', 'TL0026_enlarged', Animal_ID)) %>%
  mutate(Animal_ID = factor(Animal_ID))  #%>% filter(error_method=='no_error')

# assign factor levels correctly...
levels(SA$Animal_ID) <- c('Poor BC', 'High BC')



# standardize to TL; TL = 13.830
# normalize BCI to mean value
SA_smry <- SA %>% group_by(Animal_ID) %>% summarise(seg_sum = sum(value)) %>%
  mutate(TL = 13.830) %>%
  mutate(BCI = 100*seg_sum/(0.65*TL)^3) %>%
  ungroup() %>%
  mutate(normalized_BCI = BCI/mean(BCI))






###### calculate sds... percents can be done at any step of the way

bierlich_vol_cv2 <- vol_smry %>% 
  select(c(Animal_ID, normalized_BCI)) %>% 
  #mutate(seg_sum_std = seg_sum/mean(seg_sum)) %>%
  mutate(std_dev = (0.062*normalized_BCI)) %>%
  mutate(method='Volume Metric \n')

bierlich_sa_cv2 <- SA_smry %>% 
  select(c(Animal_ID, normalized_BCI)) %>% 
  #mutate(seg_sum_std = seg_sum/mean(seg_sum)) %>%
  mutate(std_dev = (0.0131*normalized_BCI)) %>%
  mutate(method='Surface Area Metric \n')

# combined the datasets again
bier_vol_sa <- bind_rows(bierlich_vol_cv2, bierlich_sa_cv2)  

# make the lower and upper bounds
plot_df <- bier_vol_sa %>% mutate(ymin = normalized_BCI - std_dev,
                                  ymax = normalized_BCI + std_dev,
                                  group_var = factor(paste(method, Animal_ID)))


# prints the output
plot_df

