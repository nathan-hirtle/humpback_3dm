
library(tidyverse)

hw_models <- read_csv("data_minimized/L2/20220508_models_scaled_HW-ratios-and-widths.csv")

hw_models <- pivot_longer(hw_models, cols = `0`:`17`, names_to = 'segment')

hw_models$segment <- as.numeric(hw_models$segment)+1

hw_models <- hw_models %>% group_by(Animal_ID) %>%
  mutate(total_vol = sum(value))

## load height data
height_models <- read_csv("data_minimized/L2/20220508_models_scaled_heights-and-widths.csv")

height_models <- pivot_longer(height_models, cols = `0`:`17`, names_to = 'segment')

height_models$segment <- as.numeric(height_models$segment)+1

height_models <- height_models %>% group_by(Animal_ID) %>%
  mutate(total_vol = sum(value))


## calc difference between them 

both_methods <- left_join(hw_models, 
                          height_models, 
                          by = c('Animal_ID', 'segment', 'rep_class'), 
                          suffix = c('_hw_models', '_height_models')) %>%
  mutate(vol_method_diff = value_height_models - value_hw_models ) %>%
  mutate(prop_vmd = vol_method_diff/total_vol_height_models) # standardized to volume




cse <- both_methods %>% group_by(Animal_ID, rep_class) %>% summarise(total_prop_seg_error = sum(prop_vmd)) 


# mean error by rep class
output_table <- cse %>% group_by(rep_class) %>% 
  summarise(mean_error = mean(abs(total_prop_seg_error))*100, 
            std_dev = sd(abs(total_prop_seg_error))*100)

write_csv(output_table, 'data_minimized/L3/height-vs-HW-scaling_error.csv')


