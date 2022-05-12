
library(tidyverse)



# lets only address volume betweeen 20-85% TL
vols <- read_csv('data_minimized/L2/20220511_skinny-fat-whale-comp_vol.csv') %>% 
  select(-c(`0`, `2`, `4`, `6`, `34`))

# transform data steps
vols <- pivot_longer(vols, cols = `8`:`32`, names_to = 'segment')

vols$segment <- (as.numeric(vols$segment)+2)/2

# calculate total volume
vols <-  vols %>% group_by(Animal_ID, rep_class) %>% mutate(total_vol = sum(value))

# we only want the fattest whale
#vols <- filter(vols, ID %in% c('TL0032'))


# load the data for the skinny whale (TL0026), when the whale has been transformed to the same length as TL0032
#vols2 <- read_csv('data/L2/0117_vol_error_comp2.csv') %>% select(-c(`0`, `2`, `4`, `6`, `34`))

# transform the data
#vols2 <- pivot_longer(vols2, cols = `8`:`32`, names_to = 'segment')

#vols2$segment <- (as.numeric(vols2$segment)+2)/2

#vols2 <-  vols2 %>% group_by(ID, rep_class, error_method) %>% mutate(total_vol = sum(value))

# make a new name for the 'skinny' whale stretched to the same length as TL0032
#vols2$ID <- 'TL0026_enlarged'

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

# we only want the fattest whale
#vols <- filter(vols, ID %in% c('TL0032'))


# load the data for the skinny whale (TL0026), when the whale has been transformed to the same length as TL0032
#vols2 <- read_csv('data/L2/0117_vol_error_comp2.csv') %>% select(-c(`0`, `2`, `4`, `6`, `34`))

# transform the data
#vols2 <- pivot_longer(vols2, cols = `8`:`32`, names_to = 'segment')

#vols2$segment <- (as.numeric(vols2$segment)+2)/2

#vols2 <-  vols2 %>% group_by(ID, rep_class, error_method) %>% mutate(total_vol = sum(value))

# make a new name for the 'skinny' whale stretched to the same length as TL0032
#vols2$ID <- 'TL0026_enlarged'

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

# make a plot

plot_df <- bier_vol_sa %>% mutate(ymin = normalized_BCI - std_dev,
                                  ymax = normalized_BCI + std_dev,
                                  group_var = factor(paste(method, Animal_ID)))

plot_df$group_var <- factor(plot_df$group_var, levels = c('Surface Area Metric \n Poor BC', 
                                                          'Surface Area Metric \n High BC',
                                                          'Volume Metric \n Poor BC',
                                                          'Volume Metric \n High BC'))


library(scales)

show_col(viridis_pal(alpha=1, option = 'E')(10))

my_cols <- viridis_pal(alpha=1, option = 'C')(10)[c(6,7)]

tiff("figs/Fig-S1.tiff", units="in", width=6.5, height=4.5, res=300)
ggplot(plot_df, aes(group_var, normalized_BCI, fill=method)) + 
  geom_bar(stat='identity', width = 0.75, position = position_dodge(width = 1)) + 
  geom_point() +
  theme_bw() + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, margin=margin(r=8)),
        axis.text.x = element_text(size=10, margin=margin(t=8)),
        axis.text.y = element_text(size=12, margin=margin(r=0))) +
  scale_fill_manual(values = my_cols) +
  ylab('Normalized BCI')
dev.off()



# make a new column with normalized_BCI +/- std_dev
bier_vol_sa <- bier_vol_sa  %>%
  mutate(combined_vals = paste(round(normalized_BCI,3), " ", round(std_dev,3))) 


bier_vol_sa %>% group_by(method) %>%
  mutate(normalized_BCI_diff = max(normalized_BCI) - min(normalized_BCI))
# calculate difference in normalized_BCI for each method
# calculated combined std_dev
# paste them together and round correctly
# transform into table format
bier_vol_sa <- bier_vol_sa %>% group_by(method) %>%
  mutate(normalized_BCI_diff = max(normalized_BCI) - min(normalized_BCI),
         combined_std_dev = sqrt(std_dev[1]^2 + std_dev[2]^2)) %>%
  mutate(combined_diff_vals = paste(sprintf(round(normalized_BCI_diff,3), fmt = '%#.3f'), " ", sprintf(round(combined_std_dev,3), fmt = '%#.3f'))) %>%
  select(Animal_ID, method, combined_vals, combined_diff_vals) %>% pivot_wider(names_from = Animal_ID, values_from = c(combined_vals))


df2 <- bier_vol_sa[,c(1,3,4,2)]

df2$method <- c('normalized BVI', 'normalized BAI')

library(flextable)

ft1 <- flextable(data=df2) %>% set_header_labels(method = "Body Condition Metric", combined_diff_vals = "Difference in Body Condition", skinny = "Poor Body Condition", fat = 'High Body Condition') %>%
  theme_booktabs()

autofit(ft1)


