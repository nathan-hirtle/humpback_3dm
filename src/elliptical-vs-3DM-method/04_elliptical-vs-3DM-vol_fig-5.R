

library(tidyverse)


elliptical_cse <- read_csv('data_minimized/L3/20220426_optimal-elliptical-models.csv')

elliptical_cse <- elliptical_cse %>% mutate(method = 'elliptical')

elliptical_cse$model <- as.character('minimal')



full_elliptical_cse <- read_csv('data_minimized/L3/20220426_full-elliptical-models.csv')

full_elliptical_cse <- full_elliptical_cse %>% mutate(
  
  method = 'elliptical')

full_elliptical_cse$model <- as.character('full')

accepted_adult_models <- read_csv('data_minimized/L5/20220426_optimal-adult-models.csv')

accepted_adult_models <- accepted_adult_models %>% mutate(
  
  method = '3DM')

accepted_adult_models$model <- as.character('minimal')

accepted_juv_models <- read_csv('data_minimized/L5/20220426_optimal-juv-models.csv') 

accepted_juv_models <-  accepted_juv_models %>% 
  
  select(-c('cs_error', 'perfect_vol')) %>%
  
  rename(total_prop_error = 'prop_error') %>%
  
  mutate(
    
    method = '3DM')

accepted_juv_models$model <- as.character('minimal')


#fake_no_error_df <- data.frame(Animal_ID = rep('fake',2), model = rep('full',2), rep_class = c('adult', 'juvenile'), total_prop_error = rep(0,2), method = rep('3DM', 2))

all_methods_cse <- bind_rows(full_elliptical_cse, elliptical_cse, accepted_adult_models, accepted_juv_models)#, fake_no_error_df)


ggplot(all_methods_cse, aes(rep_class, total_prop_error, color = method, linetype = model)) + geom_boxplot() + geom_hline(yintercept=0.05) + coord_cartesian(ylim=c(0,0.25)) + theme_bw()


#### making the 2 variables only one variable

all_methods_cse <- all_methods_cse %>% 
  mutate(model = stringr::str_to_title(model),
         method = ifelse(method=='elliptical', 'Elliptical', method)) %>%
  mutate(`model and method` = paste0(model, ',\n', method)) 

# filter outliers
smry_df <- all_methods_cse %>% group_by(Animal_ID, `model and method`, rep_class) %>% summarise(mean_error_perc = mean(total_prop_error)*100) %>% mutate(rep_class = stringr::str_to_title(rep_class)) %>% filter(mean_error_perc < 15)

# mean error for each animal:
# full model only has 1 value for each animal
# minimal models have multiple values for each animal, average used below
# two outliers on right plot at 20 & 25%
tiff("figs/20220509_3DM_vs_geometric_smry.tiff", units="in", width=6.5, height=6.5, res=300)
ggplot(smry_df, aes(`model and method`, mean_error_perc, color = `model and method`)) + 
  geom_violin() + 
  #geom_boxplot() +
  geom_jitter(height = 0) +
  geom_hline(yintercept=5, linetype='dashed') +
  theme_bw() + 
  facet_wrap(vars(rep_class)) +
  ylab('Volume Error (% Total Volume)') +
  xlab('Model and Method') +
  theme(legend.position = 'none',
        #legend.background = element_blank(),
        #legend.box.background = element_rect(),
        axis.title.x = element_text(size=16, margin=margin(t=8)),
        axis.title.y = element_text(size=16, margin=margin(r=8)),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size=12, margin=margin(t=8)),
        strip.text = element_text(size=12)) +
  #coord_cartesian(ylim=c(0,15)) +
  scale_color_viridis_d(option = 'C', end=0.7)
dev.off()

scales::show_col(viridis::viridis(n=3, option = 'C', end = 0.7))

# below is old data, but useful for final figure

