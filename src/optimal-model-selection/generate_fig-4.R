
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

# make indices of model combinations 
n <- 17
l <- rep(list(0:1), n)

test_df <- expand.grid(l)

test_df$iteration <- 1:nrow(test_df)

width_presence <- test_df %>% mutate(n_widths = rowSums(across(Var1:Var17))) 




cse_by_nwidths <- left_join(width_presence, cum_seg_error, by='iteration')



# convert to proportional error

cse_by_nwidths <- cse_by_nwidths %>% mutate(prop_error=cs_error/sum(full_model))

cse_by_nwidths$rep_class <- 'Adult'

adult_cse <- cse_by_nwidths


########### JUVENILE DATA #########################

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

#width_presence <- read.csv('data/L0/width_presence_n_widths.csv')  

cse_by_nwidths <- left_join(width_presence, cum_seg_error, by='iteration')

cse_by_nwidths$rep_class <- 'Juvenile'

cse_by_nwidths <- cse_by_nwidths %>% mutate(prop_error=cs_error/sum(full_model))

juv_cse <- cse_by_nwidths

both_rep_cse <- rbind(adult_cse, juv_cse)

library(stringr)
both_rep_cse$rep_class <- str_to_title(both_rep_cse$rep_class)

# below df is for experimenting with aesthetic options
#small_both_rep_cse <- sample_n(both_rep_cse, 13000)
tiff("figs/Fig_4.tiff", units="in", width=6.5, height=4.31, res=300)
ggplot(filter(both_rep_cse, n_widths > 0), aes(as.factor(n_widths), y=prop_error*100, color=as.factor(n_widths))) +
  geom_jitter(size=0.1) +
  theme_bw() +
  geom_hline(yintercept = 5, linetype='dashed') +
  coord_cartesian(ylim=c(0,15)) +
  scale_x_discrete('Number of Widths Included in Model') +
  scale_y_continuous(expression(paste('Volume Error (% Total Volume)'))) +
  facet_wrap(~rep_class) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size=16, margin=margin(t=8)),
        axis.title.y = element_text(size=16, margin=margin(r=10)),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size=12, margin = margin(t=0.1, b=0.1))) + scale_color_viridis_d(option="C", direction = 1)
dev.off()