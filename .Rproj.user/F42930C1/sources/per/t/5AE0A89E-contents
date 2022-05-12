
library(tidyverse)


Dorso_lateral_data <- read_table2("data_minimized/L0/Dorso.lateral.data.2017.2020.2021.txt")


colnames(Dorso_lateral_data) <- c('ID', 'rep_class', 'location', paste('w', as.character(seq(0.05, 0.95, 0.05)), sep = ''), paste('h', as.character(seq(0.05, 0.95, 0.05)), sep = ''), 'drslname', 'latname')

#Dorso_lateral_data <- Dorso_lateral_data[,1:40]

HW_ratios <- data.frame(Dorso_lateral_data[,23:41]/Dorso_lateral_data[,4:22]) 

colnames(HW_ratios) <- as.character(seq(0.05, 0.95, 0.05))

HW_ratios <- cbind(Dorso_lateral_data[, 1:3], HW_ratios)


# exclude outlier, remove quotation marks
HW_ratios <- HW_ratios %>% mutate(rep_class = gsub('"', '', rep_class),
                                  ID = gsub('"', '', ID),
                                  location = gsub('"', '', location)) %>%
  filter(ID != '2017.06.15.60.01b')



long_df <- HW_ratios %>% pivot_longer('0.05':'0.95', names_to= 'hw_int', values_to='hw_ratios')

mean_df <- filter(long_df, rep_class %in% c('Adult', 'Juvenile')) %>% group_by(rep_class, hw_int) %>% summarise(avg_hw_ratio=mean(hw_ratios))

# plotting the HW ratios
ggplot(filter(long_df, rep_class %in% c('Adult', 'Juvenile')), aes(x=as.factor(hw_int), y=hw_ratios, color=rep_class)) +
  geom_line(aes(group=ID)) +
  geom_line(data=mean_df, aes(hw_int, avg_hw_ratio, group=rep_class), size=2) +
  #geom_jitter(width=0.1) + 
  #geom_boxplot() +
  theme_bw() + 
  theme(legend.position = c(0.9, 0.9))


### TL not available, only relative widths and heights
### lets just say all the whales are 12 m in  length. recalc widths and heights:

DLD_fake_meas <- Dorso_lateral_data %>% mutate(across(w0.05:h0.95, .fns = ~ .x*12)) %>% mutate(rep_class = gsub('"', '', rep_class),                                          ID = gsub('"', '', ID),                                                        location = gsub('"', '', location)) %>%
  filter(rep_class %in% c('Adult', 'Juvenile'), ID != '2017.06.15.60.01b') %>% 
  select(-location) %>%
  mutate(TL=12)

# use these data to scale the base model

# use the mean HW data from these values to scale the base model


# ID, rep_class, TL, widths, heights

DLD_fake_meas <- DLD_fake_meas[,c(1,2,41, 3:21, 22:40)]


blender_ready_hw <- long_df %>% filter(rep_class %in% c('Adult', 'Juvenile') & hw_int < 0.9) %>% group_by(rep_class, hw_int) %>% summarise(avg_hw_ratio=round(mean(hw_ratios),3)) %>% pivot_wider(names_from = hw_int, values_from = avg_hw_ratio) 

better_table <- long_df %>% filter(rep_class %in% c('Adult', 'Juvenile') & hw_int < 0.9) %>% group_by(rep_class, hw_int) %>% summarise(avg_hw_ratio=round(mean(hw_ratios),3)) %>% mutate(hw_int=as.numeric(hw_int)*100)

better_table <- cbind(better_table[1:17,2:3], better_table[18:34,3])

colnames(better_table)[2:3] <- c('mean_adult_hw_ratios', 'mean_juv_hw_ratios')

library(flextable)

ft3 <- flextable(data=better_table) %>% set_header_labels(hw_int = "Percent Total Length", mean_adult_hw_ratios = "Mean Adult HW ratios", mean_juv_hw_ratios = "Mean Juv. HW ratios") %>% 
  theme_booktabs() %>%
  align(j=1, align = 'center', part = 'body') %>%
  align(j=2, align = 'center', part = 'body') %>%
  align(j=3, align = 'center', part = 'body')

autofit(ft3)




# generate table S2
save_as_docx(autofit(ft3), path = 'figs/table_S2.docx')

# write height data to csv for height vs HW scaling
write_csv(DLD_fake_meas, file='data_minimized/L1/0508_new-height-data.csv')

# write HW ratios to csv for above comparison and all other scalings
write_csv(blender_ready_hw, file='data_minimized/L1/20220508_new-HW-data.csv')
