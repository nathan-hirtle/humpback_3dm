########## this file generates table S1 ######################


library(tidyverse)


Dorso_lateral_data <- read_table("data_minimized/L0/Dorso.lateral.data.2017.2020.2021.txt")

colnames(Dorso_lateral_data) <- c('ID', 'rep_class', 'location', paste('w', as.character(seq(0.05, 0.95, 0.05)), sep = ''), paste('h', as.character(seq(0.05, 0.95, 0.05)), sep = ''), 'drslname', 'latname')


# assumed all individuals were 12 m long
# remove quotation marks
# remove outlier
data_in_analysis <- Dorso_lateral_data %>% mutate(across(w0.05:h0.95, .fns = ~ .x*12)) %>% 
  mutate(rep_class = gsub('"', '', rep_class),
         ID = gsub('"', '', ID),
         location = gsub('"', '', location)) %>%
  filter(rep_class %in% c('Adult', 'Juvenile') & ID != '2017.06.15.60.01b') %>% 
  mutate(TL=12)

# get dates
data_in_analysis <- data_in_analysis %>% mutate(
  
  date = anytime::anydate(substr(ID, 1, 10)),
  month = lubridate::month(date),
  year = lubridate::year(date),
  dataset = 'Australia'
)

# how do we want to present this data?
# one row for each individual with dates and location?

table_df <- data_in_analysis %>% select(c('ID', 'dataset', 'location', 'rep_class', 'date')) %>% rename(Animal_ID = "ID")

library(flextable)

# ft1 <- flextable(data=table_df) %>% set_header_labels(dataset = 'Dataset', location = 'Location', rep_class = 'Repr. Class', Animal_ID = 'Animal ID', date = 'Date') %>% 
#   #theme_booktabs() %>%
#   align(j=1, align = 'left', part = 'body') %>%
#   align(j=2, align = 'left', part = 'body') %>%
#   align(j=3, align = 'center', part = 'body') %>%
#   align(j=4, align = 'center', part = 'body')
# 
# autofit(ft1)

# read in NY data
NY_data <- read_csv('data_minimized/L0/0215_morphs_58-whales.csv')

`%ni%` <- Negate(`%in%`)

# extract dates from Image column based on formattting
NY_data <- NY_data %>% mutate(
  
  date = case_when(startsWith(Image, 'file') ~ substr(Image, 39, 46),
                   startsWith(Image, 'UAV_L0') ~ substr(Image, 13, 20),
                   startsWith(Image, 'final_formeas_file____Volumes') ~ substr(Image, 65, 72),
                   startsWith(Image, 'final_formeas_file____Users_EIH_Desktop') ~ '2021-10-07',
                   startsWith(Image, 'final_formeas_file____Users_EIH_Documents') ~ '2021-10-15')) %>% 
  
  mutate(
    
    date = anytime::anydate(date),
    dataset = 'New York',
    location = '--',
    Animal_ID = ifelse(Animal_ID==lag(Animal_ID, default = 'nada'), paste0(Animal_ID, '_1'), Animal_ID),
    Animal_ID = ifelse(Animal_ID=='TLSCAR', 'TL0085', Animal_ID),
    rep_class = stringr::str_to_title(rep_class)
    
  ) %>% filter(Animal_ID %ni% c('TL0092_1', 'TL0093_1')) # remove the 2 repeats



NY_table_df <-  NY_data %>% select(c('Animal_ID', 'dataset', 'location', 'rep_class', 'date'))


table_df <- bind_rows(table_df, NY_table_df)

ft1 <- flextable(data=table_df) %>% set_header_labels(dataset = 'Dataset', location = 'Location', rep_class = 'Repr. Class', Animal_ID = 'Animal ID', date = 'Date') %>% 
  #theme_booktabs() %>%
  align(j=1, align = 'left', part = 'body') %>%
  align(j=2, align = 'center', part = 'body') %>%
  align(j=3, align = 'center', part = 'body') %>%
  align(j=4, align = 'center', part = 'body')

autofit(ft1)

save_as_docx(autofit(ft1), path = 'figs/table-S1.docx')

