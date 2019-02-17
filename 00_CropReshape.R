##%######################################################%##
#                                                          #
####                  Garden Planning                   ####
#                                                          #
##%######################################################%##
# Purpose: Reshape garden planting calender for querying and plotting

library(tidyverse)
library(readxl)


datapath <- "Data"

excel_sheets(file.path(datapath, "CropCalendar.xlsx"))

df <- read_excel(file.path(datapath, "CropCalendar.xlsx"), sheet = "Raw_plantdates")
names(df)

df_long <- 
  df %>% 
  gather(key = date, value = action, -c(county:Crop)) %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>% 
  filter(!is.na(action)) %>% 
  group_by(Crop) %>% 
  mutate(start_min = min(date)) %>% 
  ungroup() %>% 
  mutate(crop_sort = fct_reorder(Crop, start_min, .desc = TRUE)) 


df_long %>% 
  ggplot(aes(x = date, y = crop_sort)) +
  geom_tile(aes(fill = action), colour = "white") 
