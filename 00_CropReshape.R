##%######################################################%##
#                                                          #
####                  Garden Planning                   ####
#                                                          #
##%######################################################%##
# Purpose: Reshape garden planting calender for querying and plotting

library(tidyverse)
library(readxl)
library(glitr)

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
  mutate(crop_sort = fct_reorder(Crop, start_min, .desc = TRUE),
         action = ifelse(action == "p", "P", action)) 


lims <- as.Date(strptime(c("2019-03-01", "2019-11-1"), 
                            format = "%Y-%m-%d"))

df_long %>% 
  ggplot(aes(x = date, y = crop_sort)) +
  geom_tile(aes(fill = action), colour = "white") +
  theme_minimal() + 
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week", expand = c(0.025, 0.025)) +
  si_style_xline() +
  theme(axis.text.x = element_text(angle = -90)) +
  scale_fill_manual(values = c(old_rose, scooter, moody_blue)) +
  labs(x = NULL, y = NULL)



# Order list
order <- c("spinach", "peas", "carrots", "cabbage", "beets", "lettuce", 
           "beans pole", "tomatoes", "squash", "peppers",
           "cucumbers")

herbs <- c("basil", "sage", "oregano", "chives")
