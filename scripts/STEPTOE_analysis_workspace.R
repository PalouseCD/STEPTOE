# STEPTOE ANALYSIS WORKSPACE/ GERRIT BASS/ FALL 2023
# WORK SPACE FOR GENERAL ANALYSIS OF STEPTOE DATA FROM THE COMPILED WISKI DATA

#LIBRARIES
library(tidyverse)

steptoe_data <- read_csv("outputs/STEPTOE_compiled.csv")


steptoe_Q_by_month <- steptoe_data %>% 
  mutate(Month = month(datetime, label = TRUE)) %>% 
  filter(Param == "Q") %>%
  group_by(Month) %>% 
  summarise(Average = mean(Value))
