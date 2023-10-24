# STEPTOE GRAPHS AND STATS FOR BRAD 2/14/22
# GERRIT BASS

library(tidyverse)
library(lubridate)

# Water temp ----------------------------------------------------------------


WT_data <- read.csv("data/STC00.70_WT.csv", skip = 15, check.names = FALSE) %>% 
  mutate(datetime = mdy_hms(paste(Date, Time))) %>%
  dplyr::rename(WT = as.numeric(3)) %>%
  mutate(WT = as.numeric(WT)) %>% # needed to specify the rename command from dplyr to work right
  select(datetime,WT)


ggplot(data = WT_data,aes(x = datetime, y = WT)) +
  geom_line(color = 'blue')+
  geom_hline(yintercept = 17.5, color = "red")

WT_summer <- WT_data %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  filter (month == 6 | month == 7 | month == 8 | month == 9) %>% 
  drop_na()

WT_summer %>% 
  group_by(year) %>% 
  summarise(mean = mean(WT))

#Streamflow -----------------------------------------------------------------

Q_data <- read.csv("data/STC00.70_Q.csv", skip = 15, check.names = FALSE) %>% 
  mutate(datetime = mdy_hms(paste(Date, Time))) %>%
  dplyr::rename(Q =3) %>%
  mutate(Q = as.numeric(Q)) %>% # needed to specify the rename command from dplyr to work right
  select(datetime,Q)

March_April_flow_mean <- Q_data %>% 
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  filter (month == 3 | month == 4) %>%
  drop_na() %>% 
  group_by(year) %>% 
  summarise(mean = mean(Q))

Q_summer <- Q_data %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  filter (month == 6 | month == 7 | month == 8 | month == 9) %>% 
  drop_na()

Q_summer %>% 
  group_by(year) %>% 
  summarise(mean = mean(Q))


# Join datasets and save -------------------------------------------------

QandT <- left_join(Q_data,WT_data,by = "datetime") %>% 
  rename("Q (cfs)" = Q,
         "WT (degC)" = WT)

write.csv(QandT,"R:\\_04_Project_Data\\R\\STEPTOE\\outputs\\Steptoe_Q_and_T.csv")
