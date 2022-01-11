#Fig S1 - Temperature from YSI 

#libraries
library(tidyverse)
library(lubridate)
library(here)

#YSI data

ysi_18 <- read_csv(here("data/raw_data", "BP_YSI_2018.csv"))

ysi_18 <- ysi_18 %>% mutate(DOY = yday(Date),
                            Year = year(Date)) %>%
  select(Year, DOY, Depth_m, Temperature, Conductivity,
         DO_sat, DO_mgL, pH)

ysi_18_means <- ysi_18 %>% group_by(Year, DOY) %>% 
  summarize_at(vars(Temperature, Conductivity,
                    DO_sat, DO_mgL, pH), list(~mean(.), ~sd(.)))

ysi_19 <- read_csv(here("data/raw_data", "BP_YSI_2019.csv")) 

## using lubridate's parser to convert date from a character format 
Date2019 <- c("03-Jun-19", "03-Jun-19", "03-Jun-19")
dmy(Date2019) 
ysi_19$Date %>% as_date(ysi_19$Date)
ysi_19 <- ysi_19 %>% mutate(Date = dmy(Date))

ysi_19 <- ysi_19 %>% mutate(DOY = yday(Date),
                            Year = year(Date)) %>%
  select(Year, DOY, Depth_m, Temperature, Conductivity,
         DO_sat, DO_mgL, pH)

ysi_19_means <- ysi_19 %>% group_by(Year, DOY) %>% 
  summarize_at(vars(Temperature, Conductivity,
                    DO_sat, DO_mgL, pH), list(~mean(.), ~sd(.)))

ysi_all <- rbind(ysi_18_means, ysi_19_means)


#Figure

ggplot(ysi_all, aes(x = DOY, y = Temperature_mean, 
                         ymin = Temperature_mean - Temperature_sd, 
                         ymax = Temperature_mean + Temperature_sd)) +
  geom_pointrange() + scale_x_continuous(breaks = seq(150, 275, 10)) +
  geom_line() + 
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0,30), expand = c(0,0)) +
  facet_wrap(~Year) + 
  xlab("Day of Year") + ylab("Temperature ⁰C")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 11)) +
  theme(axis.text.y = element_text(face = "bold", size = 11))+
  theme(axis.title.x = element_text(face = "bold", size =11))+
  theme(axis.title.y = element_text(face = "bold", size = 11)) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold", size = 11)) + 
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

