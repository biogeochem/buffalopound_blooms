# Figure S3 - Nutrient data
#libraries
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)
library(scales)

#Data
all_data <- read_csv(here("data/processed_data", "Nutrients_Biomass_BP.csv"))

long_data <- all_data %>% group_by(Year, DOY) %>%
  pivot_longer(cols = c(2:8), names_to = "parameter", values_to = "values")

#Figure
ggplot((long_data %>% filter(parameter %in% c("SRP", "NH3", "NO3"))), aes(DOY, (values*1000), colour = parameter, shape = parameter)) +
  geom_point() + geom_line() +
  scale_shape_discrete(labels = c("Ammonium N", "Nitrate N", "Soluble Reactive P")) +
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                        labels = c("Ammonium N", "Nitrate N", "Soluble Reactive P")) +
  facet_grid(vars(parameter), vars(Year)) + 
  scale_x_continuous(breaks = seq(150, 275, 10), 
                     limits = c(149, 276), expand = c(0,0))+
  xlab("Day of Year") + ylab("Concentration µg/L")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 11)) +
  theme(axis.text.y = element_text(face = "bold", size = 11))+
  theme(axis.title.x = element_text(face = "bold", size =11))+
  theme(axis.title.y = element_text(face = "bold", size = 11)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        legend.text = element_text(face = "bold"),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.placement = "outside")
  #annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  #annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

