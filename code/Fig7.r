# Fig 7 - Biomass and Chl-a

#libraries
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)
library(scales)

# data

all_data <- read_csv(here("data/processed_data", "Nutrients_Biomass_KP.csv"))

##theme
 my_theme <-   theme_classic()+
   theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 10)) +
   theme(axis.text.y = element_text(face = "bold", size = 10))+
   theme(axis.title.x = element_text(face = "bold", size =10))+
   theme(axis.title.y = element_text(face = "bold", size = 10)) + 
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text(face = "bold", size = 10, hjust = 0.1),
         legend.text = element_text(face = "bold"),
         legend.title = element_text(face = "bold"))
 
biomass_plot_p <- ggplot(all_data, aes(DOY, Tot_Biomass)) +
  geom_area(fill = "grey", colour = "black") + geom_area(aes(DOY, Planktothrix), fill= "#440154FF", alpha = 0.4) +
  facet_wrap(~Year)+
  xlab("") + ylab(expression(bold("Biomass mg/m"^3)))+ 
  scale_y_continuous(breaks=seq(0, 20000, 5000),
                     limits = c(0, 22000), label = label_number_si(), expand = c(0,0))+
  scale_x_continuous(breaks = seq(150, 275, 15), limits = c(149, 276), expand = c(0,0))+
  my_theme +
   theme(axis.text.x = element_blank()) +
   theme(axis.ticks.x = element_blank())
 
 
 chla_plot <- ggplot(all_data, aes(DOY, Chla)) +
   geom_area(fill = "grey", colour = "black") +
   facet_wrap(~Year) +
   xlab("Day of Year") + ylab("Chlorophyll-a µg/L")+ 
   scale_y_continuous(limits = c(0, 130), breaks = seq(0, 125, 25), expand = c(0,0))+
   scale_x_continuous(breaks = seq(150, 275, 15), limits = c(149, 276), expand = c(0,0))+
   my_theme

# 4 panel 
biomass_plot_p / chla_plot + plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(face = "bold", size = 12))




