#Figure 2 -  phytoplankton groups

##Libraries
library(tidyverse)
library(lubridate)
library(here)
library(viridis)
library(patchwork)
library(scales)
library(grid)

##Data

groups2018_gtot <- read_csv(here("data/processed_data/", "BP_GroupsBiomass_2018_KP.csv"))  

groups2019_gtot <- read_csv(here("data/processed_data", "BP_GroupsBiomass_2019_KP.csv"))

#Fig 2A

abund18 <- ggplot(groups2018_gtot, aes(DOY, total_biomass, fill = fct_reorder(Group, biomass_prop, .desc = TRUE))) + 
  annotate("rect", xmin = 164, xmax = 197, ymin = 0, ymax = 23000, alpha = .25, fill = "lavenderblush4") +
  annotate("rect", xmin = 197, xmax = 220, ymin = 0, ymax = 23000, alpha = .6, fill = "lavenderblush4") + 
  ##annotate("rect", xmin = 220, xmax = 221, ymin = 0, ymax = 23000, alpha = .25, fill = "lavenderblush4") + 
  annotate("rect", xmin = 220, xmax = 260,ymin = 0, ymax = 23000, alpha = .6, fill = "lavenderblush4" ) + 
  geom_area(colour = "grey") +
  scale_fill_manual(values = c(Cyanophyte = "#180F3EFF", Chlorophyte = "#451077FF", Chrysophyte = "#9F2F7FFF", 
                               Dinoflagellate = "#FD9567FF", Cryptophyte = "#CD4071FF",  Diatom = "#721F81FF",
                               Euglenophyte = "#FCFDBFFF"))+
  scale_x_continuous(breaks = seq(150, 270, 10), 
                     limits = c(149, 270), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0, 23000, 5000),
                     limits = c(0, 23000), label = label_number_si(), expand = c(0,0))+
  xlab("Day of Year 2018") + ylab(expression(bold("Total Biomass mg/m"^3)))+ 
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 11)) +
  theme(axis.text.y = element_text(face = "bold", size = 11))+
  theme(axis.title.x = element_text(face = "bold", size =11))+
  theme(axis.title.y = element_text(face = "bold", size = 11)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "none") +
  annotate("segment", x = 149.5, xend = 163.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90,  length = unit(.2,"cm"))) +
  annotate("text", x = 156, y = 22300, label = "Grp. A",
           fontface = "bold", colour = "black", size = 4)+
  annotate("segment", x = 164.5, xend = 196.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 180, y = 22300, label = "Grp. B",
           fontface = "bold", colour = "black", size = 4)+
  annotate("segment", x = 197.5, xend = 259.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 229, y = 22300, label = "Grp. C",
           fontface = "bold", colour = "black", size = 4)

#Fig 2B

abund19 <-  ggplot(groups2019_gtot, aes(DOY, total_biomass, fill = fct_reorder(Group, biomass_prop, .desc = TRUE))) + 
  annotate("rect", xmin = 186, xmax = 242, ymin = 0, ymax = 23000, alpha = .25, fill = "lavenderblush4") +
  annotate("rect", xmin = 242, xmax = 276,ymin = 0, ymax = 23000, alpha = .56, fill = "lavenderblush4" ) + 
  geom_area(colour = "grey") +
  scale_fill_manual(values = c(Cyanophyte = "#180F3EFF", Chlorophyte = "#451077FF", Chrysophyte = "#9F2F7FFF", 
                               Dinoflagellate = "#FD9567FF", Cryptophyte = "#CD4071FF",  Diatom = "#721F81FF",
                               Euglenophyte = "#FCFDBFFF"))+
  scale_x_continuous(breaks = seq(150, 275, 10), 
                     limits = c(149, 276), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0, 23000, 5000),
                     limits = c(0, 23000), label = label_number_si(), expand = c(0,0))+
  xlab("Day of Year 2019") + ylab(expression(bold("Total Biomass mg/m"^3))) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 11)) +
  theme(axis.text.y = element_text(face = "bold", size = 11))+
  theme(axis.title.x = element_text(face = "bold", size =11))+
  theme(axis.title.y = element_text(face = "bold", size = 11)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom",
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  annotate("segment", x = 149.5, xend = 185.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90,  length = unit(.2,"cm"))) +
  annotate("text", x = 166, y = 22300, label = "Grp. A",
           fontface = "bold", colour = "black", size = 4)+
  annotate("segment", x = 186.5, xend = 241.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 212, y = 22300, label = "Grp. B",
           fontface = "bold", colour = "black", size = 4)+
  annotate("segment", x = 242.5, xend = 275.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 259, y = 22300, label = "Grp. C",
           fontface = "bold", colour = "black", size = 4) +
  labs(fill = "Taxonomic Group", element_text(face = "bold"))

#Figure layout

Fig2 <- abund18/abund19 + plot_annotation(tag_levels = 'a', tag_suffix = ')')&
  theme(plot.tag = element_text(face = "bold", size = 12)) 

