##Supplemental Figure 2

#libraries
library(here)
library(tidyverse)
library(patchwork)

#data
all_data <- read_csv(here("data/clean_data", "Nutrients_Biomass_BP_clean.csv")) 

all_data <- all_data %>% mutate("D_Prop" = (Dolichospermum/Tot_Biomass)*100,
                                "P_Prop" = (Planktothrix/Tot_Biomass)*100)

##theme with font size 14
my_theme <-   theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 14)) +
  theme(axis.text.y = element_text(face = "bold", size = 14))+
  theme(axis.title.x = element_text(face = "bold", size =14))+
  theme(axis.title.y = element_text(face = "bold", size = 14)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14, hjust = 0.1),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

#Dolichospermum 
D <- ggplot(all_data, aes(Dolichospermum, Chla, size = D_Prop))+
  geom_point(shape = 21, colour = "black", fill = "green") +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, colour = "black")+
  xlab(expression(bold("Dolichsopermum Biomass mg/m"^3))) + ylab("Chlorophyll-a µg/L") +
  facet_wrap(~Year)+
  labs(size = "Dolichospermum\nBiomass (%)")+
  my_theme

#Planktothrix
P <- ggplot(all_data, aes(Planktothrix, Chla, size = P_Prop)) +
  geom_point(shape = 21, colour = "black", fill = "purple") +
  #geom_smooth(method = "lm", se = FALSE, size = 0.5, colour = "black") +
  xlab(expression(bold("Planktothrix Biomass mg/m"^3))) + ylab("Chlorophyll-a µg/L") +
  facet_wrap(~Year)+
  labs(size = "Planktothrix\nBiomass (%)")+
  my_theme

#4 panel 
D / P+ plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(plot.tag = element_text(face = "bold", size = 16))

##spearman correlations
all_2018 <- all_data %>% filter(Year == 2018)
plank_corr <- cor.test(x = all_2018$Planktothrix, y = all_2018$Chla, method = 'spearman')
doli_corr <- cor.test(x = all_2018$Dolichospermum, y = all_2018$Chla, method = 'spearman')

all_2019 <- all_data %>% filter(Year == 2019)
plank_corr <- cor.test(x = all_2019$Planktothrix, y = all_2019$Chla, method = 'spearman')
doli_corr <- cor.test(x = all_2019$Dolichospermum, y = all_2019$Chla, method = 'spearman')
