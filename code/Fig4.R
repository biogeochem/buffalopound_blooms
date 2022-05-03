#Fig 4 - toxin figure

library(tidyverse)
library(lubridate)
library(here)
library(vegan)
library(ggrepel)
library(patchwork)

##Toxins 2019 #### 
## < LODs replaced with 0's

toxs_zeros <- read_csv(here("data/processed_data/", "Toxs2019_zerosLODs.csv"))

toxs_zeros <-toxs_zeros %>% filter(Depth_m == 0.8) %>%
               mutate(DOY = yday(Date))

toxs_zeros_2 <-toxs_zeros %>% select(DOY, everything(), -Date, -Depth_m, -MC_total) %>% 
              arrange(DOY)

toxs_long <- toxs_zeros_2 %>% pivot_longer(cols = c(3:14), 
                                         names_to = "Cyanotoxin", 
                                         values_to = "Conc")

tox_names <-as_labeller(c('ANA_a'= "ANA-a", 'AP_A' = "AP-A", 'AP_B'= "AP-B",
                          'dmMC_LR'= "dmMC-LR", 'MC_HiIR' = "MC-HiIR", 'MC_LA' = "MC-LA",
                          'MC_LF' = "MC-LF", 'MC_LR'= "MC-LR", 'MC_LW'= "MC-LW",
                          'MC_LY' = "MC-LY", 'MC_RR' = "MC-RR", 'MC_YR' = "MC-YR"))

all_tox <- ggplot(toxs_long, aes(DOY, Conc, colour = Sample_Type)) +
  geom_line(size = 1) + geom_point(position = "jitter", alpha = 0.8) + 
  scale_colour_manual(values = c("black", "#878787")) + 
  facet_wrap(~Cyanotoxin, scales = "free_y", labeller = tox_names) +
  xlab("Day of Year 2019") + ylab("Concentration ng/L") + 
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275))+
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, face = "bold"),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_blank())

tot_MC <- ggplot(toxs_zeros, aes(x=DOY, y = MC_total, colour = Sample_Type)) +
  geom_line(size = 1) + geom_point(position = "jitter", alpha = 0.8) + 
  xlab("Day of Year 2019") + ylab("") + 
  scale_colour_manual(values = c("black", "#878787")) +
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275))+
  theme_classic() +
  ggtitle("Total MC")+
  theme(legend.position = "none",
        plot.title = element_text(vjust = -8, size = 11, face = "bold"),
        axis.text.x = element_text(angle = 45, face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11))
  

tox_patch <- all_tox + tot_MC + plot_layout(widths = c(3,1)) + 
             plot_annotation(title = '2019')

## Toxins 2018 ####

toxs_018 <- read_csv(here("data/processed_data/", "Toxs2018_zerosLODs.csv"))

toxs_018 <-toxs_018 %>% filter(Depth_m == 0.8) %>%
  mutate(DOY = yday(Date))

toxs_018_2 <-toxs_018 %>% select(DOY, everything(), -Date, -Depth_m, -MC_total, -ANA_a,
                                 -MC_HtyR, -dmMC_RR, -MC_LW, -CYN, -MC_HiIR,
                                 -MC_LY) %>% arrange(DOY)

toxs_018_long <- toxs_018_2 %>% pivot_longer(cols = c(3:9), 
                                           names_to = "Cyanotoxin", 
                                           values_to = "Conc")

tox_names18 <-as_labeller(c('AP_A' = "AP-A", 'AP_B'= "AP-B",
                          'dmMC_LR'= "dmMC-LR",'MC_LR'= "MC-LR",
                          'MC_RR' = "MC-RR", 'MC_YR' = "MC-YR",
                          'HANA_a' = "HANA-a"))

all_tox_18 <- ggplot(toxs_018_long, aes(DOY, Conc, colour = Sample_Type)) +
  geom_line(size = 1) + geom_point(position = "jitter", alpha = 0.8) + 
  scale_colour_manual(values = c("black", "#878787")) + 
  facet_wrap(~Cyanotoxin, scales = "free_y", labeller = tox_names18) +
  xlab("Day of Year 2018") + ylab("Concentration ng/L") + 
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275))+
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, face = "bold"),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_blank())

tot_MC_18 <- ggplot(toxs_018, aes(x=DOY, y = MC_total, colour = Sample_Type)) +
  geom_line(size = 1) + geom_point(position = "jitter", alpha = 0.8) + 
  scale_colour_manual(values = c("black", "#878787")) + 
  xlab("Day of Year 2018") + ylab("") + 
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275))+
  theme_classic() +
  ggtitle("Total MC")+
  theme(legend.position = "none",
        plot.title = element_text(vjust = -8, size = 11, face = "bold"),
        axis.text.x = element_text(angle = 45, face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11))

tox_patch18 <- all_tox_18 + tot_MC_18 + plot_layout(widths = c(3,1)) +
  plot_annotation(title = '2018')

##multi-panel figure
tox_patch18/tox_patch + plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(face = "bold")) 