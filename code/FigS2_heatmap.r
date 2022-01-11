#Fig S2 - Heatmap of cyano species

#libraries
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)
library(scales)

#2018 cyano data ####

phyt2018_tax <- read_csv(here("data/processed_data/", "BP_PhytoTax_2018_KP.csv"))

cyanos_18 <- phyt2018_tax %>% filter(Group %in% "Cyanophyte")

cyanos_18$Genus <- if_else(str_detect(cyanos_18$Genus, "Anabaena"),"Dolichospermum", cyanos_18$Genus)

cyanos_18 <-cyanos_18 %>% unite("Name", c(Genus, Species), sep = " ")

cyanos_18 <- cyanos_18 %>%
  select(DOY,
         Name,
         Biomass_mg_m3) %>%
  group_by(DOY, Name) %>%
  dplyr::summarize_at(vars(Biomass_mg_m3), list(total = sum)) %>%
  droplevels()

#quick pivot to fill in missing Taxa with 0's 
cyanos_18 <- cyanos_18 %>%
  pivot_wider(names_from = Name, values_from =total, values_fill = 0)

cyanos_18 <- cyanos_18 %>% pivot_longer(cols = c(2:11), names_to = "Name", values_to = "Biomass")

#2019 cyano data ####

phyt2019_tax <- read_csv(here("data/processed_data/", "BP_PhytoTax_2019_KP.csv"))

cyanos_19 <- phyt2019_tax %>% filter(Group %in% "Cyanophyte")

cyanos_19$Genus <- if_else(str_detect(cyanos_19$Genus, "Anabaena"),"Dolichospermum", cyanos_19$Genus)

cyanos_19 <-cyanos_19 %>% unite("Name", c(Genus, Species), sep = " ")

cyanos_19 <- cyanos_19 %>%
  select(DOY,
         Name,
         Biomass_mg_m3) %>%
  group_by(DOY, Name) %>%
  dplyr::summarize_at(vars(Biomass_mg_m3), list(total = sum)) %>%
  droplevels()

#quick pivot to fill in missing Taxa with 0's 
cyanos_19 <- cyanos_19 %>%
  pivot_wider(names_from = Name, values_from =total, values_fill = 0)

cyanos_19 <- cyanos_19 %>% pivot_longer(cols = c(2:9), names_to = "Name", values_to = "Biomass")

cyanos_19 %>% mutate("Biomass_fac"%in% cut(cyanos_19$Biomass,breaks=c(0,10,100,500,1000,5000,10000,max(cyanos_19$Biomass)),
                       labels=c("0","1-10","10-100","100-500","500-1000","1000-5000","5000-10000", ">10000")))



#plots ####

#2018

heat_18 <- ggplot(cyanos_18, aes(x = factor(DOY), y = Name)) +
  geom_tile(aes(fill = biom.category), color="white") +
  scale_y_discrete(limits = rev(levels(as.factor(cyanos_18$Name))), expand = c(0,0)) +
  labs(x = "Day of Year", y = NULL, fill = expression("Biomass (mg m"^3*")")) +
  scale_fill_discrete_sequential(palette = "Inferno", rev = FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 10),
        axis.text.y = element_text(face = "bold.italic", size = 10),
        axis.title.x = element_text(face = "bold", size =10),
        axis.title.y = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank())+
        theme(legend.position = "none") + 
         coord_fixed()

#2019

heat_19 <- ggplot(cyanos_19, aes(x = factor(DOY), y = Name)) +
  geom_tile(aes(fill = biom.category), color="white") +
  scale_y_discrete(limits = rev(levels(as.factor(cyanos_19$Name))), expand = c(0,0)) +
  labs(x = "Day of Year", y = NULL, fill = expression(bold("Biomass (mg m"^3*")"))) +
  scale_fill_discrete_sequential(palette = "Inferno", rev = FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 10),
        axis.text.y = element_text(face = "bold.italic", size = 10),
        axis.title.x = element_text(face = "bold", size =10),
        axis.title.y = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom") + coord_fixed()
        #legend.key.height=unit(0.4,"cm"),
        #legend.key.width=unit(0.4,"cm")) 

#patchwork heat maps
heat_18 / heat_19 + plot_annotation(tag_levels = 'A')

##Lisa's code for categories 

cyanos_18$biom.category <- ifelse(cyanos_18$Biomass == 0, "0", 
                                       ifelse(cyanos_18$Biomass > 0 & cyanos_18$Biomass <= 25, "< 25", 
                                        ifelse(cyanos_18$Biomass > 25 & cyanos_18$Biomass <= 50, "25 < 50", 
                                         ifelse(cyanos_18$Biomass > 50 & cyanos_18$Biomass <= 250, "50 < 250", 
                                          ifelse(cyanos_18$Biomass > 250 & cyanos_18$Biomass <= 500, "250 < 500",
                                           ifelse(cyanos_18$Biomass > 500 & cyanos_18$Biomass <= 1000, "500 < 1000", 
                                            ifelse(cyanos_18$Biomass > 1000 & cyanos_18$Biomass <= 2000, "1000 < 2000",
                                              ifelse(cyanos_18$Biomass > 2000 & cyanos_18$Biomass <= 5000, "2000 < 5000", 
                                                ifelse(cyanos_18$Biomass > 5000 & cyanos_18$Biomass <= 10000, "5000 < 10000", 
                                                  ifelse(cyanos_18$Biomass > 10000 & cyanos_18$Biomass <= 15000, "10000 < 15000", 
                                                    ifelse(cyanos_18$Biomass > 15000 & cyanos_18$Biomass <= 20000, "15000 < 20000",
                                                                                                                    "other")))))))))))

                                  


cyanos_18$biom.category <- as.factor(ordered(cyanos_18$biom.category, 
                                                         levels=c("0", "< 25", "25 < 50", "50 < 250", 
                                                                  "250 < 500", "500 < 1000", "1000 < 2000", 
                                                                  "2000 < 5000", "5000 < 10000", "10000 < 15000", "15000 < 20000")))


