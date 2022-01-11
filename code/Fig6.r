#Figure 6 - NMDS of cyanos w/ toxin vectors

## libraries
library(tidyverse)
library(lubridate)
library(here)
library(vegan)
library(ggrepel)
library(patchwork)

## 2018 ####
phytos_2018 <- read_csv(here("data/processed_data/", "BP_PhytoTax_2018_KP.csv"))

cyanos_18 <- phytos_2018 %>% filter(Group == "Cyanophyte")

cyano_matrix_18 <- cyanos_18 %>%
  select(DOY,
         name_code,
         Cell_Density_L,
         Biomass_mg_m3) %>%
  group_by(DOY, name_code) %>%
  dplyr::summarize_at(vars(Cell_Density_L, Biomass_mg_m3), list(total = sum)) %>%
  droplevels()

cyano_matrix_18_biomass <- cyano_matrix_18 %>%
  select(DOY, name_code, Biomass_mg_m3_total) %>%
  pivot_wider(names_from = name_code, values_from =  Biomass_mg_m3_total, values_fill = 0)

##drop DOY 172 b/c no toxin sample that day
cyano_matrix_18_biomass <-cyano_matrix_18_biomass[-c(5),] 

##rename the Anabaena spp. to Dolichospermum 
cyano_matrix_18_biomass <- cyano_matrix_18_biomass %>% rename(Dol.flo = Ana.flo,
                                                              Dol.cra = Ana.cra,
                                                              Dol.sol = Ana.sol)

#drop DOY column for ordination
as.data.frame(cyano_matrix_18_biomass)
cyano_biomass_ord18 <- cyano_matrix_18_biomass[, -(1)]

#NMDS
set.seed(123)
cyano_nmds18 = metaMDS(cyano_biomass_ord18, distance = "bray", autotransform = F) 
cyano_nmds18

#checks
goodness(cyano_nmds18)
stressplot(cyano_nmds18)
ordiplot(cyano_nmds18)

##extract data scores 
cyano_datascores_18 = as.data.frame(scores(cyano_nmds18)) 
#add DOY column 
cyano_datascores_18$DOY = cyano_matrix_18_biomass$DOY
#check
head(cyano_datascores_18)

##extract species scores
cyano_speciesscores_18 <- as.data.frame(scores(cyano_nmds18, "species"))  
cyano_speciesscores_18$species <- rownames(cyano_speciesscores_18)  
head(cyano_speciesscores_18) 

##plot
cyano_nmdsplot_18 <- ggplot() + 
  ## geom_point(data=cyano_speciesscores_18, aes(x=NMDS1, y=NMDS2), size = 2.5, colour = "seagreen", shape = 8, stroke =1) +
  geom_text_repel(data=cyano_speciesscores_18,aes(x=NMDS1,y=NMDS2,label=species), size=4.5, colour = "seagreen4", fontface = "bold", alpha = 0.55, point.size = NA) +  # add the species labels
  geom_point(data=cyano_datascores_18,aes(x=NMDS1,y=NMDS2),size=2, alpha = 0.7) + # add the point markers
  geom_text_repel(data=cyano_datascores_18,aes(x=NMDS1,y=NMDS2,label=DOY),size=3.5, alpha = 0.7, box.padding = 0.3, fontface = "bold") +  # add the DOY labels ##coord_equal() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

cyano_nmdsplot_18_2 <- ggplot() + 
  ## geom_point(data=cyano_speciesscores_18, aes(x=NMDS1, y=NMDS2), size = 2.5, colour = "seagreen", shape = 8, stroke =1) +
  geom_text_repel(data=cyano_speciesscores_18,aes(x=NMDS1,y=NMDS2,label=species), size=4.5, colour = "seagreen4", fontface = "bold", alpha = 0.55, point.size = NA) +  # add the species labels
  geom_point(data=cyano_datascores_18,aes(x=NMDS1,y=NMDS2),size=2, alpha = 0.7) + # add the point markers
  ## geom_text_repel(data=cyano_datascores_18,aes(x=NMDS1,y=NMDS2,label=DOY),size=3.5, alpha = 0.6, box.padding = 0.3, fontface = "bold") +  # add the DOY labels ##coord_equal() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

##2018 Toxins ####
##> LOD is 0, >LOD<LOQs replaced with LOD

toxs_2018 <- read_csv(here("data/processed_data/", "Toxs2018_zerosLOQs.csv"))

intra_tox18 <- toxs_2018 %>% filter(Depth_m == 0.8)

#create matrix for ordination 
intra_tox18_matrix <- intra_tox18 %>% mutate(DOY = yday(Date)) %>% 
  select(DOY, everything(), -Date, -Sample_Type, -Depth_m) 

as.data.frame(intra_tox18_matrix)
intra_tox18_ord <- intra_tox18_matrix[, -(1)]

##sqrt transformation of toxins
sqrt_tox18<-apply(intra_tox18_ord, c(1,2), function(x) sqrt(x))
sqrt_tox18

sqrt_tox18 <- as.data.frame(sqrt_tox18)

sqrt_tox18_vecs <- envfit(cyano_nmds18, sqrt_tox18, permu = 999, na.rm = TRUE)
sqrt_tox18_vecs

sqrt_toxscores_18 <- as.data.frame(scores(sqrt_tox18_vecs, display = "vectors")) #extract scores from envfit
sqrt_toxscores_18 <- cbind(sqrt_toxscores_18, toxs = rownames(sqrt_toxscores_18)) #add names
sqrt_toxscores_18 <- cbind(sqrt_toxscores_18, pval = sqrt_tox18_vecs$vectors$pvals) # add pvalues 
sig_sqrt_toxscores_18 <- subset(sqrt_toxscores_18, pval<=0.051) #subset variables significant at 0.05

head(sqrt_toxscores_18)

##add vectors to plot
vec_plot18 <- cyano_nmdsplot_18_2 + 
  geom_segment(data = sig_sqrt_toxscores_18, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd=0.8) + #sig toxins
  geom_text_repel(data = sig_sqrt_toxscores_18, aes(x=NMDS1, y=NMDS2, label = toxs), size = 3, bg.colour = "white", bg.r = 0.15, colour = "black", fontface = "bold", direction = "both", segment.size = NA, segment.angle = 180) #add labels for toxins

## 2019 ####
phytos_2019 <- read_csv(here("data/processed_data/", "BP_PhytoTax_2019_KP.csv"))

cyanos_19 <- phytos_2019 %>% filter(Group == "Cyanophyte")

cyano_matrix <- cyanos_19 %>%
  select(DOY,
         name_code,
         Cell_Density_L,
         Biomass_mg_m3) %>%
  group_by(DOY, name_code) %>%
  dplyr::summarize_at(vars(Cell_Density_L, Biomass_mg_m3), list(total = sum)) %>%
  droplevels()

cyano_matrix_biomass <- cyano_matrix %>%
  select(DOY, name_code, Biomass_mg_m3_total) %>%
  pivot_wider(names_from = name_code, values_from =  Biomass_mg_m3_total, values_fill = 0)

##rename Anabaena to Dolichospermum
cyano_matrix_biomass <- cyano_matrix_biomass %>% rename(Dol.flo = Ana.flo,
                                                        Dol.cra = Ana.cra)

#drop DOY column for ordination
as.data.frame(cyano_matrix_biomass)
cyano_biomass_ord <- cyano_matrix_biomass[, -(1)]

#NMDS
set.seed(123)
cyano_nmds = metaMDS(cyano_biomass_ord, distance = "bray", autotransform = F) 
cyano_nmds

#checks
goodness(cyano_nmds)
stressplot(cyano_nmds)
ordiplot(cyano_nmds)

##extract data scores 
cyano_data_scores = as.data.frame(scores(cyano_nmds)) 
#add DOY to data frame 
cyano_data_scores$DOY = cyano_matrix_biomass$DOY
#check 
head(cyano_data_scores)

##extract species scores
cyano_species_scores <- as.data.frame(scores(cyano_nmds, "species"))  
cyano_species_scores$species <- rownames(cyano_species_scores)  
head(cyano_species_scores) 

cyano_nmds_plot_19 <- ggplot() + 
  geom_text_repel(data=cyano_species_scores,aes(x=NMDS1,y=NMDS2,label=species), size=4.5, colour = "seagreen4", fontface = "bold", alpha = 0.55, point.size = NA) +  # add the species labels
  geom_point(data=cyano_data_scores,aes(x=NMDS1,y=NMDS2),size=2, alpha = 0.7) + # add the point markers
  geom_text_repel(data=cyano_data_scores,aes(x=NMDS1,y=NMDS2,label=DOY),size=3.5, alpha = 0.7, box.padding = 0.3, fontface = "bold") +  # add the DOY labels +
  ##coord_equal() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

cyano_nmds_plot_19_2 <- ggplot() + 
  geom_text_repel(data=cyano_species_scores,aes(x=NMDS1,y=NMDS2,label=species), size=4.5, colour = "seagreen4", fontface = "bold", alpha = 0.55, point.size = NA) +  # add the species labels
  geom_point(data=cyano_data_scores,aes(x=NMDS1,y=NMDS2),size=2, alpha = 0.7) + # add the point markers
  ##geom_text_repel(data=cyano_data_scores,aes(x=NMDS1,y=NMDS2,label=DOY),size=3.5, alpha = 0.6, box.padding = 0.3, fontface = "bold") +  # add the DOY labels +
  ##coord_equal() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## 2019 Toxins ####
##LODs replaced with zeros, >LOD<LOQs replaced with LOQ

toxs_2019 <- read_csv(here("data/processed_data/", "Toxs2019_zerosLOQs.csv"))

intra_tox <- toxs_2019 %>% filter(Depth_m == 0.8)
intra_tox_matrix <- intra_tox %>% mutate(DOY = yday(Date)) %>% 
  select(DOY, everything(), -Date, -Sample_Type, -Depth_m) 

as.data.frame(intra_tox_matrix)
intra_tox_ord <- intra_tox_matrix[, -(1)]

##sqrt transformation of toxins
sqrt_tox<-apply(intra_tox_ord, c(1,2), function(x) sqrt(x))
sqrt_tox

sqrt_tox <- as.data.frame(sqrt_tox)

sqrt_tox_vecs <- envfit(cyano_nmds, sqrt_tox, permu = 999, na.rm = TRUE)
sqrt_tox_vecs

sqrttox_scores <- as.data.frame(scores(sqrt_tox_vecs, display = "vectors")) #extract scores from envfit
sqrttox_scores <- cbind(sqrttox_scores, toxs = rownames(sqrttox_scores)) #add tox names
sqrttox_scores <- cbind(sqrttox_scores, pval = sqrt_tox_vecs$vectors$pvals) # add pvalues
sig_sqrttox_scores <- subset(sqrttox_scores, pval<=0.051) #subset toxins significant at 0.05

head(sqrttox_scores)

vec_plot19 <- cyano_nmds_plot_19_2 + 
  geom_segment(data = sig_sqrttox_scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd=0.8) + #sig toxins
  geom_text_repel(data = sig_sqrttox_scores, aes(x=NMDS1, y=NMDS2, label = toxs), 
                  size = 3, bg.colour = "white", bg.r = 0.15, colour = "black", fontface = "bold", direction = "both", segment.size = NA, segment.angle = 180) #add labels for toxins

## FIGURE 5 -patchwork of the ordinations ####

layout <- "AABB
           CCDD"

cyano_nmdsplot_18 +  cyano_nmds_plot_19 + vec_plot18 + vec_plot19 + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(face = "bold", size = 12))

## Spearman correlations ####
##Corr matrix of cyanos with most prevalent toxins ####
library(Hmisc)

## run code for Flatten matrix function (see below)

#2018
#reduce toxins
red_tox18_matrix <- intra_tox18_matrix %>% select(DOY, MC_total, AP_A, AP_B)
#join two 2018 matrices
cyano_tox18 <- left_join(cyano_matrix_18_biomass, red_tox18_matrix, by = "DOY")
cyano_tox18 <- cyano_tox18[, -(1)]

cor_tox18 <- rcorr(as.matrix(cyano_tox18), type = "spearman")
cor_tox18

cor_tox18_print <- flattenCorrMatrix(cor_tox18$r, cor_tox18$P)
cor_tox18_print

#2019
red_tox19_matrix <- intra_tox_matrix %>% select(DOY, MC_total, AP_A, AP_B)
#join two 2019 matrices
cyano_tox <- left_join(cyano_matrix_biomass, red_tox19_matrix, by = "DOY")
cyano_tox <- cyano_tox[, -(1)]

cor_tox <- rcorr(as.matrix(cyano_tox), type = "spearman")
cor_tox

cor_tox_print <- flattenCorrMatrix(cor_tox$r, cor_tox$P)


##code from web for function to make correlation matrix easier to read

# flattenCorrMatrix code from: http://www.sthda.com/english/wiki/correlation-matrix-formatting-and-visualization

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

