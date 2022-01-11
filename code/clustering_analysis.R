##Cluster Analysis

##libraries
library(tidyverse)
library(lubridate)
library(here)
library(vegan)
library(indicspecies)


## 2018 and 2019 Phyto Data Matrices ####
phyto_matrix_19 <- read_csv(here("data/processed_data", "2019_phyto_matrix_KP.csv"))

phyto_matrix_18 <- read_csv(here("data/processed_data", "2018_phyto_matrix_KP.csv"))

# drop DOY column for analysis 
phyto_biomass_ord <- phyto_matrix_19[, -(1)]
phyt18_biomass_ord <- phyto_matrix_18[, -(1)]

#tranform to bray-curtis dissimilarity indices to look at ordination
phyto_bray <- vegdist(phyto_biomass_ord, method = "bray")
phyto_bray_18 <- vegdist(phyt18_biomass_ord, method = "bray")

#view ordination (PCoA), display = "sites"
ord<-cmdscale(phyto_bray)
ordiplot(ord, dis = "si")
ordicluster(ord, clust_sing)

ord18 <- cmdscale(phyto_bray_18)
ordiplot(ord18, dis = "si")


## 2019 K-Means Clustering ####

##K-means clustering 2019  (using Hellinger transformed data and groups are given by analyst)
phyto_kmeans_2 <- kmeans(decostand(phyto_biomass_ord, "hell"), 2) 
phyto_kmeans_3 <- kmeans(decostand(phyto_biomass_ord, "hell"), 3) 
phyto_kmeans_4 <- kmeans(decostand(phyto_biomass_ord, "hell"), 4) 
phyto_kmeans_5 <- kmeans(decostand(phyto_biomass_ord, "hell"), 5) 


ordiplot(ord, dis = "si")
ordihull(ord, phyto_kmeans$cluster, col = "red")

k_2 <- phyto_kmeans_2$cluster
k_3 <- phyto_kmeans_3$cluster
k_4 <- phyto_kmeans_4$cluster
k_5 <- phyto_kmeans_5$cluster

##2018 K-means Clustering ####
##2 groups
kmeans_18_2 <- kmeans(decostand(phyt18_biomass_ord, "hell"), 2) 
k18_2 <- kmeans_18_2$cluster

##3 groups
kmeans_18_3 <- kmeans(decostand(phyt18_biomass_ord, "hell"), 3) 
k18_3 <- kmeans_18_3$cluster

##4 groups
kmeans_18_4 <- kmeans(decostand(phyt18_biomass_ord, "hell"), 4) 
k18_4 <- kmeans_18_4$cluster

##5 groups
kmeans_18_5 <- kmeans(decostand(phyt18_biomass_ord, "hell"), 5) 
k18_5 <- kmeans_18_5$cluster

#view plot
ordiplot(ord18, dis = "si")
ordihull(ord18, k18_5, col = "red")

##some community summary stats using labdsv
library(labdsv)
#species occurance frequencies in each class 
spec_freq19 <- const(phyto_biomass_ord, k_3)

#mean abundances in each class
spec_abund19 <- importance(phyto_biomass_ord, k_3)


##method of IndVal (r/indicspecies) used in this paper
#See indicspecies vignette:https://cran.r-project.org/web/packages/indicspecies/vignettes/indicspeciesTutorial.pdf

library(indicspecies)

indval_phyto19_2 <- multipatt(phyto_biomass_ord, k_2, control = how(nperm = 999), duleg = TRUE)
summary(indval_phyto19_2, indvalcomp = TRUE)

indval_phyto19_3 <- multipatt(phyto_biomass_ord, k_3, control = how(nperm = 999), duleg = TRUE)
summary(indval_phyto19_3, indvalcomp = TRUE)

indval_phyto19_3 <- multipatt(phyto_biomass_ord, k_3, control = how(nperm = 999))
summary(indval_phyto19_3, indvalcomp = TRUE)

indval_phyto19_4 <- multipatt(phyto_biomass_ord, k_4, control = how(nperm = 999))
summary(indval_phyto19_4, indvalcomp = TRUE)

indval_phyto19_5 <- multipatt(phyto_biomass_ord, k_5, control = how(nperm = 999))
summary(indval_phyto19_5, indvalcomp = TRUE)

#display A/specificity (prob site belongs to cluster) 
#and B/fidelity (prob species belongs to cluster)
summary(indval_phyto, indvalcomp = TRUE)


phyto_pa <-  ifelse(phyto_biomass_ord > 0,1,0)
phyto_pa <- as.data.frame(phyto_pa)
phi <-  multipatt(phyto_pa, k, func = "r.g", control = how(nperm=999))
summary(phi)
round(head(phi$str),3)

##phis
phi19_2 <-  multipatt(phyto_pa, k_2, func = "r.g", control = how(nperm=999), duleg = TRUE)
summary(phi19_2)
phi19_3 <-  multipatt(phyto_pa, k_3, func = "r.g", control = how(nperm=999), duleg = TRUE)
summary(phi19_3)
phi19_4 <-  multipatt(phyto_pa, k_4, func = "r.g", control = how(nperm=999))
summary(phi19_4)
phi19_5 <-  multipatt(phyto_pa, k_5, func = "r.g", control = how(nperm=999))
summary(phi19_5)

# 2018 (set duleg = true to consider single groups only, not combos)
indval_phyto18_2 <- multipatt(phyt18_biomass_ord, k18_2, control = how(nperm = 999), duleg = TRUE)
summary(indval_phyto18_2, indvalcomp = TRUE)

indval_phyto18_3 <- multipatt(phyt18_biomass_ord, k18_3, control = how(nperm = 999))
summary(indval_phyto18_3, indvalcomp = TRUE)

indval_phyto18_3 <- multipatt(phyt18_biomass_ord, k18_3, control = how(nperm = 999), duleg = TRUE)
summary(indval_phyto18_3, indvalcomp = TRUE)

indval_phyto18_4 <- multipatt(phyt18_biomass_ord, k18_4, control = how(nperm = 999))
summary(indval_phyto18_4, indvalcomp = TRUE)

indval_phyto18_5 <- multipatt(phyt18_biomass_ord, k18_5, control = how(nperm = 999))
summary(indval_phyto18_5, indvalcomp = TRUE)

##converts data to presence absence
phyto_pa18 <-  ifelse(phyt18_biomass_ord > 0,1,0)
phyto_pa18 <- as.data.frame(phyto_pa18)

#calculates phi coefficients (func r.g. accounts for uneven DOYs between groups)
phi18 <-  multipatt(phyto_pa18, k18_2, func = "r.g", control = how(nperm=999), duleg = TRUE)
summary(phi18)

phi18_3 <-  multipatt(phyto_pa18, k18_3, func = "r.g", control = how(nperm=999))
summary(phi18_3)

phi18_4 <-  multipatt(phyto_pa18, k18_4, func = "r.g", control = how(nperm=999))
summary(phi18_4)

phi18_5 <-  multipatt(phyto_pa18, k18_5, func = "r.g", control = how(nperm=999))
summary(phi18_5)

round(head(phi18$str),3)

#Adding Partitions to a matrix for summary stats
#2018
DOY_2018 <- phyto_matrix_18$DOY
DOY_2018 <- as.data.frame(DOY_2018)
DOY_2018 <- DOY_2018 %>% rename(DOY = DOY_2018) 
DOY_2018$K <- k18_3

#recode for clarity
DOY_2018$K <- if_else(DOY_2018$K == 3, "A",
                      if_else(DOY_2018$K == 1, "B", "C"))

#2019 
DOY_2019 <- phyto_matrix_19$DOY
DOY_2019 <- as.data.frame(DOY_2019)
DOY_2019 <- DOY_2019 %>% rename(DOY = DOY_2019) 
DOY_2019$K <- k_3

#recode for clarity
DOY_2019$K <- if_else(DOY_2019$K == 3, "A",
                      if_else(DOY_2019$K == 2, "B", "C"))


