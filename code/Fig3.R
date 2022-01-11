# Figure 3 - major cyano groups & heterocytes

## Libraries

##Data ####

#phyto (Planktothrix and N-fixers)
D_P_ALL18 <-  read_csv(here("data/processed_data", "MajorCyanos_BP_2018_KP.csv"))

D_P_ALL19 <-  read_csv(here("data/processed_data", "MajorCyanos_BP_2019_KP.csv"))

## Plots ####

#theme

mytheme <-   theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 10)) +
  theme(axis.text.y = element_text(face = "bold", size = 10))+
  theme(axis.title.x = element_text(face = "bold", size =10))+
  theme(axis.title.y = element_text(face = "bold", size = 10)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#heterocytes
hets_18 <- read_csv(here("data/processed_data", "BP_heterocytes_2018_KP.csv"))

hets_19 <- read_csv(here("data/processed_data", "BP_heterocytes_2019_KP.csv"))

#2018 plot of D and P 
DP_18_plot <- ggplot(D_P_ALL18, aes(DOY, Genus_Biomass, fill = fct_reorder2(Genus, Genus_Biomass, DOY))) +
  annotate("rect", xmin = 164, xmax = 197, ymin = 0, ymax = 23000, alpha = .25, fill = "lavenderblush4") +
  annotate("rect", xmin = 197, xmax = 221, ymin = 0, ymax = 23000, alpha = .6, fill = "lavenderblush4") + 
  #annotate("rect", xmin = 220, xmax = 221, ymin = 0, ymax = 23000, alpha = .25, fill = "lavenderblush4") + 
  annotate("rect", xmin = 221, xmax = 260,ymin = 0, ymax = 23000, alpha = .6, fill = "lavenderblush4" ) + 
  geom_area(colour = "grey")+
  scale_fill_manual(values = c(Planktothrix = "#440154FF", Dolichospermum = "#35B779FF" ))+ 
  scale_x_continuous(breaks = seq(150, 275, 10), 
                     limits = c(149, 276), expand = c(0,0)) +  
  scale_y_continuous(breaks=seq(0, 20000, 5000), limits = c(0, 23000),expand = c(0,0))+
  xlab("Day of Year 2018") + ylab(expression(bold("Total Biomass mg/m"^3))) +
  mytheme + 
  theme(legend.position = "none")+
  annotate("segment", x = 149.5, xend = 163.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90,  length = unit(.2,"cm"))) +
  annotate("text", x = 156, y = 22400, label = "Grp. A",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 164.5, xend = 196.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 180, y = 22400, label = "Grp. B",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 197.5, xend = 259.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 229, y = 22400, label = "Grp. C",
           fontface = "bold", colour = "black", size = 3.5)+
  geom_line(aes(y = D_P_ALL18$total_biomass), linetype = "dashed", size = 1, colour = "orangered3") 

#2019 plot of D and P 
DP_19_plot <- ggplot(D_P_ALL19, aes(DOY, Genus_Biomass, fill = fct_reorder2(Genus, Genus_Biomass, DOY))) +
  annotate("rect", xmin = 186, xmax = 242, ymin = 0, ymax = 23000, alpha = .25, fill = "lavenderblush4") +
  annotate("rect", xmin = 242, xmax = 276,ymin = 0, ymax = 23000, alpha = .56, fill = "lavenderblush4" ) + 
  geom_area(colour = "grey")+
  scale_fill_manual(values = c(Planktothrix = "#440154FF", Dolichospermum = "#35B779FF" ))+ 
  scale_x_continuous(breaks = seq(150, 275, 10), 
                     limits = c(149, 276), expand = c(0,0)) +  
  scale_y_continuous(breaks=seq(0, 20000, 5000), limits = c(0, 23000),expand = c(0,0))+
  xlab("Day of Year 2019") + ylab(expression(bold("Total Biomass mg/m"^3))) +
  mytheme +
  theme(legend.position = c(.2, .75),
        legend.text = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank())+
  guides(fill = guide_legend(title = NULL)) + 
  annotate("segment", x = 149.5, xend = 185.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90,  length = unit(.2,"cm"))) +
  annotate("text", x = 166, y = 22400, label = "Grp. A",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 186.5, xend = 241.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 212, y = 22400, label = "Grp. B",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 242.5, xend = 275.5, y = 21500, yend = 21500,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 259, y = 22400, label = "Grp. C",
           fontface = "bold", colour = "black", size = 3.5)+
  geom_line(aes(y = D_P_ALL19$total_biomass),  linetype = "dashed", size = 1, colour = "orangered3") 

##2018 heterocytes 

##color vector
cols18 <- c(Dol.cra = "#22A884FF", Dol.flo = "#7AD151FF", Dol.sol = "#F68F46FF")

hetplot_18 <- ggplot(hets_18, aes(x = DOY, y = Biomass_mg_m3,  fill = factor(name_code))) +
  annotate("rect", xmin = 164, xmax = 197, ymin = 0, ymax = 1050, alpha = .25, fill = "lavenderblush4") +
  annotate("rect", xmin = 197, xmax = 221, ymin = 0, ymax = 1050, alpha = .6, fill = "lavenderblush4") + 
  #annotate("rect", xmin = 220, xmax = 221, ymin = 0, ymax = 1050, alpha = .25, fill = "lavenderblush4") + 
  annotate("rect", xmin = 221, xmax = 260,ymin = 0, ymax = 1050, alpha = .6, fill = "lavenderblush4" ) + 
  geom_area(colour = "grey") +
  scale_fill_manual(values = c(Dol.cra = "#22A884FF", Dol.flo = "#7AD151FF", Dol.sol = "#F68F46FF"),
                    labels = c("D. crassa", "D. flos-aquae", "D. solitaria"))+ 
  scale_x_continuous(breaks = seq(150, 275, 10), 
                     limits = c(149, 276), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0, 1000, 250),
                     limits = c(0, 1050),expand = c(0,0))+
  xlab("Day of Year 2018") + ylab(expression(bold("Heterocyte Biomass mg/m"^3))) +
  mytheme + 
  theme(legend.position = c(.15, .75),
        legend.text = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(fill = guide_legend(title = NULL)) + 
  annotate("segment", x = 149.5, xend = 163.5, y = 975, yend = 975,
           arrow = arrow(ends = "both", angle = 90,  length = unit(.2,"cm"))) +
  annotate("text", x = 156, y = 1025, label = "Grp. A",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 164.5, xend = 196.5, y = 975, yend = 975,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 180, y = 1025, label = "Grp. B",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 197.5, xend = 259.5, y = 975, yend = 975,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 229, y = 1025, label = "Grp. C",
           fontface = "bold", colour = "black", size = 3.5)

##2019 heterocytes

##color vector
cols19 <- c(Dol.cra = "#22A884FF", Dol.flo = "#7AD151FF", Aph.flo = "#FDE725FF")

hetplot_19 <- ggplot(hets_19, aes(x = DOY, y = Biomass_mg_m3,  fill = fct_reorder2(name_code, DOY, Biomass_mg_m3))) +
  annotate("rect", xmin = 186, xmax = 242, ymin = 0, ymax = 1050, alpha = .25, fill = "lavenderblush4") +
  annotate("rect", xmin = 242, xmax = 276,ymin = 0, ymax = 1050, alpha = .56, fill = "lavenderblush4" ) + 
  geom_area(colour = "grey") +
  scale_fill_manual(values = cols19, labels = c("D. crassa", "D. flos-aquae", "A. flos-aquae"))+ 
  scale_x_continuous(breaks = seq(150, 275, 10), 
                     limits = c(149, 276), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0, 1000, 250),
                     limits = c(0, 1050),expand = c(0,0))+
  xlab("Day of Year 2019") + ylab(expression(bold("Heterocyte Biomass mg/m"^3))) +
  mytheme + 
  theme(legend.position = c(.15, .75),
        legend.text = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(fill = guide_legend(title = NULL)) + 
  annotate("segment", x = 149.5, xend = 185.5, y = 975, yend = 975,
           arrow = arrow(ends = "both", angle = 90,  length = unit(.2,"cm"))) +
  annotate("text", x = 166, y = 1025, label = "Grp. A",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 186.5, xend = 241.5, y = 975, yend = 975,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 212, y = 1025, label = "Grp. B",
           fontface = "bold", colour = "black", size = 3.5)+
  annotate("segment", x = 242.5, xend = 275.5, y = 975, yend = 975,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 259, y = 1025, label = "Grp. C",
           fontface = "bold", colour = "black", size = 3.5)

#Format plot 

library(cowplot)

plot_grid(DP_18_plot, DP_19_plot, hetplot_18, hetplot_19, ncol=2,
          labels = c('a)', 'b)', 'c)', 'd)'), label_size = 12)  