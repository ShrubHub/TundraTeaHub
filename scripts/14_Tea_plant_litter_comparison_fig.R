# Litter bed Analysis
# 14 Aug 2016
# Haydn Thomas

# packages ----
library(tidyverse)
library(lme4)
library(stringr)
library(mgcv)
require(vegan)
require(grid)
require(gridExtra)
`%notin%` <- function(x, y)
  ! (x %in% y)
se <- function(x)
  sqrt(var(x, na.rm = T) / length(x, na.rm = T))

# Import Data ----
litter <- read.csv("data/Litterbags.csv")
litter1yr <- subset(litter, Date.of.recovery == "12/07/16")
litter1yr$Average <- as.numeric(as.character(litter1yr$Average))
litter1yr_leaf_K <- subset(litter1yr, Organ == "Leaf" & Origin == "K")

tea <- read.csv(file = "data/teabag_data.csv")
tea$Loss <- as.numeric(as.character(tea$Loss))
tea$Days <- as.numeric(as.character(tea$Days))

tea <- subset(tea, Plot == "DC_HT_1yr")
tea <- select(tea, Species = Tea_Type, Average = Loss)
litter <- select(litter1yr_leaf_K, Species, Average)

tea_litter <- rbind(tea, litter)
tea_litter$Species <- as.character(tea_litter$Species)
tea_litter[tea_litter$Species == "Green", ]$Species <- "Green Tea"
tea_litter[tea_litter$Species == "Rooibos", ]$Species <- "Rooibos Tea"

tea_litter$Type <- "Litter"
tea_litter[tea_litter$Species == "Green Tea", ]$Type <- "Green Tea"
tea_litter[tea_litter$Species == "Rooibos Tea", ]$Type <-
  "Rooibos Tea"

tea_litter[tea_litter$Species == "Rhododendron groenlandica", ]$Species <-
  "Rhod. groenlandica"
tea_litter[tea_litter$Species == "Moss A", ]$Species <- "Moss sp."
tea_litter[tea_litter$Species == "Silene acualis", ]$Species <-
  "Silene acaulis"


# Add funtional groups
tea_litter$F_Group <- NA
tea_litter[tea_litter$Species == "Moss sp.", ]$F_Group <- "Moss"
tea_litter[tea_litter$Species == "Silene acaulis", ]$F_Group <- "ES"
tea_litter[tea_litter$Species == "Rhod. groenlandica", ]$F_Group <-
  "ES"
tea_litter[tea_litter$Species == "Salix pulchra", ]$F_Group <- "DS"
tea_litter[tea_litter$Species == "Salix arctica", ]$F_Group <- "DS"
tea_litter[tea_litter$Species == "Betula glandulosa", ]$F_Group <-
  "DS"
tea_litter[tea_litter$Species == "Cetraria cucculata", ]$F_Group <-
  "L"
tea_litter[tea_litter$Species == "Pedicularis verticillata ", ]$F_Group <-
  "F"
tea_litter[tea_litter$Species == "Festuca rubra", ]$F_Group <- "G"
tea_litter[tea_litter$Species == "Poa arctica", ]$F_Group <- "G"
tea_litter[tea_litter$Species == "Petasites frigidus", ]$F_Group <-
  "F"
tea_litter[tea_litter$Species == "Stellaria longipes", ]$F_Group <-
  "F"
tea_litter[tea_litter$Species == "Equisetum avense", ]$F_Group <- "F"

tea_litter[tea_litter$Species == "Green Tea", ]$Species <- "green tea"
tea_litter[tea_litter$Species == "Rooibos Tea", ]$Species <-
  "rooibos tea"

pdf(file = "users/hthomas/tea/figures/tea_litterbed.pdf",
    width = 6,
    height = 5)
(
 tea_litter_plot <-  ggplot(tea_litter, aes(
    reorder(factor(Species), Average, median), Average * 100, fill = Type
  )) +
    geom_boxplot() +
    labs(y = "% Mass loss", x = "") +
    theme_bw() +
    coord_cartesian(ylim = c(0, 100)) +
    scale_fill_manual(
      values = c("#00b100", "goldenrod", "#9A0C0C"),
      name = ""
    ) +
    theme_bw() + theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(face = "italic"))
)
dev.off()

ggplot(litter1yr, aes(reorder(factor(Species), Average, median), Average, fill =
                        factor(Origin))) +
  geom_boxplot() +
  labs(y = "Mass loss", x = "Species") +
  theme_bw() +
  ggtitle("Litterbed Mass Loss") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




#### supplmentary C:n ration, C N content comparison with tundra trait database ####
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggrepel)
studied_species <- c("Silene acaulis","Ledum palustre","Salix pulchra","Salix arctica",
                     "Salix glauca", "Betula glandulosa","Festuca rubra","Petasites frigidus","Equisetum arvense")

TTT_traits <- fread(file.path("data","TTT_cleaned_dataset_v1.csv"))

unique(TTT_traits$Trait)

TTT_traits_CN <- TTT_traits[Trait%in% c("Leaf carbon (C) content per leaf dry mass",
                                        "Leaf nitrogen (N) content per leaf dry mass",
                                        "Leaf nitrogen/phosphorus (N/P) ratio"),]

TTT_traits_CN <- dcast(TTT_traits_CN , AccSpeciesName ~ Trait ,value.var = "Value" ,fun.aggregate = mean)

TTT_traits_CN <- TTT_traits_CN[complete.cases(TTT_traits_CN[,c("Leaf carbon (C) content per leaf dry mass",
                                                               "Leaf nitrogen (N) content per leaf dry mass")]),]
colnames(TTT_traits_CN) <- c("species_name","C_content","N_content","CN_ratio")


TTT_traits_CN_species <- TTT_traits_CN[species_name%in% studied_species,]


### teabag data ####
# green
2.019 - 0.246 

teabag_dt <- data.table(species_name= c("green tea","rooibos tea"), 
                        C_content= c(49.055,50.511)*10, 
                        N_content= c(4.02,1.185)*10, CN_ratio= c(12.23,42.87))


(tundra_species_C_N_plot <- ggplot(TTT_traits_CN,aes(x=C_content/10,y=N_content/10))+
  geom_point(color="grey70") +
  geom_point(data = TTT_traits_CN_species,fill="cadetblue",size=3,pch=21) +
  geom_text_repel(data = rbind(teabag_dt,TTT_traits_CN_species),aes(label =species_name ),size= 4,nudge_y = 0.25,min.segment.length = 0.75,fontface= c("plain" ,"plain",rep("italic",9 ))) +
  geom_point(data=teabag_dt,aes(fill=species_name),size=4,pch=21,alpha=0.85,show.legend = F)+
  scale_fill_manual(values =c("#00b100","#9A0C0C"))+
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = c(0.15,0.8)
  )+
  labs(fill = "Tea types",x = "Carbon content (%)",y = "Nitrogen content (%)")+
  scale_y_continuous(limits = c(0,6)))

ggplot(TTT_traits_CN,aes(x=CN_ratio,y=N_content/10,fill=CN_ratio))+
  geom_point(pch=21) +
  geom_point(data = TTT_traits_CN_species,size=3,pch=21) +
  geom_point(data=teabag_dt,size=4,pch=21)+
  scale_fill_viridis_c()+
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

ggplot(TTT_traits_CN,aes(x =CN_ratio))+ 
  geom_histogram(fill="grey78",color="grey20",origin = 5,binwidth = 1)+
  theme_bw()+ 
  geom_vline(xintercept = teabag_dt$CN_ratio)+
  theme(    panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black") )

pdf(file = "figures/tea_C_N_content.pdf",width = 6,height = 4)
tundra_species_C_N_plot
dev.off()

svg(file = "figures/tea_C_N_content.svg",width = 6,height = 4)
tundra_species_C_N_plot
dev.off()


ggarrange(tea_litter_plot,tundra_species_C_N_plot,nrow=2 )
