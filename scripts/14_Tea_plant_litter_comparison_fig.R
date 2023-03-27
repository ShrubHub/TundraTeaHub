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
litter <- read.csv("users/hthomas/tea/data/Litterbags.csv")
litter1yr <- subset(litter, Date.of.recovery == "12/07/16")
litter1yr$Average <- as.numeric(as.character(litter1yr$Average))
litter1yr_leaf_K <- subset(litter1yr, Organ == "Leaf" & Origin == "K")

tea <- read.csv(file = "users/hthomas/tea/data/teabag_data.csv")
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
  ggplot(tea_litter, aes(
    reorder(factor(Species), Average, median), Average * 100, fill = Type
  )) +
    geom_boxplot() +
    labs(y = "% Mass loss", x = "") +
    theme_bw() +
    coord_cartesian(ylim = c(0, 100)) +
    scale_fill_manual(
      values = c("# 00b100", "goldenrod", "# 9A0C0C"),
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
