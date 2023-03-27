#Tea bag linear versus not
#18 Nov 2021
#Sleeping willow's supervisor is making a brew

#Detach packages####
detachAllPackages <- function() {
  basic.packages <-
    c(
      "package:stats",
      "package:graphics",
      "package:grDevices",
      "package:utils",
      "package:datasets",
      "package:methods",
      "package:base"
    )
  
  package.list <-
    search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  
  package.list <- setdiff(package.list, basic.packages)
  
  if (length(package.list) > 0)
    for (package in package.list)
      detach(package, character.only = TRUE)
  
}

detachAllPackages()

#Question 1 - Differences between teas

# packages ----
library(tidyverse)
library(gridExtra)

#Set some custom functions#
`%notin%` <- function(x, y)
  ! (x %in% y)
se <- function(x)
  sqrt(var(x, na.rm = T) / length(x))

#Import data####
tea <- read.csv(file = "users/hthomas/tea/data/combined_tea.csv")

#Remove daily tea - too confusing!
tea <- subset(tea,!grepl("CG_DT_HT", tea$Plot))

#Remove sub zero plots
tea <- subset(tea, Loss > 0)
tea[tea$Tea_Type == "Rooibos" & tea$Loss > 0.5, ]$Loss <- NA

#Make hemisphere variable
tea <- tea %>% mutate(Hemisphere = ifelse(Lon > 0, "west", "east"))

#Make sure only using control plots
ambient <- subset(tea, Treatment == "None")

#Split into seasons to make things easier
summer <- subset(ambient, Season == "Summer")
year <- subset(ambient, Season == "Year")
winter <- subset(ambient, Season == "Winter")

green <- filter(summer, Tea_Type == "Green")
red <- filter(summer, Tea_Type == "Rooibos")

# Air Temp.

(
  air_temp <- ggplot() +
    geom_point(
      data = summer,
      aes(airtemp_mean, (Loss) * 100, colour = Tea_Type),
      alpha = 0.5
    ) +
    geom_smooth(data = summer %>% filter(Tea_Type == "Green"), aes(airtemp_mean, (Loss)*100, group = Tea_Type), method = "loess", se = FALSE, colour = "darkgreen", linetype = 1, size = 1.5) +
    geom_smooth(data = summer %>% filter(Tea_Type == "Rooibos"), aes(airtemp_mean, (Loss)*100, group = Tea_Type), method = "loess", se = FALSE, colour = "#500e00", linetype = 1, size = 1.5) +
    
    geom_smooth(data = green, aes(airtemp_mean, (Loss)*100, group = Hemisphere), method = "loess", se = FALSE, colour = "darkgreen", linetype = 2) +
    geom_smooth(data = red, aes(airtemp_mean, (Loss)*100, group = Hemisphere), method = "loess", se = FALSE, colour = "#500e00", linetype = 2) +
    theme_classic() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    scale_colour_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    labs(y = "Mass loss (%, summer)", x = "Site summer air temperature (°C)") +
    #scale_x_continuous(breaks = seq(0,50,10)) +
    theme(legend.position = "none") +
    ggtitle("")
)

# Soil Temp.

(
  soil_temp <- ggplot() +
    geom_point(
      data = summer,
      aes(soiltemp_mean, (Loss) * 100, colour = Tea_Type),
      alpha = 0.5
    ) +
    geom_smooth(data = summer %>% filter(Tea_Type == "Green"), aes(soiltemp_mean, (Loss)*100, group = Tea_Type), method = "loess", se = FALSE, colour = "darkgreen", linetype = 1, size = 1.5) +
    geom_smooth(data = summer %>% filter(Tea_Type == "Rooibos"), aes(soiltemp_mean, (Loss)*100, group = Tea_Type), method = "loess", se = FALSE, colour = "#500e00", linetype = 1, size = 1.5) +
    geom_smooth(data = green, aes(soiltemp_mean, (Loss)*100, group = Hemisphere), method = "loess", se = FALSE, colour = "darkgreen", linetype = 2) +
    geom_smooth(data = red, aes(soiltemp_mean, (Loss)*100, group = Hemisphere), method = "loess", se = FALSE, colour = "#500e00", linetype = 2) +
    theme_classic() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    scale_colour_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    labs(y = "Mass loss (%, summer)", x = "Site summer soil temperature (°C)") +
    #scale_x_continuous(breaks = seq(0,50,10)) +
    theme(legend.position = "none") +
    ggtitle("")
)

# Soil moisture

(
  soil_moisture <- ggplot() +
    geom_point(
      data = summer,
      aes(moisture_mean, (Loss) * 100, colour = Tea_Type),
      alpha = 0.5
    ) +
    geom_point(
      data = (summer %>% filter(moisture_mean > 75)),
      aes(moisture_mean, (Loss) * 100, colour = Tea_Type),
      shape = 15,
      alpha = 1,
      size = 2
    ) +
    geom_smooth(data = summer %>% filter(Tea_Type == "Green"), aes(moisture_mean, (Loss)*100, group = Tea_Type), method = "loess", se = FALSE, colour = "darkgreen", linetype = 1, size = 1.5) +
    geom_smooth(data = summer %>% filter(Tea_Type == "Rooibos"), aes(moisture_mean, (Loss)*100, group = Tea_Type), method = "loess", se = FALSE, colour = "#500e00", linetype = 1, size = 1.5) +
    geom_smooth(data = green, aes(moisture_mean, (Loss)*100, group = Hemisphere), method = "loess", se = FALSE, colour = "darkgreen", linetype = 2) +
    geom_smooth(data = red, aes(moisture_mean, (Loss)*100, group = Hemisphere), method = "loess", se = FALSE, colour = "#500e00", linetype = 2) +
    theme_classic() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    scale_colour_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    labs(y = "Mass loss (%, summer)", x = "Site summer soil moisture (%)") +
    #scale_x_continuous(breaks = seq(0,50,10)) +
    theme(legend.position = "none")
)

pdf(file="users/hthomas/tea/figures/GAM_models.pdf", width = 10, height = 4)

grid.arrange(air_temp, soil_temp, soil_moisture, ncol=3)

dev.off()

summer_test <- summer %>% filter(moisture_mean > 75)
