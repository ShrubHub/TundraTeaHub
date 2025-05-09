#Tea bag full analysis script
#20 Nov 2017
#Sleeping willow is making a brew

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

####Open packages####
library(tidyverse)
library(raster)
library(rgdal)
library(lme4)
library(effects)
library(stringr)
require(gridExtra)
#library(brms)
#library(rstan)
#library(StanHeaders)
library(MuMIn)
library(MCMCglmm)

#Set some custom functions#
`%notin%` <- function(x, y)
  ! (x %in% y)
se <- function(x)
  sqrt(var(x, na.rm = T) / length(x))

#Import data####
tea <- read.csv(file = "data/teabag_data_update.csv",encoding = "UTF-8")

tea$Tea_init <-
  as.numeric(as.character(tea$Tea_init)) #Convert to number
tea$Tea_final <-
  as.numeric(as.character(tea$Tea_final)) #Convert to number
tea$Loss <-
  1 - (tea$Tea_final / tea$Tea_init) #Recalculate loss using full values
tea$Days <- as.numeric(as.character(tea$Days)) #Convert to number
tea <- tea[!is.na(tea$Loss), ] #Remove NAs (4859 -> 4728)
tea <- tea[!is.na(tea$Days), ] #Remove NAs (none)
tea$Burial <-
  as.Date(tea$Burial, format = "%d/%m/%Y") #Convert date format
tea$Recovery <-
  as.Date(tea$Recovery, format = "%d/%m/%Y") #Convert date format
tea$latlon <-
  paste(tea$Lat, "_", tea$Lon, sep = "") #Add unique coordinate column

#Test decay rates (Common garden data)####
#Check tea decay rates for Common Garden

Daily_Tea <- tea[grep("DT", tea$Plot), ] #Daily tea only
Common_garden <-
  tea[grep("CG", tea$Plot), ] #All common garden ambient plots
two_yr_Kluane <- filter(tea, Site == "Kluane Plateau")
two_yr_Common_Garden <- filter(
  tea,
  Plot == "CG_HT_year" |
    Plot == "CG_HT_year" |
    Plot == "CG_2Y" |
    Plot == "CG_Y1_3m" |
    Plot == "CG_Y2_3m"
)

#Take mean of daily tea
Daily_Tea_mean <- Daily_Tea %>%
  group_by(Days, Tea_Type) %>%
  summarise(Loss = mean(Loss))

summary(lm(log((1 - Loss) * 100) ~ Days, Daily_Tea_mean[Daily_Tea_mean$Tea_Type ==
                                                          "Green", ]))
summary(lm(log((1 - Loss) * 100) ~ Days, Daily_Tea_mean[Daily_Tea_mean$Tea_Type ==
                                                          "Rooibos", ]))

(
  daily <-
    ggplot(Daily_Tea_mean, aes(Days, (1 - Loss) * 100, colour = factor(Tea_Type))) +
    geom_point() +
    geom_smooth(
      method = "lm",
      formula = (y ~ (log(x))),
      aes(fill = Tea_Type)
    ) +
    theme_classic() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    scale_colour_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    labs(y = "Mass remaining (%)", x = "Days since burial") +
    scale_x_continuous(breaks = seq(0, 50, 10)) +
    theme(legend.position = "none") +
    ggtitle("Kluane (warm site):\nTwo months")
)

# #Create figure
# pdf(file = "users/hthomas/tea/figures/Incubation_2_month.pdf",
#     width = 4,
#     height = 3)
# daily
# dev.off()

#Kluane
m1 <-
  lmer((1 - Loss) * 100 ~ (log(Days)) + (1 |
                                           Plot), data = two_yr_Kluane[two_yr_Kluane$Tea_Type == "Green", ])
ef_g <-
  as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(
    min(two_yr_Kluane$Days), max(two_yr_Kluane$Days), 1
  ))))

m2 <-
  lmer((1 - Loss) * 100 ~ (log(Days)) + (1 |
                                           Plot), data = two_yr_Kluane[two_yr_Kluane$Tea_Type == "Rooibos", ])
ef_r <-
  as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(
    min(two_yr_Kluane$Days), max(two_yr_Kluane$Days), 1
  ))))

(
  Kluane <- ggplot(two_yr_Kluane) +
    geom_point(aes(Days, (1 - Loss) * 100, colour = factor(Tea_Type))) +
    geom_ribbon(
      data = ef_g,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#15BB15",
      alpha = 0.5
    ) +
    geom_ribbon(
      data = ef_r,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#9A0C0C",
      alpha = 0.5
    ) +
    geom_line(
      data = ef_g,
      mapping = aes(x = Days, y = fit),
      colour = "#15BB15",
      lwd = 1.25
    ) +
    geom_line(
      data = ef_r,
      mapping = aes(x = Days, y = fit),
      colour = "#9A0C0C",
      lwd = 1.25
    ) +
    scale_colour_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = "Mass remaining (%)", x = "Days since burial") +
    ggtitle("Kluane (cold site):\nTwo years")
)

#Common Garden
m1 <-
  lmer((1 - Loss) * 100 ~ (log(Days)) + (1 |
                                           Plot), data = two_yr_Common_Garden[two_yr_Common_Garden$Tea_Type == "Green", ])
ef_g <-
  as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(
    min(two_yr_Common_Garden$Days),
    max(two_yr_Common_Garden$Days),
    1
  ))))

m2 <-
  lmer((1 - Loss) * 100 ~ (log(Days)) + (1 |
                                           Plot), data = two_yr_Common_Garden[two_yr_Common_Garden$Tea_Type == "Rooibos", ])
ef_r <-
  as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(
    min(two_yr_Common_Garden$Days),
    max(two_yr_Common_Garden$Days),
    1
  ))))

(
  Common <- ggplot(two_yr_Common_Garden) +
    geom_point(aes(Days, (1 - Loss) * 100, colour = factor(Tea_Type))) +
    geom_ribbon(
      data = ef_g,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#15BB15",
      alpha = 0.5
    ) +
    geom_ribbon(
      data = ef_r,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#9A0C0C",
      alpha = 0.5
    ) +
    geom_line(
      data = ef_g,
      mapping = aes(x = Days, y = fit),
      colour = "#15BB15",
      lwd = 1.25
    ) +
    geom_line(
      data = ef_r,
      mapping = aes(x = Days, y = fit),
      colour = "#9A0C0C",
      lwd = 1.25
    ) +
    scale_colour_manual(values = c("#15BB15", "#9A0C0C"), name = "Tea Type") +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = "Mass remaining (%)", x = "Days since burial") +
    ggtitle("Kluane (warm site):\nTwo years")
)

pdf(file = "users/hthomas/tea/figures/Incubation_duration.pdf",
    width = 8,
    height = 3)
grid.arrange(daily, Common, Kluane, ncol = 3)
dev.off()

#Clean data####
#Set minimum of three weeks incubation (i.e. first few DTs)
tea <- tea[tea$Days > 21, ]

#Calculate mass loss per day
tea$Loss_Day <- tea$Loss / tea$Days

#NOT APPROPIATE - you need multiple time points!!

#Shift points that models will think are in the sea
# tea[tea$Site=="Storfjord, Lofoten Island",]$Lon<-15.75397
# tea$latlon<-paste(tea$Lat,"_",tea$Lon,sep="")

#Check meta data####

length(unique(tea$Region)) #38 regions
length(unique(tea$Site)) #361 sites (unique coordinates)
length(unique(tea$latlon)) #340 unique coordinates (unique coordinates)

#Make sure plots aren't replicated across incubation lengths
tea$Plot_unique <- paste(tea$Plot, tea$Season, sep = "_")

length(unique(tea$Plot_unique)) #648 unique plots (unique coordinates - 649 but includes NA)

#Remove pentlands (boo)
tea <- subset(tea, Site != "Pentlands")

#Add tea bag index variables####

#Tea bag hydrolisable fractions:
Hg <- 0.842
Hr <- 0.552

#Note that I have code to group tea in multiple ways (pairwise, by plot, by site etc),
#but here I am grouping by plot

plot_means <- tea %>%
  dplyr::group_by(Site, Plot_unique, Tea_Type, Days) %>%
  dplyr::select(Tea_ID, Site, Plot_unique, Days, Tea_Type, Loss) %>%
  dplyr::summarise(plot_mean = mean(Loss)) %>%
  tidyr::spread(Tea_Type, plot_mean) %>%
  arrange(Site) %>%
  ungroup()

#Calculate TBI values
plot_means$S <- 1 - (plot_means$Green / Hg)
plot_means$ar <- Hr * (1 - plot_means$S)
plot_means$k <-
  log(plot_means$ar / ((1 - plot_means$Rooibos) - (1 - plot_means$ar))) /
  plot_means$Days

#Add data to plot_means
plot_means$Region <-
  tea$Region[match(plot_means$Plot_unique, tea$Plot_unique)] #Add region
plot_means$Season <-
  tea$Season[match(plot_means$Plot_unique, tea$Plot_unique)] #Add season

#Examine realtionship between S & K#
str(plot_means)

ggplot(plot_means, aes(S, k)) +
  geom_point(aes(colour = factor(Season)), alpha = 0.5) +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  ggtitle("S vs k - All time periods")

ggplot(plot_means[plot_means$Season == "Summer", ], aes(S, k)) +
  geom_point(aes(colour = factor(Region)), alpha = 0.5) +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  ggtitle("S vs k - Summer")

#Add back to tea#
tea$TBI_S <-
  plot_means$S[match(tea$Plot_unique, plot_means$Plot_unique)]
tea$TBI_k <-
  plot_means$k[match(tea$Plot_unique, plot_means$Plot_unique)]

#Add climatic information####

#1) Field measured variables ####
##NB - these could be updated with 2017 values
#Add plot-measured variables####
Plot_Variables_Soil <-
  read.csv(file = "users/hthomas/tea/data/Env.Vars/Soil_Temps.csv")
Plot_Variables_Air <-
  read.csv(file = "users/hthomas/tea/data/Env.Vars/Air_Temps.csv")
Plot_Variables_Moisture <-
  read.csv(file = "users/hthomas/tea/data/Env.Vars/Moisture.csv")

#Soil
tea$soiltemp_mean <-
  Plot_Variables_Soil$Mean_Temperature[match(tea$Plot, Plot_Variables_Soil$Plot)]
tea$soiltemp_GDD5 <-
  Plot_Variables_Soil$GDD_5[match(tea$Plot, Plot_Variables_Soil$Plot)]
tea$soiltemp_source <-
  Plot_Variables_Soil$source[match(tea$Plot, Plot_Variables_Soil$Plot)]

#Air
tea$airtemp_mean <-
  Plot_Variables_Air$Mean_Temperature[match(tea$Plot, Plot_Variables_Air$Plot)]
tea$airtemp_GDD5 <-
  Plot_Variables_Air$GDD_5[match(tea$Plot, Plot_Variables_Air$Plot)]
tea$airtemp_source <-
  Plot_Variables_Air$source[match(tea$Plot, Plot_Variables_Air$Plot)]

#Moisture
tea$moisture_mean <-
  Plot_Variables_Moisture$Mean_moisture[match(tea$Plot, Plot_Variables_Moisture$Plot)]
tea$moisture_mean_source <-
  Plot_Variables_Moisture$source[match(tea$Plot, Plot_Variables_Moisture$Plot)]

moisture <- subset(tea, !is.na(moisture_mean))
#Convert HOBO to %

#NB - could decide to exclude all (online) weather station data at this point#

#2) CHELSA data ####
chelsa_year <-
  raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_1.tif")
chelsa_summer <-
  raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_10.tif")
chelsa_summer_tundra <-
  raster(
    "/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_temp_arctic_10corrected.tif"
  )
chelsa_winter <-
  raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_11.tif")
chelsa_precip_year <-
  raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_12.tif")
chelsa_precip_summer <-
  raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_18.tif")
chelsa_precip_summer_tundra <-
  raster(
    "/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_precip_arctic_10corrected.tif"
  )
chelsa_precip_winter <-
  raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_19.tif")

#NB - May also want to create composite rasters - e.g. JJA, or !JJA / Spring

tea_coords <- cbind(tea$Lon, tea$Lat)

tea$CHELSA_year_temp <- extract(chelsa_year, tea_coords) / 10
tea$CHELSA_summer_temp <- extract(chelsa_summer, tea_coords) / 10
tea$CHELSA_winter_temp <- extract(chelsa_winter, tea_coords) / 10
tea$CHELSA_year_precip <- extract(chelsa_precip_year, tea_coords) / 10
tea$CHELSA_summer_precip <-
  extract(chelsa_precip_summer, tea_coords) / 10
tea$CHELSA_winter_precip <-
  extract(chelsa_precip_winter, tea_coords) / 10

#3) Soil moisture data
ESA_summer_NH <-
  raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_JJA.tif")
ESA_summer_SH <-
  raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_DJF_SH.tif")

ESA_summer <- merge(ESA_summer_NH, ESA_summer_SH)

ESA_winter_NH <-
  raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_JJA.tif")
ESA_winter_SH <-
  raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_DJF_SH.tif")
ESA_winter <- merge(ESA_winter_NH, ESA_winter_SH)

ESA_year <-
  raster("scripts/users/hthomas/ESA_Soil_Moisture/1978_2015_mean.tif")


#Add to tea object
tea$ESA_moisture <- extract(ESA_summer, tea_coords) * 100
tea$ESA_moisture_winter <- extract(ESA_winter, tea_coords) * 100
tea$ESA_moisture_year <- extract(ESA_year, tea_coords) * 100

#Add hierarchy###################-------------------------------------

#Decomposition sites####

#Region = ESA cell#
tea$ESA_cell <- extract(ESA_summer, tea_coords, cellnumbers = TRUE)[, 1]
#ESA cell_season
tea$ESA_cell_season <- paste(tea$ESA_cell, tea$Season, sep = "_")

#Add plots and sites within region
nsite <- tea %>%
  group_by(ESA_cell_season) %>%
  summarise(Unique_Sites = n_distinct(Site),
            Unique_Plots = n_distinct(Plot))

nsite$has.sites <- ifelse(nsite$Unique_Sites <= 1, 0, 1)
nsite$has.plots <- ifelse(nsite$Unique_Plots <= 1, 0, 1)

#Add plots within site
nsite.plot <- tea %>%
  group_by(ESA_cell_season, Site) %>%
  summarise(Unique_Site_Plot = n_distinct(Plot))

nsite.plot$has.site.plots <-
  ifelse(nsite.plot$Unique_Site_Plot <= 1, 0, 1)

#Merge object
nsite$has.site.plots <-
  nsite.plot$has.site.plots[match(nsite$ESA_cell_season, nsite.plot$ESA_cell_season)]

#Define four levels
#1 - region only (plot = site = region)
nsite$is.region <-
  ifelse(nsite$has.sites == 0 & nsite$has.plots == 0, 1, 0)

#2 - site only (plot = site, multiple sites in region)
nsite$is.site <-
  ifelse(nsite$has.sites == 1 & nsite$has.plots == 0, 1, 0)

#3 - plot only (multiple plots in site, site = region)
nsite$is.plot <-
  ifelse(nsite$has.sites == 0 & nsite$has.plots == 1, 1, 0)

#4 - site_plot (plot within site within region)
nsite$is.site.plot <-
  ifelse(nsite$has.sites == 1 & nsite$has.plots == 1, 1, 0)

#Check - should all add up to one
nsite$check <-
  nsite$is.region + nsite$is.site + nsite$is.plot + nsite$is.site.plot
max(nsite$check)
min(nsite$check)

#Add back to tea
tea <- merge(tea, nsite[, c(1, 7:10)], by = "ESA_cell_season")

#Environmental variables#######-------------------------------------------

env.list <-
  c(
    "airtemp_mean",
    "soiltemp_mean",
    "moisture_mean",
    "CHELSA_summer_temp",
    "CHELSA_summer_precip",
    "ESA_moisture"
  )

for (i in 1:6) {
  var = env.list[i]
  
  #Are variables unique to site
  nsite <- tea %>%
    group_by(ESA_cell_season) %>%
    summarise(Unique_site = n_distinct(get(var)))
  
  nsite$has.site <- ifelse(nsite$Unique_site <= 1, 0, 1)
  
  #Are variables unique to plot
  nplot <- tea %>%
    group_by(ESA_cell_season, Site) %>%
    summarise(Unique_plot = n_distinct(get(var)))
  
  nplot$has.plot <- ifelse(nplot$Unique_plot <= 1, 0, 1)
  
  #Merge object
  nsite$has.plot <-
    nplot$has.plot[match(nsite$ESA_cell_season, nplot$ESA_cell_season)]
  
  #Sum - if has site and plot = 2, if just site = 1, if just region = 0
  nsite$sum <- nsite$has.plot + nsite$has.site
  
  #Add category
  nsite$cat <-
    ifelse(nsite$sum == 2, "Plot", ifelse(nsite$sum == 1, "Site", "Region"))
  names(nsite)[6] <- paste0(var, "_var_level")
  
  tea <- merge(tea, nsite[, c(1, 6)], by = "ESA_cell_season")
}

#Write output csv

#write.csv(tea,"scripts/users/hthomas/tea/combined_tea.csv")

tea <- read.csv("data/combined_tea.csv",encoding = "UTF-8")

tea_stats <- tea %>%
  group_by(Region, Site) %>%
  summarise(
    tea = length(Loss),
    ESA_cell = length(unique(ESA_cell)),
    Plot = length(unique(Plot))
  )

tea$Region <- as.character(tea$Region)

tea[tea$Contributor == "Jonathan von Oppen, Sonja Wipf" |
      tea$Contributor == "Christian Rixen, Janet Prevey", ]$Region <-
  "Swiss Alps"

tea_stats3 <- tea %>%
  group_by(Region) %>%
  summarise(
    sites = length(unique(Site)),
    Plot = length(unique(Plot)),
    tea = length(Loss),
    MAT = mean(CHELSA_year_temp),
    MST = mean(CHELSA_summer_temp),
    MWT = mean(CHELSA_winter_temp),
    MAP = mean(CHELSA_year_precip),
    MSP = mean(CHELSA_summer_precip),
    MWP = mean(CHELSA_winter_precip),
    MAM = mean(ESA_moisture_year),
    MSM = mean(ESA_moisture),
    MWM = mean(ESA_moisture_winter)
  )

sum(tea_stats$tea)

unique(tea_stats$Region)

min(tea$CHELSA_year_temp, na.rm = T)

tea[3251, ]

#Tea comparison figure####
litter_Haydn <-
  read.csv("data/Litterbags_2017.csv")
litter_Haydn$Species


Common_garden2 <- tea[grep("CG", tea$Plot), ]
litterbed <- subset(tea, Plot == "DC_HT_1yr")
litterbed$Type <- "Surface"
Common_garden2$Type <- "Soil"
tea_CG <- rbind(Common_garden2, litterbed)

tea_CG2 <- tea_CG[grep("DT", tea_CG$Plot), ] #Daily tea only

tea_CG2$Plot <- as.character(tea_CG2$Plot)
tea_CG$Plot <- as.character(tea_CG$Plot)
tea_CG <- subset(tea_CG, Plot %notin% tea_CG2$Plot)

pdf(
  "scripts/users/hthomas/Output_Images/Tea/Final/SF_Surface.pdf",
  height = 3,
  width = 5
)
ggplot(tea_CG[tea_CG$Season == "Year", ]) +
  geom_boxplot(aes(Tea_Type, Loss * 100, fill = factor(Type))) +
  theme_classic() +
  scale_fill_manual(
    values = c("grey40", "white"),
    labels = c("Buried (5cm depth)", "Surface"),
    name = ""
  ) +
  theme(legend.justification = "top") +
  labs(x = "Tea type", y = "Mass loss (%)") +
  ylim(0, 100)
dev.off()





##############Relationships############################

#Make sure only using control plots
ambient <- subset(tea, Treatment == "None")

#Split into seasons to make things easier

summer <- subset(ambient, Season == "Summer")
year <- subset(ambient, Season == "Year")
winter <- subset(ambient, Season == "Winter")

ggplot(summer, aes(soiltemp_mean, k)) +
  geom_point()
#Classic density plots####

all <-
  ggplot(tea, aes(
    x = Loss * 100,
    y = ..density..,
    fill = factor(Tea_Type)
  )) +
  geom_histogram(alpha = 0.8, position = 'identity') +
  geom_density(alpha = 0.25, adjust = 3) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("darkgreen", "red"), name = "Tea Type") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(y = "Density", x = "% Mass Loss") +
  ggtitle("All incubations")

ambs <-
  ggplot(ambient, aes(
    x = Loss * 100,
    y = ..density..,
    fill = factor(Tea_Type)
  )) +
  geom_histogram(alpha = 0.8, position = 'identity') +
  geom_density(alpha = 0.25, adjust = 3) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("darkgreen", "red"), name = "Tea Type") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(y = "Density", x = "% Mass Loss") +
  ggtitle("Ambient incubations")

summer_dens <-
  ggplot(summer, aes(
    x = Loss * 100,
    y = ..density..,
    fill = factor(Tea_Type)
  )) +
  geom_histogram(alpha = 0.8, position = 'identity') +
  geom_density(alpha = 0.25, adjust = 3) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("darkgreen", "red"), name = "Tea Type") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(y = "Density", x = "% Mass Loss") +
  ggtitle("Summer incubations")

grid.arrange(all, ambs, summer_dens)

pdf(file = "scripts/users/hthomas/Output_Images/Tea/tea_dists.pdf",
    width = 3,
    height = 3)
ggplot(tea, aes(
  x = Loss * 100,
  y = ..density.. * 100,
  fill = factor(Tea_Type)
)) +
  geom_histogram(alpha = 0.8, position = 'identity') +
  geom_density(alpha = 0.25, adjust = 3) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("darkgreen", "red"), name = "Tea Type") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(y = "Proportion of teabags (%)", x = "Mass Loss (%)") +
  theme(legend.position = "none")
dev.off()

#Do teas correlate?####

tea_grouped_plots <- summer %>%
  group_by(Site, Plot, Tea_Type, Region) %>%
  summarise(Loss = mean(Loss))

tea_grouped_red <- filter(tea_grouped_plots, Tea_Type == "Rooibos")
names(tea_grouped_red)[5] <- "Red"
tea_grouped_green <- filter(tea_grouped_plots, Tea_Type == "Green")
names(tea_grouped_green)[5] <- "Green"

tea_grouped_green$Red <-
  tea_grouped_red$Red[match(tea_grouped_green$Plot, tea_grouped_red$Plot)]

pdf(file = "scripts/users/hthomas/Output_Images/Tea/tea_correlations.pdf",
    width = 8,
    height = 5)
ggplot(tea_grouped_green, aes(Red, Green)) +
  geom_point(aes(colour = Region)) +
  stat_smooth(method = "lm") +
  stat_smooth(aes(colour = Region), method = "lm", se = FALSE) +
  theme_bw() +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(y = "Green Tea", x = "Rooibos Tea") +
  ggtitle("Rooibos vs green tea decomposition")
dev.off()

#Relationships: 1) Air Temp, 2) Soil Temp, 3) Soil Moisture, 4) CHELSA Temp5) CHELSA Precip, 6) ESA Moisture####

#Seasons: i) Summer, ii) Winter, iii) Year

#Set season
season.list <- c("summer", "winter", "year")

#VARIABLES: Loss, Loss_Day, k, TBI_k, TBI_S####

#Set Variable
var.list <- c("Loss", "Loss_Day", "k", "TBI_k", "TBI_S")
plots <- list()
library(MCMCglmm)
library(lme4)
library(broom.mixed)
for (i in 1:5) {
  i = 1
  #Get column number
  var.num <- which(colnames(summer) == var.list[i])
  
  #1) AIR TEMP#################################################
  
  #Remove na values from season object
  season_narm <- get(season.list[1]) %>%
    filter(is.finite(get(season.list[1])[, var.num]), is.finite(airtemp_mean))
  
  #Define interest variable
  names(season_narm)[var.num] <- "var"
  
  #MCMC model
  MC_LM_air <-
    MCMCglmm(
      var ~ Tea_Type + airtemp_mean + Days,
      random =  ~ Region + Region:Site,
      data = season_narm,
      family = "gaussian",
      pr = TRUE,
      nitt = 10000,
      burnin = 1000
    ) #Run
  summary(MC_LM_air) #Summarise
  
  #Calculate mean burial length
  mean_burial <- mean(season_narm$Days)
  min_air <- min(season_narm$airtemp_mean, na.rm = TRUE)
  max_air <- max(season_narm$airtemp_mean, na.rm = TRUE)
  
  #Run for green
  MC_ML_air_g <-
    MCMCglmm(
      var ~ airtemp_mean + Days,
      random =  ~ Site ,
      data = season_narm[season_narm$Tea_Type == "Green", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_air_g)
  
  #Model preditions
  mm.green <-
    expand.grid(
      airtemp_mean = seq(min_air, max_air, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ airtemp_mean + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <-
    mm %*% fixef(MC_ML_air_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_air_g)$solutions[, 2]
  mm.green$phi <- mm %*% summary(MC_ML_air_g)$solutions[, 3]
  
  #Run for rooibos
  MC_ML_air_r <-
    MCMCglmm(
      var ~ airtemp_mean + Days,
      random =  ~ Site ,
      data = season_narm[season_narm$Tea_Type == "Rooibos", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_air_r)
  
  #Model preditions
  mm.red <-
    expand.grid(
      airtemp_mean = seq(min_air, max_air, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ airtemp_mean + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <-
    mm %*% fixef(MC_ML_air_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_air_r)$solutions[, 2]
  mm.red$phi <- mm %*% summary(MC_ML_air_r)$solutions[, 3]
  
  #Create dataframe for plotting so ggplot doesn't overwrite
  plotting_data <- get(season.list[1]) #copy dataset
  names(plotting_data)[var.num] <- "var" #rename variable of interest
  
  # Graph
  #When recreating object with loop, use assign: assign(paste(season, tea variable, output))
  (
    sum_air <- ggplot() +
      #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(airtemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
      #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(airtemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
      #geom_ribbon(data = mm.red, mapping = aes(x = airtemp_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = airtemp_mean, y=var*100), colour="red3", size=1) +
      geom_ribbon(
        data = mm.green,
        mapping = aes(
          x = airtemp_mean,
          ymin = plo * 100,
          ymax = phi * 100
        ),
        fill = "darkgreen",
        alpha = 0.3
      ) +
      geom_line(
        data = mm.green,
        mapping = aes(x = airtemp_mean, y = var * 100),
        colour = "darkgreen",
        size = 1
      ) +
      geom_point(
        data = plotting_data[plotting_data$Tea_Type == "Green", ],
        aes(airtemp_mean, var * 100),
        colour = "green3",
        alpha = 0.3,
        pch = 16
      ) +
      #geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(airtemp_mean,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_colour_manual(values = c("darkgreen", "red"), name = "Tea Type") +
      labs(y = "Stabilisation Factor (S-TBI)", x = "Air Temeprature °C") +
      ggtitle("Measured Air Temperature")
  )
  
  
  
  #b) SOIL TEMP ONLY#################################################
  
  #Remove na values from season object
  season_narm <- get(season.list[1]) %>%
    filter(is.finite(get(season.list[1])[, var.num]), is.finite(soiltemp_mean))
  
  #Define interest variable
  names(season_narm)[var.num] <- "var"
  
  #Create model
  MC_LM_soil <-
    MCMCglmm(
      var ~ Tea_Type + soiltemp_mean + Days,
      random = ~ Region + Region:Site,
      data = season_narm,
      family = "gaussian",
      pr = TRUE,
      nitt = 10000,
      burnin = 1000
    ) #Run
  summary(MC_LM_soil) #Summarise
  
  #Calculate mean burial length
  mean_burial <- mean(season_narm$Days)
  min_soil <- min(season_narm$soiltemp_mean, na.rm = TRUE)
  max_soil <- max(season_narm$soiltemp_mean, na.rm = TRUE)
  
  #Run for green
  MC_ML_soil_g <-
    MCMCglmm(
      var ~ soiltemp_mean + Days,
      random =  ~ Site,
      data = season_narm[season_narm$Tea_Type == "Green", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_soil_g)
  
  #Model preditions
  mm.green <-
    expand.grid(
      soiltemp_mean = seq(min_soil, max_soil, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ soiltemp_mean + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <-
    mm %*% fixef(MC_ML_soil_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_soil_g)$solutions[, 2]
  mm.green$phi <- mm %*% summary(MC_ML_soil_g)$solutions[, 3]
  
  #Run for rooibos
  MC_ML_soil_r <-
    MCMCglmm(
      var ~ soiltemp_mean + Days,
      random =  ~ Site,
      data = season_narm[season_narm$Tea_Type == "Rooibos", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_soil_r)
  
  #Model preditions
  mm.red <-
    expand.grid(
      soiltemp_mean = seq(min_soil, max_soil, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ soiltemp_mean + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <-
    mm %*% fixef(MC_ML_soil_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_soil_r)$solutions[, 2]
  mm.red$phi <- mm %*% summary(MC_ML_soil_r)$solutions[, 3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/soil_temp_MCMC.pdf", width = 3, height = 3)
  (
    sum_soil_t <- ggplot() +
      #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(soiltemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
      #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(soiltemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
      #geom_ribbon(data = mm.red, mapping = aes(x = soiltemp_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = soiltemp_mean, y=var*100), colour="red3", size=1) +
      geom_ribbon(
        data = mm.green,
        mapping = aes(
          x = soiltemp_mean,
          ymin = plo * 100,
          ymax = phi * 100
        ),
        fill = "darkgreen",
        alpha = 0.3
      ) +
      geom_line(
        data = mm.green,
        mapping = aes(x = soiltemp_mean, y = var * 100),
        colour = "darkgreen",
        size = 1
      ) +
      geom_point(
        data = plotting_data[plotting_data$Tea_Type == "Green", ],
        aes(soiltemp_mean, var * 100),
        colour = "green3",
        alpha = 0.3,
        pch = 16
      ) +
      #geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(soiltemp_mean,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_colour_manual(values = c("darkgreen", "red"), name = "Tea Type") +
      labs(y = "Stabilisation Factor (S-TBI)", x = "Soil Temeprature °C") +
      ggtitle("Measured Soil Temperature")
  )
  #dev.off()
  
  assign(paste("soil_t_fig", var.list[i], season.list[1], sep = "_"),
         sum_soil_t)
  
  
  #c) SOIL MOISTURE ONLY###################################################
  
  #Remove na values from season object
  season_narm <- get(season.list[1]) %>%
    filter(is.finite(get(season.list[1])[, var.num]), is.finite(moisture_mean))
  
  #Define interest variable
  names(season_narm)[var.num] <- "var"
  
  #Create model
  MC_LM_moisture <-
    MCMCglmm(
      var ~ Tea_Type + moisture_mean + Days,
      random = ~ Region + Region:Site,
      data = season_narm,
      family = "gaussian",
      pr = TRUE,
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_LM_moisture)
  
  #Calculate mean burial length
  mean_burial <- mean(season_narm$Days)
  min_moisture <- min(season_narm$moisture_mean, na.rm = TRUE)
  max_moisture <- max(season_narm$moisture_mean, na.rm = TRUE)
  
  #Run for green
  MC_ML_moisture_g <-
    MCMCglmm(
      var ~ moisture_mean + Days,
      random =  ~ Site ,
      data = season_narm[season_narm$Tea_Type == "Green", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_moisture_g)
  
  #Model preditions
  mm.green <-
    expand.grid(
      moisture_mean = seq(min_moisture, max_moisture, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ moisture_mean + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <-
    mm %*% fixef(MC_ML_moisture_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_moisture_g)$solutions[, 2]
  mm.green$phi <- mm %*% summary(MC_ML_moisture_g)$solutions[, 3]
  
  #Run for rooibos
  MC_ML_moisture_r <-
    MCMCglmm(
      var ~ moisture_mean + Days,
      random =  ~ Site ,
      data = season_narm[season_narm$Tea_Type == "Rooibos", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_moisture_r)
  
  #Model preditions
  mm.red <-
    expand.grid(
      moisture_mean = seq(min_moisture, max_moisture, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ moisture_mean + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <-
    mm %*% fixef(MC_ML_moisture_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_moisture_r)$solutions[, 2]
  mm.red$phi <- mm %*% summary(MC_ML_moisture_r)$solutions[, 3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (
    sum_soil_m <- ggplot() +
      #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
      #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
      #geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = moisture_mean, y=var*100), colour="red3", size=1) +
      geom_ribbon(
        data = mm.green,
        mapping = aes(
          x = moisture_mean,
          ymin = plo * 100,
          ymax = phi * 100
        ),
        fill = "darkgreen",
        alpha = 0.3
      ) +
      geom_line(
        data = mm.green,
        mapping = aes(x = moisture_mean, y = var * 100),
        colour = "darkgreen",
        size = 1
      ) +
      geom_point(
        data = plotting_data[plotting_data$Tea_Type == "Green", ],
        aes(moisture_mean, var * 100),
        colour = "green3",
        alpha = 0.3,
        pch = 16
      ) +
      #geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(moisture_mean,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_colour_manual(values = c("darkgreen", "red"), name = "Tea Type") +
      labs(y = "Stabilisation Factor (S-TBI)", x = "Soil Moisture Content (%)") +
      ggtitle("Measured Soil Moisture")
  )
  #dev.off()
  
  assign(paste("soil_m_fig", var.list[i], season.list[1], sep = "_"),
         sum_soil_m)
  
  #d) CHELSA AIR###################################################
  
  #Remove na values from season object
  season_narm <- get(season.list[1]) %>%
    filter(is.finite(get(season.list[1])[, var.num]))
  
  #Define interest variable
  names(season_narm)[var.num] <- "var"
  
  #Create model
  MC_LM_CHELSA_temp <-
    MCMCglmm(
      var ~ Tea_Type + CHELSA_summer_temp  + Days,
      random = ~ Site,
      data = season_narm,
      family = "gaussian",
      pr = TRUE,
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_LM_CHELSA_temp)
  
  #Calculate mean burial length
  mean_burial <- mean(season_narm$Days)
  min_CH_temp <- min(season_narm$CHELSA_summer_temp, na.rm = TRUE)
  max_CH_temp <- max(season_narm$CHELSA_summer_temp, na.rm = TRUE)
  
  #Run for green
  MC_ML_CH_T_g <-
    MCMCglmm(
      var ~ CHELSA_summer_temp  + Days,
      random =  ~ Site ,
      data = season_narm[season_narm$Tea_Type == "Green", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_CH_T_g)
  
  #Model preditions
  mm.green <-
    expand.grid(
      CHELSA_summer_temp = seq(min_CH_temp, max_CH_temp, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ CHELSA_summer_temp + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <-
    mm %*% fixef(MC_ML_CH_T_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_CH_T_g)$solutions[, 2]
  mm.green$phi <- mm %*% summary(MC_ML_CH_T_g)$solutions[, 3]
  
  #Run for rooibos
  MC_ML_CH_T_r <-
    MCMCglmm(
      var ~ CHELSA_summer_temp  + Days,
      random =  ~ Site,
      data = season_narm[season_narm$Tea_Type == "Rooibos", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_CH_T_r)
  
  #Model preditions
  mm.red <-
    expand.grid(
      CHELSA_summer_temp = seq(min_CH_temp, max_CH_temp, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ CHELSA_summer_temp + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <-
    mm %*% fixef(MC_ML_CH_T_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_CH_T_r)$solutions[, 2]
  mm.red$phi <- mm %*% summary(MC_ML_CH_T_r)$solutions[, 3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (
    sum_CH_T <- ggplot() +
      #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
      #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
      #geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_summer_temp, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = CHELSA_summer_temp, y=var*100), colour="red3", size=1) +
      geom_ribbon(
        data = mm.green,
        mapping = aes(
          x = CHELSA_summer_temp,
          ymin = plo * 100,
          ymax = phi * 100
        ),
        fill = "darkgreen",
        alpha = 0.3
      ) +
      geom_line(
        data = mm.green,
        mapping = aes(x = CHELSA_summer_temp, y = var * 100),
        colour = "darkgreen",
        size = 1
      ) +
      geom_point(
        data = plotting_data[plotting_data$Tea_Type == "Green", ],
        aes(CHELSA_summer_temp, var * 100),
        colour = "green3",
        alpha = 0.3,
        pch = 16
      ) +
      #geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(CHELSA_summer_temp,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_colour_manual(values = c("darkgreen", "red"), name = "Tea Type") +
      labs(y = "Stabilisation Factor (S-TBI)", x = "CHELSA Summer Temperature (°C)") +
      ggtitle("CHELSA Summer Temperature")
  )
  #dev.off()
  
  assign(paste("CH_t_fig", var.list[i], season.list[1], sep = "_"),
         sum_CH_T)
  
  #e) CHELSA PRECIP###################################################
  
  #Remove na values from season object
  season_narm <- get(season.list[1]) %>%
    filter(is.finite(get(season.list[1])[, var.num]))
  
  #Define interest variable
  names(season_narm)[var.num] <- "var"
  
  #Create model
  MC_LM_CHELSA_precip <-
    MCMCglmm(
      var ~ Tea_Type + CHELSA_summer_precip  + Days,
      random = ~ Site,
      data = season_narm,
      family = "gaussian",
      pr = TRUE,
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_LM_CHELSA_precip)
  
  #Calculate mean burial length
  mean_burial <- mean(season_narm$Days)
  min_CH_precip <- min(season_narm$CHELSA_summer_precip, na.rm = TRUE)
  max_CH_precip <- max(season_narm$CHELSA_summer_precip, na.rm = TRUE)
  
  #Run for green
  MC_ML_CH_P_g <-
    MCMCglmm(
      var ~ CHELSA_summer_precip  + Days,
      random =  ~ Site,
      data = season_narm[season_narm$Tea_Type == "Green", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_CH_P_g)
  
  #Model preditions
  mm.green <-
    expand.grid(
      CHELSA_summer_precip = seq(min_CH_precip, max_CH_precip, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ CHELSA_summer_precip + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <-
    mm %*% fixef(MC_ML_CH_P_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_CH_P_g)$solutions[, 2]
  mm.green$phi <- mm %*% summary(MC_ML_CH_P_g)$solutions[, 3]
  
  #Run for rooibos
  MC_ML_CH_P_r <-
    MCMCglmm(
      var ~ CHELSA_summer_precip  + Days,
      random =  ~ Site,
      data = season_narm[season_narm$Tea_Type == "Rooibos", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_CH_P_r)
  
  #Model preditions
  mm.red <-
    expand.grid(
      CHELSA_summer_precip = seq(min_CH_precip, max_CH_precip, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ CHELSA_summer_precip + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <-
    mm %*% fixef(MC_ML_CH_P_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_CH_P_r)$solutions[, 2]
  mm.red$phi <- mm %*% summary(MC_ML_CH_P_r)$solutions[, 3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (
    sum_CH_P <- ggplot() +
      #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
      #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
      #geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_summer_precip, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = CHELSA_summer_precip, y=var*100), colour="red3", size=1) +
      geom_ribbon(
        data = mm.green,
        mapping = aes(
          x = CHELSA_summer_precip,
          ymin = plo * 100,
          ymax = phi * 100
        ),
        fill = "darkgreen",
        alpha = 0.3
      ) +
      geom_line(
        data = mm.green,
        mapping = aes(x = CHELSA_summer_precip, y = var * 100),
        colour = "darkgreen",
        size = 1
      ) +
      geom_point(
        data = plotting_data[plotting_data$Tea_Type == "Green", ],
        aes(CHELSA_summer_precip, var * 100),
        colour = "green3",
        alpha = 0.3,
        pch = 16
      ) +
      #geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(CHELSA_summer_precip,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_colour_manual(values = c("darkgreen", "red"), name = "Tea Type") +
      labs(y = "Stabilisation Factor (S-TBI)", x = "CHELSA Summer Precipitation (mm)") +
      ggtitle("CHELSA Summer Precipitation")
  )
  #dev.off()
  
  assign(paste("CH_p_fig", var.list[i], season.list[1], sep = "_"),
         sum_CH_P)
  
  #f) ESA Moisture###################################################
  
  #Remove na values from season object
  season_narm <- get(season.list[1]) %>%
    filter(is.finite(get(season.list[1])[, var.num]), is.finite(ESA_moisture))
  
  
  #Define interest variable
  names(season_narm)[var.num] <- "var"
  
  #Create model
  MC_LM_ESA_moist <-
    MCMCglmm(
      var ~ Tea_Type + ESA_moisture  + Days,
      random = ~ Site,
      data = season_narm,
      family = "gaussian",
      pr = TRUE,
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_LM_ESA_moist)
  
  #Calculate mean burial length
  mean_burial <- mean(season_narm$Days)
  min_ESA_moist <- min(season_narm$ESA_moisture, na.rm = TRUE)
  max_ESA_moist <- max(season_narm$ESA_moisture, na.rm = TRUE)
  
  #Run for green
  MC_ML_ESA_moist_g <-
    MCMCglmm(
      var ~ ESA_moisture  + Days,
      random =  ~ Site,
      data = season_narm[season_narm$Tea_Type == "Green", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_ESA_moist_g)
  
  #Model preditions
  mm.green <-
    expand.grid(
      ESA_moisture = seq(min_ESA_moist, max_ESA_moist, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ ESA_moisture + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <-
    mm %*% fixef(MC_ML_ESA_moist_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_ESA_moist_g)$solutions[, 2]
  mm.green$phi <- mm %*% summary(MC_ML_ESA_moist_g)$solutions[, 3]
  
  #Run for rooibos
  MC_ML_ESA_moist_r <-
    MCMCglmm(
      var ~ ESA_moisture  + Days,
      random =  ~ Site,
      data = season_narm[season_narm$Tea_Type == "Rooibos", ],
      nitt = 10000,
      burnin = 1000
    )
  summary(MC_ML_ESA_moist_r)
  
  #Model preditions
  mm.red <-
    expand.grid(
      ESA_moisture = seq(min_ESA_moist, max_ESA_moist, 0.1),
      Days = rep(mean_burial),
      var = 0
    )  # Create a blank dataset with the years we want
  mm <-
    model.matrix(terms(var ~ ESA_moisture + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <-
    mm %*% fixef(MC_ML_ESA_moist_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_ESA_moist_r)$solutions[, 2]
  mm.red$phi <- mm %*% summary(MC_ML_ESA_moist_r)$solutions[, 3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (
    sum_ESA_moist <- ggplot() +
      #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
      #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
      #geom_ribbon(data = mm.red, mapping = aes(x = ESA_moisture, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = ESA_moisture, y=var*100), colour="red3", size=1) +
      geom_ribbon(
        data = mm.green,
        mapping = aes(
          x = ESA_moisture,
          ymin = plo * 100,
          ymax = phi * 100
        ),
        fill = "darkgreen",
        alpha = 0.3
      ) +
      geom_line(
        data = mm.green,
        mapping = aes(x = ESA_moisture, y = var * 100),
        colour = "darkgreen",
        size = 1
      ) +
      geom_point(
        data = plotting_data[plotting_data$Tea_Type == "Green", ],
        aes(ESA_moisture, var * 100),
        colour = "green3",
        alpha = 0.3,
        pch = 16
      ) +
      #geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(ESA_moisture,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw() +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_colour_manual(values = c("darkgreen", "red"), name = "Tea Type") +
      labs(y = "Stabilisation Factor (S-TBI)", x = "ESA Soil Moisture (%)") +
      ggtitle("ESA Soil Moisture")
  )
  #dev.off()
  
  assign(paste("ESA_moist_fig", var.list[i], season.list[1], sep = "_"),
         sum_ESA_moist)
  
}

library(gridExtra)

grid.arrange(
  air_fig_Loss_summer,
  soil_t_fig_Loss_summer,
  soil_m_fig_Loss_summer,
  CH_p_fig_Loss_summer,
  CH_t_fig_Loss_summer,
  ESA_moist_fig_Loss_summer
)




#Overall model (Loss - summer)####

summer_narm <- summer %>%
  filter(
    is.finite(Loss),
    is.finite(soiltemp_mean),
    is.finite(moisture_mean),
    is.finite(Days)
  )

Loss_summer <-
  MCMCglmm(
    scale(Loss) ~ Tea_Type + scale(soiltemp_mean) + scale(moisture_mean)  + scale(Days),
    random = ~ Region ,
    data = summer_narm,
    nitt = 10000,
    burnin = 2000
  )
summary(Loss_summer)

#NB - I'm not sure whether it is better to use days as a fixed or random effect

Loss_summer_fixefs <-
  as.data.frame(abs(summary(Loss_summer)$solution[, 1]))
names(Loss_summer_fixefs) <- "Effect_Size"
rownames(Loss_summer_fixefs) <-
  c("Intercept",
    "Tea Type",
    "Soil Temperature",
    "Soil Moisture",
    "Incubation Length")
Loss_summer_fixefs$Variable <- rownames(Loss_summer_fixefs)
Loss_summer_fixefs$lower <- abs(summary(Loss_summer)$solution[, 2])
Loss_summer_fixefs$upper <- abs(summary(Loss_summer)$solution[, 3])

pdf(file = "scripts/users/hthomas/Output_Images/Tea/Effect_sizes.pdf",
    width = 4,
    height = 3)
(
  summer_effects <-
    ggplot(Loss_summer_fixefs[-1, ], aes(Variable, Effect_Size)) +
    geom_errorbar(
      data = Loss_summer_fixefs[-1, ],
      mapping = aes(x = Variable, ymin = upper, ymax = lower),
      linetype = 1,
      width = 0,
      size = 1
    ) +
    coord_flip() +
    ylim(0, 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(y = "Standardised Effect Size")
)
dev.off()


#Overall model (Loss - all)####

ambient_narm <- ambient %>%
  filter(
    is.finite(Loss),
    is.finite(soiltemp_mean),
    is.finite(moisture_mean),
    is.finite(Days)
  )

Loss_year <-
  MCMCglmm(
    scale(Loss) ~ Tea_Type + scale(soiltemp_mean) + scale(moisture_mean)  + scale(Days),
    random = ~ Region + Season ,
    data = ambient_narm,
    nitt = 10000,
    burnin = 2000
  )
summary(Loss_year)

#NB - I'm not sure whether it is better to use days as a fixed or random effect

Loss_year_fixefs <-
  as.data.frame(abs(summary(Loss_year)$solution[, 1]))
names(Loss_year_fixefs) <- "Effect_Size"
rownames(Loss_year_fixefs) <-
  c("Intercept",
    "Tea Type",
    "Soil Temperature",
    "Soil Moisture",
    "Incubation Length")
Loss_year_fixefs$Variable <- rownames(Loss_year_fixefs)
Loss_year_fixefs$lower <- abs(summary(Loss_year)$solution[, 2])
Loss_year_fixefs$upper <- abs(summary(Loss_year)$solution[, 3])


ggplot(Loss_year_fixefs[-1, ], aes(Variable, Effect_Size)) +
  geom_errorbar(
    data = Loss_year_fixefs[-1, ],
    mapping = aes(x = Variable, ymin = upper, ymax = lower),
    linetype = 1,
    width = 0,
    size = 1
  ) +
  coord_flip() +
  ylim(0, 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  ggtitle("Effect Sizes (Summer Mass Loss)") +
  labs(y = "Standardised Effect Size")


#############With CHELSA data##########################################################

#Chelsa summer data vs measured summer data - air####

#Remove NAs
summer_narm <- summer %>%
  filter(is.finite(CHELSA_summer_temp), is.finite(airtemp_mean))

#Find only unique plot temperatures
summer_narm_unique <-
  summer_narm[!duplicated(summer_narm[c("CHELSA_summer_temp", "airtemp_mean")]), ]

#Take extent
min_air <- min(summer_narm_unique$airtemp_mean, na.rm = TRUE)
max_air <- max(summer_narm_unique$airtemp_mean, na.rm = TRUE)

#Run model
lm_air_chelsa_summer <-
  lmer(CHELSA_summer_temp ~ airtemp_mean + (1 |
                                              Region), data = summer_narm_unique)
#lm_air_chelsa_summer<-lm(CHELSA_summer_temp ~ airtemp_mean, data=summer_narm_unique)
summary(lm_air_chelsa_summer)
r.squaredGLMM(lm_air_chelsa_summer)

# Predictions
mm.air <-
  expand.grid(airtemp_mean = seq(min_air, max_air, 0.1),
              CHELSA_summer_temp = 0)  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(lm_air_chelsa_summer), mm.air)  # Create matrix of relevant effect sizes
mm.air$CHELSA_summer_temp <-
  mm %*% fixef(lm_air_chelsa_summer)  # Calculate height based on the relevant effect sizes
pvar.mm.air <-
  diag(mm %*% tcrossprod(vcov(lm_air_chelsa_summer), mm))
mm.air <-
  data.frame(
    mm.air,
    plo = mm.air$CHELSA_summer_temp - 1.96 * sqrt(pvar.mm.air),
    phi = mm.air$CHELSA_summer_temp + 1.96 * sqrt(pvar.mm.air)
  )  # Add errors

(
  air <-
    ggplot(summer_narm_unique, aes(airtemp_mean, CHELSA_summer_temp)) +
    geom_ribbon(
      data = mm.air,
      mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi),
      alpha = 0.3
    ) +
    geom_line(
      data = mm.air,
      mapping = aes(x = airtemp_mean),
      colour = "red",
      size = 1
    ) +
    #stat_smooth(method = "lm", colour = "red")+
    geom_jitter(
      width = 0.1,
      height = 0.1,
      alpha = 0.6,
      size = 2,
      pch = 16,
      colour = "red"
    ) +
    theme_bw() +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(y = "CHELSA Air Temperature °C (1979-2013)", x = "Measured Air Temeprature °C") +
    ggtitle("Measured vs CHELSA - Air")
)

#Chelsa summer data vs measured summer data - soil####

#Remove NAs
summer_narm <- summer %>%
  filter(is.finite(CHELSA_summer_temp), is.finite(soiltemp_mean))

#Find only unique plot temperatures
summer_narm_unique <-
  summer_narm[!duplicated(summer_narm[c("CHELSA_summer_temp", "soiltemp_mean")]), ]

#Take extent
min_soil <- min(summer_narm_unique$soiltemp_mean, na.rm = TRUE)
max_soil <- max(summer_narm_unique$soiltemp_mean, na.rm = TRUE)

#Run model
lm_soil_chelsa_summer <-
  lmer(CHELSA_summer_temp ~ soiltemp_mean + (1 |
                                               Region), data = summer_narm_unique)
#lm_soil_chelsa_summer<-lm(CHELSA_summer_temp ~ soiltemp_mean, data=summer_narm_unique)
summary(lm_soil_chelsa_summer)
r.squaredGLMM(lm_soil_chelsa_summer)

#Predictions
mm.soil <-
  expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1),
              CHELSA_summer_temp = 0)  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(lm_soil_chelsa_summer), mm.soil)  # Create matrix of relevant effect sizes
mm.soil$CHELSA_summer_temp <-
  mm %*% fixef(lm_soil_chelsa_summer)  # Calculate height based on the relevant effect sizes
pvar.mm.soil <-
  diag(mm %*% tcrossprod(vcov(lm_soil_chelsa_summer), mm))
mm.soil <-
  data.frame(
    mm.soil,
    plo = mm.soil$CHELSA_summer_temp - 1.96 * sqrt(pvar.mm.soil),
    phi = mm.soil$CHELSA_summer_temp + 1.96 * sqrt(pvar.mm.soil)
  )  # Add errors

(
  soil <-
    ggplot(summer_narm_unique, aes(soiltemp_mean, CHELSA_summer_temp)) +
    #stat_smooth(method = "lm", colour = "goldenrod")+
    geom_ribbon(
      data = mm.soil,
      mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi),
      alpha = 0.3
    ) +
    geom_line(
      data = mm.soil,
      mapping = aes(x = soiltemp_mean),
      colour = "goldenrod",
      size = 1
    ) +
    geom_jitter(
      width = 0.1,
      height = 0.1,
      alpha = 0.6,
      size = 2,
      pch = 16,
      colour = "goldenrod"
    ) +
    theme_bw() +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(y = "CHELSA Air Temperature °C (1979-2013)", x = "Measured Soil Temeprature °C") +
    ggtitle("Measured vs CHELSA - Soil")
)

#Chelsa summer data vs measured summer data - moisture####

#Remove NAs
summer_narm <- summer %>%
  filter(is.finite(CHELSA_summer_precip), is.finite(moisture_mean))

#Find only unique plot temperatures
summer_narm_unique <-
  summer_narm[!duplicated(summer_narm[c("CHELSA_summer_precip", "moisture_mean")]), ]

#Take extent
min_moisture <- min(summer_narm_unique$moisture_mean, na.rm = TRUE)
max_moisture <- max(summer_narm_unique$moisture_mean, na.rm = TRUE)

#Run model
lm_moisture_chelsa_summer <-
  lmer(CHELSA_summer_precip ~ moisture_mean + (1 |
                                                 Region), data = summer_narm_unique)
#lm_moisture_chelsa_summer<-lm(CHELSA_summer_precip ~ moisture_mean, data=summer_narm_unique)
summary(lm_moisture_chelsa_summer)
r.squaredGLMM(lm_moisture_chelsa_summer)

# Predictions
mm.moisture <-
  expand.grid(
    moisture_mean = seq(min_moisture, max_moisture, 0.1),
    CHELSA_summer_precip = 0
  )  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(lm_moisture_chelsa_summer), mm.moisture)  # Create matrix of relevant effect sizes
mm.moisture$CHELSA_summer_precip <-
  mm %*% fixef(lm_moisture_chelsa_summer)  # Calculate height based on the relevant effect sizes
pvar.mm.moisture <-
  diag(mm %*% tcrossprod(vcov(lm_moisture_chelsa_summer), mm))
mm.moisture <-
  data.frame(
    mm.moisture,
    plo = mm.moisture$CHELSA_summer_precip - 1.96 * sqrt(pvar.mm.moisture),
    phi = mm.moisture$CHELSA_summer_precip + 1.96 *
      sqrt(pvar.mm.moisture)
  )  # Add errors

(
  precip <-
    ggplot(summer_narm_unique, aes(moisture_mean, CHELSA_summer_precip)) +
    geom_ribbon(
      data = mm.moisture,
      mapping = aes(x = moisture_mean, ymin = plo, ymax = phi),
      alpha = 0.3
    ) +
    geom_line(
      data = mm.moisture,
      mapping = aes(x = moisture_mean),
      colour = "dodgerblue",
      size = 1
    ) +
    #stat_smooth(method = "lm", colour = "dodgerblue")+
    geom_jitter(
      width = 0.1,
      height = 0.1,
      alpha = 0.6,
      size = 2,
      pch = 16,
      colour = "dodgerblue"
    ) +
    theme_bw() +
    #geom_abline(slope=1, intercept=0, linetype = "dashed")+
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(y = "CHELSA Precipitation (mm) (1979-2013)", x = "Measured Soil Moisture (%)") +
    ggtitle("Measured vs CHELSA - moisture")
)

#ESA summer data vs measured summer data - moisture####

#Remove NAs
summer_narm <- summer %>%
  filter(is.finite(ESA_moisture), is.finite(moisture_mean))

#Find only unique plot temperatures
summer_narm_unique <-
  summer_narm[!duplicated(summer_narm[c("ESA_moisture", "moisture_mean")]), ]

#Take extent
min_moisture <- min(summer_narm_unique$moisture_mean, na.rm = TRUE)
max_moisture <- max(summer_narm_unique$moisture_mean, na.rm = TRUE)

#Run model
#lm_moisture_ESA_summer<-lmer(ESA_moisture ~ moisture_mean + (1|Region), data=summer_narm_unique)
lm_moisture_ESA_summer <-
  lm(ESA_moisture ~ moisture_mean, data = summer_narm_unique)
summary(lm_moisture_ESA_summer)
r.squaredGLMM(lm_moisture_ESA_summer)

# Predictions
# mm.moisture <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), ESA_moisture = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(lm_moisture_ESA_summer), mm.moisture)  # Create matrix of relevant effect sizes
# mm.moisture$ESA_moisture <- mm %*% fixef(lm_moisture_ESA_summer)  # Calculate height based on the relevant effect sizes
# pvar.mm.moisture <- diag(mm %*% tcrossprod(vcov(lm_moisture_ESA_summer), mm))
# mm.moisture <- data.frame(mm.moisture, plo = mm.moisture$ESA_moisture-1.96*sqrt(pvar.mm.moisture),
#                           phi = mm.moisture$ESA_moisture+1.96*sqrt(pvar.mm.moisture))  # Add errors

(
  moisture <-
    ggplot(summer_narm_unique, aes(moisture_mean, ESA_moisture)) +
    #geom_ribbon(data = mm.moisture, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), alpha = 0.3) +
    #geom_line(data = mm.moisture, mapping = aes(x = moisture_mean), colour="blue", size=1) +
    stat_smooth(method = "lm", colour = "blue") +
    geom_jitter(
      width = 0.1,
      height = 0.1,
      alpha = 0.6,
      size = 2,
      pch = 16,
      colour = "blue"
    ) +
    theme_bw() +
    #geom_abline(slope=1, intercept=0, linetype = "dashed")+
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(y = "ESA Soil Moisture (%) (1979-2013)", x = "Measured Soil Moisture (%)") +
    ggtitle("Measured vs ESA - moisture")
)

grid.arrange(air, soil, precip, moisture, ncol = 4)



#Create blank
library(grid)
blank <- grid.rect(gp = gpar(col = "white"))

#Combine figures######
pdf(file = "scripts/users/hthomas/Output_Images/Tea/Env_vars.pdf",
    width = 12.5,
    height = 10)
grid.arrange(
  air_fig_Loss_summer,
  soil_t_fig_Loss_summer,
  soil_m_fig_Loss_summer,
  soil_m_fig_Loss_summer,
  CH_t_fig_Loss_summer,
  CH_t_fig_Loss_summer,
  CH_p_fig_Loss_summer,
  ESA_moist_fig_Loss_summer,
  air,
  soil,
  precip,
  moisture,
  ncol = 4
)
dev.off()

#Add k and S values####
#Single pool k
pdf(file = "scripts/users/hthomas/Output_Images/Tea/Env_vars_k.pdf",
    width = 12.5,
    height = 10)
grid.arrange(
  air_fig_k_summer,
  soil_t_fig_k_summer,
  soil_m_fig_k_summer,
  soil_m_fig_k_summer,
  CH_t_fig_k_summer,
  CH_t_fig_k_summer,
  CH_p_fig_k_summer,
  ESA_moist_fig_k_summer,
  ncol = 4
)
dev.off()

#TBI k
pdf(file = "scripts/users/hthomas/Output_Images/Tea/Env_vars_TBI_k.pdf",
    width = 12.5,
    height = 10)
grid.arrange(
  air_fig_TBI_k_summer,
  soil_t_fig_TBI_k_summer,
  soil_m_fig_TBI_k_summer,
  soil_m_fig_TBI_k_summer,
  CH_t_fig_TBI_k_summer,
  CH_t_fig_TBI_k_summer,
  CH_p_fig_TBI_k_summer,
  ESA_moist_fig_TBI_k_summer,
  ncol = 4
)
dev.off()

#TBI S
pdf(file = "scripts/users/hthomas/Output_Images/Tea/Env_vars_TBI_S.pdf",
    width = 12.5,
    height = 10)
grid.arrange(
  air_fig_TBI_S_summer,
  soil_t_fig_TBI_S_summer,
  soil_m_fig_TBI_S_summer,
  soil_m_fig_TBI_S_summer,
  CH_t_fig_TBI_S_summer,
  CH_t_fig_TBI_S_summer,
  CH_p_fig_TBI_S_summer,
  ESA_moist_fig_TBI_S_summer,
  ncol = 4
)
dev.off()

#Create effect-size figure
summer_dens <- summer_dens +
  theme(legend.position = "none")
summer_dens <- ggplot_gtable(ggplot_build(summer_dens))

summer_effects <- ggplot_gtable(ggplot_build(summer_effects))

maxWidth = unit.pmax(summer_dens$widths[2:3], summer_effects$widths[2:3])

summer_dens$widths[2:3] <- maxWidth
summer_effects$widths[2:3] <- maxWidth

pdf(file = "scripts/users/hthomas/Output_Images/Tea/Env_vars_effects_summer.pdf",
    width = 3,
    height = 3)
grid.arrange(summer_dens, summer_effects, nrow = 2)
dev.off()

#############Create map##########################################################

#load extra packages
library(rasterVis)
library(viridisLite) # for generating your own viridis colour scale
library(sp)
library(maps)
library(maptools)
library(mapproj)


####STEP 1####

mean_burial <- mean(summer$Days) #Get mean incubation length

#Remove Nas from predictors
season_narm <- summer %>%
  filter(is.finite(CHELSA_summer_temp), is.finite(ESA_moisture))

#Create overall model####
MC_map_g <-
  MCMCglmm(
    Loss ~ CHELSA_summer_temp + ESA_moisture + Days,
    random = ~ Site,
    data = season_narm[season_narm$Tea_Type == "Green", ]
  )
summary(MC_map_g)
#r.squaredGLMM(MC_map_g)

#Disagreggate ESA data so resolution is equal to CHELSA
ESA_summer_NH_disagr <- disaggregate(ESA_summer_NH, fact = 30)
res(ESA_summer_NH_disagr)
res(chelsa_summer_tundra)

#Ensure extents match
ESA_summer_NH_disagr <-
  crop(ESA_summer_NH_disagr, extent(-180.000, 180, 40, 84))
chelsa_summer_tundra <-
  crop(chelsa_summer_tundra, extent(-180.000, 180, 40, 84))
extent(ESA_summer_NH_disagr)
extent(chelsa_summer_tundra)

#Create overall decomposition map
decomp_map <-
  (
    summary(MC_map_g)$solutions[1, 1] + summary(MC_map_g)$solutions[2, 1] * chelsa_summer_tundra + summary(MC_map_g)$solutions[3, 1] * ESA_summer_NH_disagr + summary(MC_map_g)$solutions[4, 1] * mean_burial
  )


# Plot the map
levelplot(decomp_map, margin = F, main = "Decomposition Map")

writeRaster(decomp_map,
            "/Volumes/Teamshrub/Haydn/decom_map_g.tif",
            overwrite = TRUE)

#Create for rooibos####

#Create overall model####
MC_map_r <-
  MCMCglmm(
    Loss ~ CHELSA_summer_temp + ESA_moisture + Days,
    random = ~ Site,
    data = season_narm[season_narm$Tea_Type == "Rooibos", ]
  )
summary(MC_map_r)
#r.squaredGLMM(MC_map_g)

#Create overall decomposition map
decomp_map <-
  (
    summary(MC_map_r)$solutions[1, 1] + summary(MC_map_r)$solutions[2, 1] * chelsa_summer_tundra + summary(MC_map_r)$solutions[3, 1] * ESA_summer_NH_disagr + summary(MC_map_r)$solutions[4, 1] * mean_burial
  )

# Plot the map
#levelplot(decomp_map, margin = F, main = "Decomposition Map")

writeRaster(decomp_map,
            "/Volumes/Teamshrub/Haydn/decom_map_r.tif",
            overwrite = TRUE)

#Draw map####
#Re-import data
decomp_map_g <- raster("/Volumes/Teamshrub/Haydn/decom_map_g.tif")
decomp_map_r <- raster("/Volumes/Teamshrub/Haydn/decom_map_r.tif")

#Transform map so it is pole-focussed
newproj <-
  "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
project_raster_new_g <- projectRaster(decomp_map_g, crs = newproj)

writeRaster(
  project_raster_new_g,
  "/Volumes/Teamshrub/Haydn/decom_map_g_polar.tif",
  overwrite = TRUE
)

#Transform map so it is pole-focussed
newproj <-
  "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
project_raster_new_r <- projectRaster(decomp_map_r, crs = newproj)

writeRaster(project_raster_new_r,
            "/Volumes/Teamshrub/Haydn/decom_map_r_polar.tif")



test <- map2SpatialLines(decomp_map_g)
laea_wrld_sp <-
  spTransform(
    decomp_map_g,
    CRS(
      "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
    )
  )
plot(project_raster_new_g)

# Make pretty themes
no_borders_theme <-
  rasterTheme(
    region = inferno(100, begin = 0, end = 1),
    # virdis scale
    axis.line = list(col = 0),
    # no axes, scales etc
    par.main.text = list(font = 1, cex = 1)
  ) # normal face title

no_borders_theme_white <-
  rasterTheme(
    region = "white",
    # virdis scale
    axis.line = list(col = 0),
    # no axes, scales etc
    par.main.text = list(font = 1, cex = 1)
  ) # normal face title


#Add tea sites###
coords.tea <- tea %>%
  group_by(Region) %>%
  summarise(
    meanLat = mean(Lat),
    meanLon = mean(Lon),
    count = length(Region)
  )

coord <-
  SpatialPoints(cbind(coords.tea$meanLon, coords.tea$meanLat),
                proj4string = CRS("+init=epsg:4326"))
coord1 <-
  spTransform(
    coord,
    CRS(
      "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
    )
  )

coords <-
  cbind(Longitude = as.numeric(as.character(data$longitude)),
        Latitude = as.numeric(as.character(data$latitude)))
points <-
  SpatialPointsDataFrame(coords, data, proj4string = CRS("+init=epsg:4326"))


coord1 <-
  SpatialPoints(
    data.frame(coords.tea$meanLon, coords.tea$meanLat),
    proj4string = CRS(
      "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
    )
  )
coord2 <-
  SpatialPoints(
    data.frame(coords.tea$meanLon, coords.tea$meanLat),
    proj4string = CRS(
      "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
    )
  )


# Load  data
library(rgdal)
vegmap <-
  readOGR("/Volumes/Teamshrub/Climate_Data/CAVM_map/cp_biozone_la_shp",
          "cavm_all polygon")
ogrInfo("/Volumes/Teamshrub/Climate_Data/CAVM_map/cp_biozone_la_shp",
        "cavm_all polygon")
vegmap2 <-
  spTransform(vegmap,
              CRS(
                "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
              ))
vegmap3 <- vegmap2[which(vegmap2$ZONE < 1), ]


# Creates mask for everything below treeline
# Takes ages to run
# masked_raster <- mask(decomp_map, vegmap3, inverse = TRUE)
#
# writeRaster(masked_raster, "/Volumes/Teamshrub/Haydn/decom_map_no_ice.tif")
#
# masked_raster2 <- mask(masked_raster, vegmap2, inverse = TRUE)
#
# writeRaster(masked_raster2, "/Volumes/Teamshrub/Haydn/decom_map_treeline.tif")

#decomp_map_no_ice<-raster("/Volumes/Teamshrub/Haydn/decom_map_no_ice.tif")

colfunc <- colorRampPalette(c("blue", "purple", "orange", "red"))

library("sp")
library("maptools")
data(wrld_simpl)

#Green Map
pdf(file = "scripts/users/hthomas/Output_Images/Tea/Map_green_polar.pdf",
    width = 10,
    height = 10)
image(project_raster_new_g, col = inferno(100, begin = 0, end = 1))
data(wrld_simpl)
plot(wrld_simpl, add = TRUE)
plot(
  coord1,
  add = TRUE,
  pch = 16,
  col = alpha("darkgreen", 0.5),
  bg = "black",
  cex = sqrt((coords.tea$count / 5))
)
plot(
  coord1,
  add = TRUE,
  pch = 1,
  col = alpha("black", 0.8),
  bg = "black",
  cex = sqrt((coords.tea$count / 5))
)
dev.off()

#Green Map (old code)
# pdf(file="scripts/users/hthomas/Output_Images/Tea/Map_green.pdf",width=15,height=7.5)
# levelplot(decomp_map_g, main = "", margin=FALSE, frame.plot=FALSE, axes=FALSE, labels = FALSE,box=FALSE, par.settings = no_borders_theme, xlab=NULL, ylab=NULL, frame.plot=FALSE, axes=FALSE, box=FALSE, scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), maxpixels=2e6)+
#   layer(sp.points(coord1, add = TRUE, pch=16, col=alpha("darkgreen",0.5), bg="black", cex=sqrt((coords.tea$count/5))))+
#   layer(sp.points(coord1, add = TRUE, pch=1, col=alpha("black",0.8), bg="black", cex=sqrt((coords.tea$count/5))))
# dev.off()

#Rooibos Map
pdf(file = "scripts/users/hthomas/Output_Images/Tea/Map_red.pdf",
    width = 15,
    height = 7.5)
image(decomp_map_r,
      col = inferno(100, begin = 0, end = 1),
      ylim = c(-50, 90))
data(wrld_simpl)
plot(wrld_simpl, add = TRUE)
plot(
  coord1,
  add = TRUE,
  pch = 16,
  col = alpha("firebrick", 0.5),
  bg = "black",
  cex = sqrt((coords.tea$count / 5))
)
plot(
  coord1,
  add = TRUE,
  pch = 1,
  col = alpha("black", 0.8),
  bg = "black",
  cex = sqrt((coords.tea$count / 5))
)
dev.off()

#Differnece between maps
decomp_map_diff <- decomp_map_g - decomp_map_r

pdf(file = "scripts/users/hthomas/Output_Images/Tea/Map_difference.pdf",
    width = 15,
    height = 7.5)
image(decomp_map_diff,
      col = inferno(100, begin = 0, end = 1),
      ylim = c(-50, 90))
data(wrld_simpl)
plot(wrld_simpl, add = TRUE)
plot(
  coord1,
  add = TRUE,
  pch = 16,
  col = alpha("darkorchid", 0.5),
  bg = "black",
  cex = sqrt((coords.tea$count / 5))
)
plot(
  coord1,
  add = TRUE,
  pch = 1,
  col = alpha("black", 0.8),
  bg = "black",
  cex = sqrt((coords.tea$count / 5))
)
dev.off()

####WITHIN-SITE ANALYSIS####---------------------------------------------------

#Normalise everything within each site
ambient_normal <- ambient %>%
  group_by(Region, Tea_Type, Season) %>%
  mutate(
    Loss_normal = scale(Loss),
    airtemp_mean_normal = scale(airtemp_mean),
    soiltemp_mean_normal = scale(soiltemp_mean),
    moisture_mean_normal = scale(moisture_mean),
    CHELSA_summer_temp_normal = scale(CHELSA_summer_temp),
    CHELSA_summer_precip_normal = scale(CHELSA_summer_precip),
    ESA_moisture_normal = scale(ESA_moisture)
  )

summer_normal <- subset(ambient_normal, Season == "Summer")

#Run within-site models####≈
#a) Air temp
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(airtemp_mean_normal))

#air_normal<-MCMCglmm(Loss_normal ~ airtemp_mean_normal, random = ~Region + Site +Plot, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
air_normal <-
  lmer(Loss_normal ~ airtemp_mean_normal + (1 |
                                              Region / Site / Plot), data = season_narm)
summary(air_normal)

#Take extent
min_air <- min(season_narm$airtemp_mean_normal, na.rm = TRUE)
max_air <- max(season_narm$airtemp_mean_normal, na.rm = TRUE)

# #Model predictions
# mm.air <- expand.grid(airtemp_mean_normal = seq(min_air, max_air, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ airtemp_mean_normal), mm.air)  # Create matrix of relevant effect sizes
# mm.air$Loss_normal <- mm %*% fixef(air_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.air$plo <- mm %*% summary(air_normal)$solutions[,2]
# mm.air$phi <- mm %*% summary(air_normal)$solutions[,3]

# Predictions
mm.air <-
  expand.grid(airtemp_mean_normal = seq(min_air, max_air, 0.1),
              Loss_normal = 0)  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(air_normal), mm.air)  # Create matrix of relevant effect sizes
mm.air$Loss_normal <-
  mm %*% fixef(air_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.air <- diag(mm %*% tcrossprod(vcov(air_normal), mm))
mm.air <-
  data.frame(
    mm.air,
    plo = mm.air$Loss_normal - 1.96 * sqrt(pvar.mm.air),
    phi = mm.air$Loss_normal + 1.96 * sqrt(pvar.mm.air)
  )  # Add errors


#Test plot - tea vs air temp
(
  air_normal_fig <-
    ggplot(summer_normal, aes(airtemp_mean_normal, Loss_normal)) +
    geom_point(aes(colour = factor(Region)), alpha = 0.5, pch = 16) +
    geom_ribbon(
      data = mm.air,
      mapping = aes(x = airtemp_mean_normal, ymin = plo, ymax = phi),
      alpha = 0.3,
      fill = "red"
    ) +
    geom_line(
      data = mm.air,
      mapping = aes(x = airtemp_mean_normal, y = Loss_normal),
      size = 1,
      colour = "red"
    ) +
    theme_classic() +
    #stat_smooth(method = "lm", aes(colour = Region))+
    #stat_smooth(method = "lm")+
    ggtitle("Air Temperature (within sites)") +
    theme(legend.position = "none") +
    labs(x = "Normalilsed Air Temperature", y = "Normalised Mass Loss_normal")
)

#b) Soil temp
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(soiltemp_mean_normal))

soil_normal <-
  lmer(Loss_normal ~ soiltemp_mean_normal + (1 |
                                               Region / Site / Plot),
       data = season_narm)
#soil_normal<-MCMCglmm(Loss_normal ~ soiltemp_mean_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(soil_normal)

#Take extent
min_soil <- min(season_narm$soiltemp_mean_normal, na.rm = TRUE)
max_soil <- max(season_narm$soiltemp_mean_normal, na.rm = TRUE)

# #Model predictions
# mm.soil <- expand.grid(soiltemp_mean_normal = seq(min_soil, max_soil, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ soiltemp_mean_normal), mm.soil)  # Create matrix of relevant effect sizes
# mm.soil$Loss_normal <- mm %*% fixef(soil_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.soil$plo <- mm %*% summary(soil_normal)$solutions[,2]
# mm.soil$phi <- mm %*% summary(soil_normal)$solutions[,3]

# Predictions
mm.soil <-
  expand.grid(soiltemp_mean_normal = seq(min_soil, max_soil, 0.1),
              Loss_normal = 0)  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(soil_normal), mm.soil)  # Create matrix of relevant effect sizes
mm.soil$Loss_normal <-
  mm %*% fixef(soil_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.soil <- diag(mm %*% tcrossprod(vcov(soil_normal), mm))
mm.soil <-
  data.frame(
    mm.soil,
    plo = mm.soil$Loss_normal - 1.96 * sqrt(pvar.mm.soil),
    phi = mm.soil$Loss_normal + 1.96 * sqrt(pvar.mm.soil)
  )  # Add errors


#Test plot - tea vs soil temp
(
  soil_normal_fig <- ggplot() +
    geom_point(
      data = summer_normal,
      aes(soiltemp_mean_normal, Loss_normal, colour = factor(Region)),
      alpha = 0.5,
      pch = 16
    ) +
    geom_ribbon(
      data = mm.soil,
      mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi),
      alpha = 0.3,
      fill = "goldenrod"
    ) +
    geom_line(
      data = mm.soil,
      mapping = aes(x = soiltemp_mean_normal, y = Loss_normal),
      size = 1,
      colour = "goldenrod"
    ) +
    theme_classic() +
    ggtitle("Soil Temperature (within sites)") +
    theme(legend.position = "none") +
    labs(x = "Normalilsed Soil Temperature", y = "Normalised Mass Loss_normal")
)

#c) Soil Moisture
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(moisture_mean_normal))

moisture_normal <-
  lmer(Loss_normal ~ moisture_mean_normal + (1 |
                                               Region / Site / Plot),
       data = season_narm)
#moisture_normal<-MCMCglmm(Loss_normal ~ moisture_mean_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(moisture_normal)

#Take extent
min_moisture <- min(season_narm$moisture_mean_normal, na.rm = TRUE)
max_moisture <- max(season_narm$moisture_mean_normal, na.rm = TRUE)

# #Model predictions
# mm.moisture <- expand.grid(moisture_mean_normal = seq(min_moisture, max_moisture, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ moisture_mean_normal), mm.moisture)  # Create matrix of relevant effect sizes
# mm.moisture$Loss_normal <- mm %*% fixef(moisture_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.moisture$plo <- mm %*% summary(moisture_normal)$solutions[,2]
# mm.moisture$phi <- mm %*% summary(moisture_normal)$solutions[,3]

# Predictions
mm.moisture <-
  expand.grid(
    moisture_mean_normal = seq(min_moisture, max_moisture, 0.1),
    Loss_normal = 0
  )  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(moisture_normal), mm.moisture)  # Create matrix of relevant effect sizes
mm.moisture$Loss_normal <-
  mm %*% fixef(moisture_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.moisture <-
  diag(mm %*% tcrossprod(vcov(moisture_normal), mm))
mm.moisture <-
  data.frame(
    mm.moisture,
    plo = mm.moisture$Loss_normal - 1.96 * sqrt(pvar.mm.moisture),
    phi = mm.moisture$Loss_normal + 1.96 * sqrt(pvar.mm.moisture)
  )  # Add errors


#Test plot - tea vs moisture temp
(
  moisture_normal_fig <- ggplot() +
    geom_point(
      data = summer_normal,
      aes(moisture_mean_normal, Loss_normal, colour = factor(Region)),
      alpha = 0.5,
      pch = 16
    ) +
    geom_ribbon(
      data = mm.moisture,
      mapping = aes(x = moisture_mean_normal, ymin = plo, ymax = phi),
      alpha = 0.3,
      fill = "dodgerblue"
    ) +
    geom_line(
      data = mm.moisture,
      mapping = aes(x = moisture_mean_normal, y = Loss_normal),
      size = 1,
      colour = "dodgerblue"
    ) +
    theme_classic() +
    stat_smooth(method = "lm") +
    ggtitle("Soil Moisture (within sites)") +
    theme(legend.position = "none") +
    labs(x = "Normalilsed Soil Moisture", y = "Normalised Mass Loss_normal")
)

#d) CHELSA Temperatire
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal),
         is.finite(CHELSA_summer_temp_normal))

CH_t_normal <-
  lmer(Loss_normal ~ CHELSA_summer_temp_normal + (1 |
                                                    Region / Site / Plot),
       data = season_narm)
#CH_t_normal<-MCMCglmm(Loss_normal ~ CHELSA_summer_temp_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(CH_t_normal)

#Take extent
min_CH_t <- min(season_narm$CHELSA_summer_temp_normal, na.rm = TRUE)
max_CH_t <- max(season_narm$CHELSA_summer_temp_normal, na.rm = TRUE)

# #Model predictions
# mm.CH_t <- expand.grid(CHELSA_summer_temp_normal = seq(min_CH_t, max_CH_t, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ CHELSA_summer_temp_normal), mm.CH_t)  # Create matrix of relevant effect sizes
# mm.CH_t$Loss_normal <- mm %*% fixef(CH_t_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.CH_t$plo <- mm %*% summary(CH_t_normal)$solutions[,2]
# mm.CH_t$phi <- mm %*% summary(CH_t_normal)$solutions[,3]

# Predictions
mm.CH_t <-
  expand.grid(
    CHELSA_summer_temp_normal = seq(min_CH_t, max_CH_t, 0.1),
    Loss_normal = 0
  )  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(CH_t_normal), mm.CH_t)  # Create matrix of relevant effect sizes
mm.CH_t$Loss_normal <-
  mm %*% fixef(CH_t_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.CH_t <- diag(mm %*% tcrossprod(vcov(CH_t_normal), mm))
mm.CH_t <-
  data.frame(
    mm.CH_t,
    plo = mm.CH_t$Loss_normal - 1.96 * sqrt(pvar.mm.CH_t),
    phi = mm.CH_t$Loss_normal + 1.96 * sqrt(pvar.mm.CH_t)
  )  # Add errors


#Test plot - tea vs moisture temp
(
  CH_t_normal_fig <- ggplot() +
    geom_point(
      data = summer_normal,
      aes(CHELSA_summer_temp_normal, Loss_normal, colour = factor(Region)),
      alpha = 0.5,
      pch = 16
    ) +
    geom_ribbon(
      data = mm.CH_t,
      mapping = aes(x = CHELSA_summer_temp_normal, ymin = plo, ymax = phi),
      alpha = 0.3,
      fill = "red"
    ) +
    geom_line(
      data = mm.CH_t,
      mapping = aes(x = CHELSA_summer_temp_normal, y = Loss_normal),
      size = 1,
      colour = "red"
    ) +
    theme_classic() +
    stat_smooth(method = "lm") +
    ggtitle("CHELSA Temperature (within sites)") +
    theme(legend.position = "none") +
    labs(x = "Normalilsed CHELSA Air Temperature", y = "Normalised Mass Loss_normal")
)

#e) CHELSA Precip
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal),
         is.finite(CHELSA_summer_precip_normal))

CH_p_normal <-
  lmer(Loss_normal ~ CHELSA_summer_precip_normal + (1 |
                                                      Region / Site / Plot),
       data = season_narm)
#CH_p_normal<-MCMCglmm(Loss_normal ~ CHELSA_summer_precip_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(CH_p_normal)

#Take extent
min_CH_p <- min(season_narm$CHELSA_summer_precip_normal, na.rm = TRUE)
max_CH_p <- max(season_narm$CHELSA_summer_precip_normal, na.rm = TRUE)

# #Model predictions
# mm.CH_p <- expand.grid(CHELSA_summer_precip_normal = seq(min_CH_p, max_CH_p, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ CHELSA_summer_precip_normal), mm.CH_p)  # Create matrix of relevant effect sizes
# mm.CH_p$Loss_normal <- mm %*% fixef(CH_p_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.CH_p$plo <- mm %*% summary(CH_p_normal)$solutions[,2]
# mm.CH_p$phi <- mm %*% summary(CH_p_normal)$solutions[,3]

# Predictions
mm.CH_p <-
  expand.grid(
    CHELSA_summer_precip_normal = seq(min_CH_p, max_CH_p, 0.1),
    Loss_normal = 0
  )  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(CH_p_normal), mm.CH_p)  # Create matrix of relevant effect sizes
mm.CH_p$Loss_normal <-
  mm %*% fixef(CH_p_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.CH_p <- diag(mm %*% tcrossprod(vcov(CH_p_normal), mm))
mm.CH_p <-
  data.frame(
    mm.CH_p,
    plo = mm.CH_p$Loss_normal - 1.96 * sqrt(pvar.mm.CH_p),
    phi = mm.CH_p$Loss_normal + 1.96 * sqrt(pvar.mm.CH_p)
  )  # Add errors


#Test plot - tea vs moisture precip
(
  CH_p_normal_fig <- ggplot() +
    geom_point(
      data = summer_normal,
      aes(CHELSA_summer_precip_normal, Loss_normal, colour = factor(Region)),
      alpha = 0.5,
      pch = 16
    ) +
    geom_ribbon(
      data = mm.CH_p,
      mapping = aes(x = CHELSA_summer_precip_normal, ymin = plo, ymax = phi),
      alpha = 0.3,
      fill = "dodgerblue"
    ) +
    geom_line(
      data = mm.CH_p,
      mapping = aes(x = CHELSA_summer_precip_normal, y = Loss_normal),
      size = 1,
      colour = "dodgerblue"
    ) +
    theme_classic() +
    stat_smooth(method = "lm") +
    ggtitle("CHELSA Precip (within sites)") +
    theme(legend.position = "none") +
    labs(x = "Normalilsed CHELSA Precipitation", y = "Normalised Mass Loss_normal")
)

#f) ESA moisture
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(ESA_moisture_normal))

ESA_m_normal <-
  lmer(Loss_normal ~ ESA_moisture_normal + (1 |
                                              Region / Site / Plot), data = season_narm)
#ESA_m_normal<-MCMCglmm(Loss_normal ~ ESA_moisture_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(ESA_m_normal)

#Take extent
min_ESA_m <- min(season_narm$ESA_moisture_normal, na.rm = TRUE)
max_ESA_m <- max(season_narm$ESA_moisture_normal, na.rm = TRUE)

# #Model predictions
# mm.ESA_m <- expand.grid(ESA_moisture_normal = seq(min_ESA_m, max_ESA_m, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ ESA_moisture_normal), mm.ESA_m)  # Create matrix of relevant effect sizes
# mm.ESA_m$Loss_normal <- mm %*% fixef(ESA_m_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.ESA_m$plo <- mm %*% summary(ESA_m_normal)$solutions[,2]
# mm.ESA_m$phi <- mm %*% summary(ESA_m_normal)$solutions[,3]

# Predictions
mm.ESA_m <-
  expand.grid(ESA_moisture_normal = seq(min_ESA_m, max_ESA_m, 0.1),
              Loss_normal = 0)  # Create a blank dataset with the years we want
mm <-
  model.matrix(terms(ESA_m_normal), mm.ESA_m)  # Create matrix of relevant effect sizes
mm.ESA_m$Loss_normal <-
  mm %*% fixef(ESA_m_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.ESA_m <- diag(mm %*% tcrossprod(vcov(ESA_m_normal), mm))
mm.ESA_m <-
  data.frame(
    mm.ESA_m,
    plo = mm.ESA_m$Loss_normal - 1.96 * sqrt(pvar.mm.ESA_m),
    phi = mm.ESA_m$Loss_normal + 1.96 * sqrt(pvar.mm.ESA_m)
  )  # Add errors


#Test plot - tea vs moisture precip
(
  ESA_m_normal_fig <- ggplot() +
    geom_point(
      data = summer_normal,
      aes(ESA_moisture_normal, Loss_normal, colour = factor(Region)),
      alpha = 0.5,
      pch = 16
    ) +
    geom_ribbon(
      data = mm.ESA_m,
      mapping = aes(x = ESA_moisture_normal, ymin = plo, ymax = phi),
      alpha = 0.3,
      fill = "blue"
    ) +
    geom_line(
      data = mm.ESA_m,
      mapping = aes(x = ESA_moisture_normal, y = Loss_normal),
      size = 1,
      colour = "blue"
    ) +
    theme_classic() +
    stat_smooth(method = "lm") +
    ggtitle("ESA Soil Moisture (within sites)") +
    theme(legend.position = "none") +
    labs(x = "Normalilsed ESA Soil Moisture", y = "Normalised Mass Loss_normal")
)


#Arrange plots
pdf(file = "scripts/users/hthomas/Output_Images/Tea/normalised_relationships.pdf",
    width = 10,
    height = 7.5)
grid.arrange(
  air_normal_fig,
  soil_normal_fig,
  moisture_normal_fig,
  CH_t_normal_fig,
  CH_p_normal_fig,
  ESA_m_normal_fig,
  nrow = 2
)
dev.off()

####Binary Moisture Class for Sites#######------------------------------------
summer[(summer$Soil.moisture == ""), ]$Soil.moisture <- NA

#Compare moisture
##T-tests
m <-
  pairwise.t.test(summer$moisture_mean, summer$Soil.moisture, p.adj = "bonf")


#Significance bar locations
df1 <- data.frame(a = c(1, 1, 2, 3, 3), b = c(110, 111, 111, 111, 110))
df2 <- data.frame(a = c(1, 1, 1.5, 2, 2), b = c(95, 96, 96, 96, 95))
df3 <-
  data.frame(a = c(2, 2, 2.5, 3, 3),
             b = c(103, 104, 104, 104, 103))

#pdf(file="scripts/users/hthomas/Output_Images/Tea/Moisture_class_moisture.pdf", width = 5, height = 3)
#Plot
ggplot(summer[!is.na(summer$Soil.moisture), ], aes(Soil.moisture, moisture_mean)) +
  geom_boxplot(aes(fill = Soil.moisture)) +
  theme_classic() +
  scale_fill_manual(values = c("grey", "dodgerblue", "blue2"),
                    name = "Moisture Class") +
  geom_line(data = df1, aes(x = a, y = b)) + annotate(
    "text",
    x = 2,
    y = 112,
    label = ifelse(
      m$p.value[2, 1] < 0.001,
      "***",
      ifelse(m$p.value[2, 1] < 0.01, "**", ifelse(
        m$p.value[2, 1] < 0.05, "*", ifelse(l_r$p.value[1, 1] < 0.1, ".", "ns")
      ))
    ),
    size = 3.5
  ) +
  geom_line(data = df2, aes(x = a, y = b)) + annotate(
    "text",
    x = 1.5,
    y = 97,
    label = ifelse(
      m$p.value[1, 1] < 0.001,
      "***",
      ifelse(m$p.value[1, 1] < 0.01, "**", ifelse(
        m$p.value[1, 1] < 0.05, "*", ifelse(l_r$p.value[1, 1] < 0.1, ".", "ns")
      ))
    ),
    size = 3.5
  ) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate(
    "text",
    x = 2.5,
    y = 105,
    label = ifelse(
      m$p.value[2, 2] < 0.001,
      "***",
      ifelse(m$p.value[2, 2] < 0.01, "**", ifelse(
        m$p.value[2, 2] < 0.05, "*", ifelse(l_r$p.value[2, 2] < 0.1, ".", "ns")
      ))
    ),
    size = 3.5
  ) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))
#dev.off()

#Compare Decomposition
##T-tests
l_g <-
  pairwise.t.test(summer[summer$Tea_Type == "Green", ]$Loss, summer[summer$Tea_Type ==
                                                                      "Green", ]$Soil.moisture, p.adj = "bonf")
l_r <-
  pairwise.t.test(summer[summer$Tea_Type == "Rooibos", ]$Loss, summer[summer$Tea_Type ==
                                                                        "Rooibos", ]$Soil.moisture, p.adj = "bonf")
l <- pairwise.t.test(summer$Loss, summer$Tea_Type, p.adj = "bonf")

#Significance bar locations
df1 <-
  data.frame(a = c(0.75, 0.75, 1.0, 1.25, 1.25),
             b = c(92, 93, 93, 93, 92))
df2 <-
  data.frame(a = c(0.75, 0.75, 0.9, 1, 1),
             b = c(83, 84, 84, 84, 83))
df3 <-
  data.frame(a = c(1, 1, 1.1, 1.25, 1.25),
             b = c(88, 89, 89, 89, 88))

df4 <-
  data.frame(a = c(1.75, 1.75, 2, 2.25, 2.25),
             b = c(67, 68, 68, 68, 67))
df5 <-
  data.frame(a = c(1.75, 1.75, 1.9, 2, 2),
             b = c(58, 59, 59, 59, 58))
df6 <-
  data.frame(a = c(2, 2, 2.1, 2.25, 2.25),
             b = c(61, 62, 62, 62, 61))

df7 <- data.frame(a = c(1, 1, 1.5, 2, 2), b = c(98, 99, 99, 99, 98))

pdf(file = "scripts/users/hthomas/Output_Images/Tea/Moisture_class_decomp.pdf",
    width = 5,
    height = 3)
#Plot
ggplot(summer[!is.na(summer$Soil.moisture), ], aes(Tea_Type, Loss * 100)) +
  geom_boxplot(aes(fill = factor(Soil.moisture))) +
  theme_classic() +
  scale_fill_manual(values = c("grey", "dodgerblue", "blue2"),
                    name = "Moisture Class") +
  geom_line(data = df1, aes(x = a, y = b)) + annotate(
    "text",
    x = 1,
    y = 94,
    label = ifelse(
      l_g$p.value[2, 1] < 0.001,
      "***",
      ifelse(
        l_g$p.value[2, 1] < 0.01,
        "**",
        ifelse(l_g$p.value[2, 1] < 0.05, "*", ifelse(l_r$p.value[1, 1] < 0.1, ".", "ns"))
      )
    ),
    size = 3.5
  ) +
  geom_line(data = df2, aes(x = a, y = b)) + annotate(
    "text",
    x = 0.875,
    y = 87,
    label = ifelse(
      l_g$p.value[1, 1] < 0.001,
      "***",
      ifelse(
        l_g$p.value[1, 1] < 0.01,
        "**",
        ifelse(l_g$p.value[1, 1] < 0.05, "*", ifelse(l_r$p.value[1, 1] < 0.1, ".", "ns"))
      )
    ),
    size = 3.5
  ) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate(
    "text",
    x = 1.125,
    y = 90,
    label = ifelse(
      l_g$p.value[2, 2] < 0.001,
      "***",
      ifelse(
        l_g$p.value[2, 2] < 0.01,
        "**",
        ifelse(l_g$p.value[2, 2] < 0.05, "*", ifelse(l_r$p.value[2, 2] < 0.1, ".", "ns"))
      )
    ),
    size = 3.5
  ) +
  geom_line(data = df4, aes(x = a, y = b)) + annotate(
    "text",
    x = 2,
    y = 71,
    label = ifelse(
      l_r$p.value[2, 1] < 0.001,
      "***",
      ifelse(
        l_r$p.value[2, 1] < 0.01,
        "**",
        ifelse(l_r$p.value[2, 1] < 0.05, "*", ifelse(l_r$p.value[2, 1] < 0.1, ".", "ns"))
      )
    ),
    size = 3.5
  ) +
  geom_line(data = df5, aes(x = a, y = b)) + annotate(
    "text",
    x = 1.875,
    y = 62,
    label = ifelse(
      l_r$p.value[1, 1] < 0.001,
      "***",
      ifelse(
        l_r$p.value[1, 1] < 0.01,
        "**",
        ifelse(l_r$p.value[1, 1] < 0.05, "*", ifelse(l_r$p.value[1, 1] < 0.1, ".", "ns"))
      )
    ),
    size = 3.5
  ) +
  geom_line(data = df6, aes(x = a, y = b)) + annotate(
    "text",
    x = 2.125,
    y = 66,
    label = ifelse(
      l_r$p.value[2, 2] < 0.001,
      "***",
      ifelse(
        l_r$p.value[2, 2] < 0.01,
        "**",
        ifelse(l_r$p.value[2, 2] < 0.05, "*", ifelse(l_r$p.value[2, 2] < 0.1, ".", "ns"))
      )
    ),
    size = 5
  ) +
  geom_line(data = df7, aes(x = a, y = b)) + annotate(
    "text",
    x = 1.5,
    y = 100,
    label = ifelse(
      l$p.value[1, 1] < 0.001,
      "***",
      ifelse(l$p.value[1, 1] < 0.01, "**", ifelse(
        l$p.value[1, 1] < 0.05, "*", ifelse(l_r$p.value[1, 1] < 0.1, ".", "ns")
      ))
    ),
    size = 3.5
  ) +
  ylim(0, 100)
dev.off()


#Replot narms with class#------------------------------------------------
summer_normal[(summer_normal$Soil.moisture == ""), ]$Soil.moisture <-
  NA

#a) Air temp
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(airtemp_mean_normal))

#Plot - tea vs air temp
(
  air_normal_fig_class <- ggplot() +
    geom_point(
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(airtemp_mean_normal, Loss_normal, colour = factor(Soil.moisture)),
      alpha = 0.5,
      pch = 16
    ) +
    #geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "goldenrod") +
    #geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, y=Loss_normal), size=1, colour = "goldenrod") +
    theme_classic() +
    stat_smooth(
      method = "lm",
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(airtemp_mean_normal, Loss_normal, colour = factor(Soil.moisture))
    ) +
    scale_colour_manual(
      values = c("grey", "dodgerblue", "blue2"),
      name = "Moisture Class"
    ) +
    ggtitle("Air Temperature (within sites)") +
    labs(x = "Normalilsed Air Temperature", y = "Normalised Mass Loss")
)


#b) Soil temp
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(soiltemp_mean_normal))

#Plot - tea vs soil temp
(
  soil_normal_fig_class <- ggplot() +
    geom_point(
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(soiltemp_mean_normal, Loss_normal, colour = factor(Soil.moisture)),
      alpha = 0.5,
      pch = 16
    ) +
    #geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "goldenrod") +
    #geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, y=Loss_normal), size=1, colour = "goldenrod") +
    theme_classic() +
    stat_smooth(
      method = "lm",
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(soiltemp_mean_normal, Loss_normal, colour = factor(Soil.moisture))
    ) +
    scale_colour_manual(
      values = c("grey", "dodgerblue", "blue2"),
      name = "Moisture Class"
    ) +
    ggtitle("Soil Temperature (within sites)") +
    labs(x = "Normalilsed Soil Temperature", y = "Normalised Mass Loss")
)


#c) Soil Moisture
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(moisture_mean_normal))

#Test plot - tea vs moisture temp
(
  moisture_normal_fig_class <- ggplot() +
    geom_point(
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(moisture_mean_normal, Loss_normal, colour = factor(Soil.moisture)),
      alpha = 0.5,
      pch = 16
    ) +
    #geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "goldenrod") +
    #geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, y=Loss_normal), size=1, colour = "goldenrod") +
    theme_classic() +
    stat_smooth(
      method = "lm",
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(moisture_mean_normal, Loss_normal, colour = factor(Soil.moisture))
    ) +
    scale_colour_manual(
      values = c("grey", "dodgerblue", "blue2"),
      name = "Moisture Class"
    ) +
    ggtitle("Soil Moisture (within sites)") +
    labs(x = "Normalilsed Soil Moisture", y = "Normalised Mass Loss")
)


#d) CHELSA Temperatire
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal),
         is.finite(CHELSA_summer_temp_normal))


#Test plot - tea vs moisture temp
(
  CH_t_normal_fig_class <- ggplot() +
    geom_point(
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(
        CHELSA_summer_temp_normal,
        Loss_normal,
        colour = factor(Soil.moisture)
      ),
      alpha = 0.5,
      pch = 16
    ) +
    #geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "goldenrod") +
    #geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, y=Loss_normal), size=1, colour = "goldenrod") +
    theme_classic() +
    stat_smooth(
      method = "lm",
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(
        CHELSA_summer_temp_normal,
        Loss_normal,
        colour = factor(Soil.moisture)
      )
    ) +
    scale_colour_manual(
      values = c("grey", "dodgerblue", "blue2"),
      name = "Moisture Class"
    ) +
    ggtitle("CHELSA Temperature (within sites)") +
    labs(x = "Normalilsed CHELSA Air temperature", y = "Normalised Mass Loss")
)


#e) CHELSA Precip
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal),
         is.finite(CHELSA_summer_precip_normal))


#Test plot - tea vs moisture precip
(
  CH_p_normal_fig_class <- ggplot() +
    geom_point(
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(
        CHELSA_summer_precip_normal,
        Loss_normal,
        colour = factor(Soil.moisture)
      ),
      alpha = 0.5,
      pch = 16
    ) +
    #geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "goldenrod") +
    #geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, y=Loss_normal), size=1, colour = "goldenrod") +
    theme_classic() +
    stat_smooth(
      method = "lm",
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(
        CHELSA_summer_precip_normal,
        Loss_normal,
        colour = factor(Soil.moisture)
      )
    ) +
    scale_colour_manual(
      values = c("grey", "dodgerblue", "blue2"),
      name = "Moisture Class"
    ) +
    ggtitle("CHELSA Precipitation (within sites)") +
    labs(x = "Normalilsed CHELSA Precipitation", y = "Normalised Mass Loss")
)

#f) ESA moisture
season_narm <- summer_normal %>%
  filter(is.finite(Loss_normal), is.finite(ESA_moisture_normal))


#Test plot - tea vs moisture precip
(
  ESA_m_normal_fig_class <- ggplot() +
    geom_point(
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(ESA_moisture_normal, Loss_normal, colour = factor(Soil.moisture)),
      alpha = 0.5,
      pch = 16
    ) +
    #geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "goldenrod") +
    #geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, y=Loss_normal), size=1, colour = "goldenrod") +
    theme_classic() +
    stat_smooth(
      method = "lm",
      data = summer_normal[!is.na(summer_normal$Soil.moisture), ],
      aes(ESA_moisture_normal, Loss_normal, colour = factor(Soil.moisture))
    ) +
    scale_colour_manual(
      values = c("grey", "dodgerblue", "blue2"),
      name = "Moisture Class"
    ) +
    ggtitle("ESA Moisture (within sites)") +
    labs(x = "Normalilsed ESA Moisture", y = "Normalised Mass Loss")
)

pdf(file = "scripts/users/hthomas/Output_Images/Tea/normalised_relationships_mClass.pdf",
    width = 12,
    height = 6)
grid.arrange(
  air_normal_fig_class,
  soil_normal_fig_class,
  moisture_normal_fig_class,
  CH_t_normal_fig_class,
  CH_p_normal_fig_class,
  ESA_m_normal_fig_class,
  nrow = 2
)
dev.off()
