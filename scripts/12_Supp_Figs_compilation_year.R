# Tea - Figure compiling script
# Sleeping willow should have paid more attention at Coding Club

rm(list = ls())

# packages ----
library(tidyverse)
library(gridExtra)
library(lme4)
library(effects)
library(MCMCglmm)

# Read in tea
tea <-
  read.csv("users/hthomas/tea/data/combined_tea.csv",
           stringsAsFactors = F)

# Remove daily tea - too confusing!
tea <- subset(tea,!grepl("CG_DT_HT", tea$Plot))

# Remove sub zero plots
tea <- subset(tea, Loss > 0)
tea[tea$Tea_Type == "Rooibos" & tea$Loss > 0.5,]$Loss <- NA

# Make sure only using control plots
ambient <- subset(tea, Treatment == "None")

# Split into seasons to make things easier
summer <- subset(ambient, Season == "Summer")
year <- subset(ambient, Season == "Year")
winter <- subset(ambient, Season == "Winter")

# STAN MODEL - soil temperature ----
# soil temperature#
var.list <- c("Loss", "Loss_Day", "k", "TBI_k", "TBI_S")

# Calculate mean burial length

# Get column number
i = 1
var.num <- which(colnames(year) == var.list[i])

season_narm <- year %>%
  filter(is.finite(year[, var.num]), is.finite(airtemp_mean))

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

# AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

# Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

# Multiple plots per Site (more than 1)
count.plots <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(Plot)))

season_narm_r$MultiplePlots <-
  ifelse(season_narm_r$Site %in% count.plots$Site[count.plots$n.plots > 1], 1, 0)

# Add env.levels (original)
# season_narm_r$envlevel <-
#   ifelse(
#     season_narm_r$airtemp_mean_var_level == "Region",
#     0,
#     ifelse(season_narm_r$airtemp_mean_var_level ==
#              "Site", 1, 2)
#   )

# Add env.levels (alternative)
# Add env.levels (new - based on nestedness)
env.levels <- season_narm_r %>%
  dplyr::select(airtemp_mean, ESA_cell, Site, Plot)

season_narm_r$envlevel <- 0

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.plots = length(unique(airtemp_mean)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1],
         1,
         season_narm_r$envlevel)

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(airtemp_mean)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1],
         2,
         season_narm_r$envlevel)

# season_narm_r<-subset(season_narm_r, Cat == 1 | Cat == 2 | Cat == 3)

# AB: REMOVE MISSING VALUES OF SOIL airtemp AND TEMPERATURE FOR THE airtemp X TEMPERATURE INTERACTION MODEL
season_narm_r <- season_narm_r[!is.na(season_narm_r$airtemp_mean),]

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum),]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum),]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum),]

# Centre values - AB note: Either don't name this the same thing or save the amount you center by first so we can add it to the xhats later
airtemp_cent_amount <-
  attr(scale(
    season_narm_r$airtemp_mean,
    center = TRUE,
    scale = FALSE
  ),
  'scaled:center')
season_narm_r$airtemp_mean <-
  scale(season_narm_r$airtemp_mean,
        center = TRUE,
        scale = FALSE)
days_cent_amount <-
  attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE),
       'scaled:center')
season_narm_r$Days <-
  scale(season_narm_r$Days, center = TRUE, scale = FALSE)

# AB: caluclate mean and sd per site - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_sites <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::summarise(
    airtemp_mean_site = mean(airtemp_mean),
    airtemp_sd_site = sd(airtemp_mean)
  )

season_narm_r$airtemp_mean_site <-
  season_narm_r_sites$airtemp_mean_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$airtemp_sd_site <-
  season_narm_r_sites$airtemp_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]

season_narm_r$airtemp_sd_site[season_narm_r$airtemp_sd_site == 0] <-
  mean(season_narm_r$airtemp_sd_site[season_narm_r$airtemp_sd_site > 0], na.rm = T)
season_narm_r$airtemp_sd_site[is.na(season_narm_r$airtemp_sd_site)] <-
  0.01

# AB: caluclate mean and sd per region - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_regions <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::summarise(
    airtemp_mean_region = mean(airtemp_mean),
    airtemp_sd_region = sd(airtemp_mean)
  )

season_narm_r$airtemp_mean_region <-
  season_narm_r_regions$airtemp_mean_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$airtemp_sd_region <-
  season_narm_r_regions$airtemp_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]

season_narm_r$airtemp_sd_region[season_narm_r$airtemp_sd_region == 0] <-
  mean(season_narm_r$airtemp_sd_region[season_narm_r$airtemp_sd_region >
                                         0], na.rm = T)
season_narm_r$airtemp_sd_region[is.na(season_narm_r$airtemp_sd_region)] <-
  0.01

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::mutate(SiteDays = mean(Days),
                SiteDays_sd = sd(Days))

season_narm_r$SiteDays_sd[season_narm_r$SiteDays_sd == 0 |
                            is.na(season_narm_r$SiteDays_sd)] <-
  0.001

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::mutate(RegionDays = mean(Days),
                RegionDays_sd = sd(Days))

season_narm_r$RegionDays_sd[season_narm_r$RegionDays_sd == 0 |
                              is.na(season_narm_r$RegionDays_sd)] <-
  0.001

mean_burial <- mean(season_narm_r$Days)
min_soil <- min(season_narm_r$airtemp_mean, na.rm = TRUE)
max_soil <- max(season_narm_r$airtemp_mean, na.rm = TRUE)
min_airtemp <- min(season_narm_r$airtemp_mean, na.rm = TRUE)
max_airtemp <- max(season_narm_r$airtemp_mean, na.rm = TRUE)

xhats <-
  expand.grid(xhat1 = seq(min_airtemp, max_airtemp, by = 0.01),
              xhat3 = mean_burial) # AB: predicting soil airtemp at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

# Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multsites_lobs = season_narm_r$MultipleSites,
  multsites_lplot = season_narm_r$MultipleSites[!duplicated(season_narm_r$PlotNum)],
  multsites_lsite = season_narm_r$MultipleSites[!duplicated(season_narm_r$SiteNum)],
  multsites_lregion = season_narm_r$MultipleSites[!duplicated(season_narm_r$RegionNum)],
  multplots_lobs = season_narm_r$MultiplePlots,
  multplots_lplot = season_narm_r$MultiplePlots[!duplicated(season_narm_r$PlotNum)],
  multplots_lsite = season_narm_r$MultiplePlots[!duplicated(season_narm_r$SiteNum)],
  multplots_lregion = season_narm_r$MultiplePlots[!duplicated(season_narm_r$RegionNum)],
  multplots_region_lobs = season_narm_r$MultiplePlots_Region,
  multplots_region_lplot = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$PlotNum)],
  multplots_region_lsite = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$SiteNum)],
  multplots_region_lregion = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$RegionNum)],
  traitobs = season_narm_r$Loss,
  # temp_plot=as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum),]$airtemp_mean),
  # temp_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$airtemp_mean),
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$airtemp_mean_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$airtemp_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$airtemp_mean_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$airtemp_sd_site),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum),]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum),]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum),]$envlevel,
  meanT = mean(as.numeric(season_narm_r$airtemp_mean[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_air <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/airtemp_fits_year.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_air <- cout[cout$Param %in% c("preds"),]
predsout.space_air$airtemp <- rep(jags.dat_air$xhat1, each = 2)
predsout.space_air$Temp <- rep(jags.dat_air$xhat2, each = 2)
predsout.space_air$Tea_TypeNum <-
  rep(c(1, 2), times = (length(predsout.space_air$mean) / 2))
predsout.space_air$Tea_Type <-
  ifelse(predsout.space_air$Tea_TypeNum == 1, "Green", "Rooibos")

season_narm_r_air <- season_narm_r

# Load figures
(
  air <- ggplot() +
    geom_point(
      data = season_narm_r_air,
      aes(
        x = jitter(airtemp_mean + airtemp_cent_amount, amount = 0.05),
        y = jitter(Loss * 100, factor = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.4
    ) +
    geom_ribbon(
      data = predsout.space_air,
      aes(
        x = airtemp + airtemp_cent_amount,
        ymin = (`2.5%` * 100),
        ymax = (`97.5%` * 100),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_air,
      aes(
        x = airtemp + airtemp_cent_amount,
        y = mean * 100,
        colour = Tea_Type
      ),
      alpha = 1,
      lwd = 1.5
    ) +
    theme_classic() +
    coord_cartesian(y = c(0, 100)) +
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "airtemp",
      labels = c("low", "high")
    ) +
    labs(x = "Site annual air temp. (°C)", y = "Mass loss (%, year)") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("a)")
)

# Soil ----

season_narm <- year %>%
  filter(is.finite(year[, var.num]), is.finite(soiltemp_mean))

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

# AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

# Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

# Multiple plots per Site (more than 1)
count.plots <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(Plot)))

season_narm_r$MultiplePlots <-
  ifelse(season_narm_r$Site %in% count.plots$Site[count.plots$n.plots > 1], 1, 0)

# Add env.levels (original)
# season_narm_r$envlevel <-
#   ifelse(
#     season_narm_r$soiltemp_mean_var_level == "Region",
#     0,
#     ifelse(season_narm_r$soiltemp_mean_var_level ==
#              "Site", 1, 2)
#   )

# Add env.levels (alternative)
# Add env.levels (new - based on nestedness)
env.levels <- season_narm_r %>%
  dplyr::select(soiltemp_mean, ESA_cell, Site, Plot)

season_narm_r$envlevel <- 0

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.plots = length(unique(soiltemp_mean)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1],
         1,
         season_narm_r$envlevel)

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(soiltemp_mean)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1],
         2,
         season_narm_r$envlevel)

# Add categories

# season_narm_r$Cat <-
#   ifelse(
#     season_narm_r$MultiplePlots_Region == 0 &
#       season_narm_r$MultipleSites == 0,
#     1,
#     # No nesting - automatically at plot level
#     ifelse(
#       season_narm_r$MultiplePlots == 1 &
#         season_narm_r$MultipleSites == 0 &
#         season_narm_r$envlevel == 2,
#       2,
#       # Plot in site, plot level env data
#       ifelse(
#         season_narm_r$MultiplePlots == 1 &
#           season_narm_r$MultipleSites == 0 &
#           season_narm_r$envlevel != 2,
#         3,
#         # Plot in site, site / region level env data
#         ifelse(
#           season_narm_r$MultipleSites == 1 &
#             season_narm_r$MultiplePlots == 0 &
#             season_narm_r$envlevel == 2,
#           4,
#           ifelse(
#             season_narm_r$MultipleSites == 1 &
#               season_narm_r$MultiplePlots == 0 &
#               season_narm_r$envlevel == 1,
#             4,
#             ifelse(
#               season_narm_r$MultipleSites == 1 &
#                 season_narm_r$MultiplePlots == 0 &
#                 season_narm_r$envlevel == 0,
#               5,
#               ifelse(
#                 season_narm_r$MultipleSites == 1 &
#                   season_narm_r$MultiplePlots == 1 &
#                   season_narm_r$envlevel == 2,
#                 6,
#                 ifelse(
#                   season_narm_r$MultipleSites == 1 &
#                     season_narm_r$MultiplePlots == 1 &
#                     season_narm_r$envlevel == 1,
#                   7,
#                   ifelse(
#                     season_narm_r$MultipleSites == 1 &
#                       season_narm_r$MultiplePlots == 1 &
#                       season_narm_r$envlevel == 2,
#                     8,
#                     "NA"
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )

# Subset so only using data I want to check model

# season_narm_r<-subset(season_narm_r, Cat == 1 | Cat == 2 | Cat == 3)

# AB: REMOVE MISSING VALUES OF SOIL soiltemp AND TEMPERATURE FOR THE soiltemp X TEMPERATURE INTERACTION MODEL
season_narm_r <-
  season_narm_r[!is.na(season_narm_r$soiltemp_mean),]

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum),]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum),]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum),]

# Centre values - AB note: Either don't name this the same thing or save the amount you center by first so we can add it to the xhats later
soiltemp_cent_amount <-
  attr(scale(
    season_narm_r$soiltemp_mean,
    center = TRUE,
    scale = FALSE
  ),
  'scaled:center')
season_narm_r$soiltemp_mean <-
  scale(season_narm_r$soiltemp_mean,
        center = TRUE,
        scale = FALSE)
days_cent_amount <-
  attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE),
       'scaled:center')
season_narm_r$Days <-
  scale(season_narm_r$Days, center = TRUE, scale = FALSE)

# AB: caluclate mean and sd per site - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_sites <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::summarise(
    soiltemp_mean_site = mean(soiltemp_mean),
    soiltemp_sd_site = sd(soiltemp_mean)
  )

season_narm_r$soiltemp_mean_site <-
  season_narm_r_sites$soiltemp_mean_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$soiltemp_sd_site <-
  season_narm_r_sites$soiltemp_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]

season_narm_r$soiltemp_sd_site[season_narm_r$soiltemp_sd_site == 0] <-
  mean(season_narm_r$soiltemp_sd_site[season_narm_r$soiltemp_sd_site > 0], na.rm = T)
season_narm_r$soiltemp_sd_site[is.na(season_narm_r$soiltemp_sd_site)] <-
  0.01

# AB: caluclate mean and sd per region - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_regions <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::summarise(
    soiltemp_mean_region = mean(soiltemp_mean),
    soiltemp_sd_region = sd(soiltemp_mean)
  )

season_narm_r$soiltemp_mean_region <-
  season_narm_r_regions$soiltemp_mean_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$soiltemp_sd_region <-
  season_narm_r_regions$soiltemp_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]

season_narm_r$soiltemp_sd_region[season_narm_r$soiltemp_sd_region == 0] <-
  mean(season_narm_r$soiltemp_sd_region[season_narm_r$soiltemp_sd_region >
                                          0], na.rm = T)
season_narm_r$soiltemp_sd_region[is.na(season_narm_r$soiltemp_sd_region)] <-
  0.01

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::mutate(SiteDays = mean(Days),
                SiteDays_sd = sd(Days))

season_narm_r$SiteDays_sd[season_narm_r$SiteDays_sd == 0 |
                            is.na(season_narm_r$SiteDays_sd)] <-
  0.001

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::mutate(RegionDays = mean(Days),
                RegionDays_sd = sd(Days))

season_narm_r$RegionDays_sd[season_narm_r$RegionDays_sd == 0 |
                              is.na(season_narm_r$RegionDays_sd)] <-
  0.001

mean_burial <- mean(season_narm_r$Days)
min_soil <- min(season_narm_r$soiltemp_mean, na.rm = TRUE)
max_soil <- max(season_narm_r$soiltemp_mean, na.rm = TRUE)
min_soiltemp <- min(season_narm_r$soiltemp_mean, na.rm = TRUE)
max_soiltemp <- max(season_narm_r$soiltemp_mean, na.rm = TRUE)

xhats <-
  expand.grid(xhat1 = seq(min_soiltemp, max_soiltemp, by = 0.01),
              xhat3 = mean_burial) # AB: predicting soil soiltemp at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

# Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multsites_lobs = season_narm_r$MultipleSites,
  multsites_lplot = season_narm_r$MultipleSites[!duplicated(season_narm_r$PlotNum)],
  multsites_lsite = season_narm_r$MultipleSites[!duplicated(season_narm_r$SiteNum)],
  multsites_lregion = season_narm_r$MultipleSites[!duplicated(season_narm_r$RegionNum)],
  multplots_lobs = season_narm_r$MultiplePlots,
  multplots_lplot = season_narm_r$MultiplePlots[!duplicated(season_narm_r$PlotNum)],
  multplots_lsite = season_narm_r$MultiplePlots[!duplicated(season_narm_r$SiteNum)],
  multplots_lregion = season_narm_r$MultiplePlots[!duplicated(season_narm_r$RegionNum)],
  multplots_region_lobs = season_narm_r$MultiplePlots_Region,
  multplots_region_lplot = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$PlotNum)],
  multplots_region_lsite = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$SiteNum)],
  multplots_region_lregion = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$RegionNum)],
  traitobs = season_narm_r$Loss,
  # temp_plot=as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum),]$soiltemp_mean),
  # temp_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$soiltemp_mean),
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$soiltemp_mean_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$soiltemp_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$soiltemp_mean_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$soiltemp_sd_site),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum),]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum),]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum),]$envlevel,
  meanT = mean(as.numeric(season_narm_r$soiltemp_mean[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_soil <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/soiltemp_fits_year.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_soil <- cout[cout$Param %in% c("preds"),]
predsout.space_soil$soiltemp <- rep(jags.dat_soil$xhat1, each = 2)
predsout.space_soil$Temp <- rep(jags.dat_soil$xhat2, each = 2)
predsout.space_soil$Tea_TypeNum <-
  rep(c(1, 2), times = (length(predsout.space_soil$mean) / 2))
predsout.space_soil$Tea_Type <-
  ifelse(predsout.space_soil$Tea_TypeNum == 1, "Green", "Rooibos")

season_narm_r_soil <- season_narm_r

season_narm_r_soil$Tea_Type <-
  as.factor(season_narm_r_soil$Tea_Type)

lmer_mod <-
  lmer(Loss ~ Tea_Type * soiltemp_mean + (Tea_Type |
                                            ESA_cell / Site / Plot),
       data = season_narm_r_soil)
out <-
  as.data.frame(effect(c("Tea_Type", "soiltemp_mean"), lmer_mod))

season_narm_r_soil$Tea_Type <-
  as.factor(season_narm_r_soil$Tea_Type)

MCMC_mod <-
  MCMCglmm(Loss ~ soiltemp_mean,
           random = ~ ESA_cell:Site:Plot,
           data = season_narm_r_soil[season_narm_r_soil$Tea_Type == "Green",])

mm <-
  expand.grid(soiltemp_mean = seq(-8, 10, 0.1), Loss = 0)  # Create a blank dataset with the years we want
mm2 <-
  model.matrix(terms(Loss ~ soiltemp_mean), mm)  # Create matrix of relevant effect sizes
mm$Loss <-
  mm2 %*% fixef(MCMC_mod, use = "mean")  # Calculate based on the relevant effect sizes
mm$plo <- mm2 %*% summary(MCMC_mod)$solutions[, 2]
mm$phi <- mm2 %*% summary(MCMC_mod)$solutions[, 3]

ggplot(season_narm_r_soil) +
  geom_point(aes(soiltemp_mean, Loss, colour = Tea_Type)) +
  # stat_smooth(aes(soiltemp_mean,Loss,colour = Tea_Type),method = "lm")+
  geom_ribbon(
    data = out,
    mapping = aes(
      x = soiltemp_mean,
      ymin = lower,
      ymax = upper,
      fill = factor(Tea_Type)
    ),
    alpha = 0.5,
    show.legend = F
  ) +
  geom_line(
    data = out,
    mapping = aes(x = soiltemp_mean, y = fit, colour = Tea_Type),
    lwd = 1.5
  ) +
  geom_ribbon(
    data = mm,
    mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi),
    alpha = 0.5
  ) +
  geom_line(
    data = mm,
    mapping = aes(x = soiltemp_mean, y = Loss),
    size = 1.25
  ) +
  geom_ribbon(data = predsout.space_soil,
              aes(
                x = soiltemp,
                ymin = (`2.5%`),
                ymax = (`97.5%`),
                fill = factor(Tea_Type)
              ),
              alpha = 0.5) +
  geom_line(
    data = predsout.space_soil,
    aes(x = soiltemp, y = mean, colour = Tea_Type),
    alpha = 0.8,
    lwd = 1.5
  )

# Load figures
(
  soil <- ggplot() +
    geom_point(
      data = season_narm_r_soil,
      aes(
        x = jitter(soiltemp_mean + soiltemp_cent_amount, amount = 0.05),
        y = jitter(Loss * 100, amount = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.5
    ) +
    geom_ribbon(
      data = predsout.space_soil,
      aes(
        x = soiltemp + soiltemp_cent_amount,
        ymin = (`2.5%` * 100),
        ymax = (`97.5%` * 100),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_soil,
      aes(
        x = soiltemp + soiltemp_cent_amount,
        y = mean * 100,
        colour = Tea_Type
      ),
      alpha = 0.8,
      lwd = 1.5
    ) +
    theme_classic() +
    coord_cartesian(y = c(0, 100)) +
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "soiltemp",
      labels = c("low", "high")
    ) +
    labs(x = "Site annual soil temp. (°C)", y = "Mass loss (%, year)") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("b)")
)

# moisture ----

season_narm <- year %>%
  filter(is.finite(year[, var.num]), is.finite(moisture_mean))

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

# AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

# Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

# Multiple plots per Site (more than 1)
count.plots <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(Plot)))

season_narm_r$MultiplePlots <-
  ifelse(season_narm_r$Site %in% count.plots$Site[count.plots$n.plots > 1], 1, 0)

# Add env.levels (original)
# season_narm_r$envlevel <-
#   ifelse(
#     season_narm_r$moisture_mean_var_level == "Region",
#     0,
#     ifelse(season_narm_r$moisture_mean_var_level ==
#              "Site", 1, 2)
#   )

# Add env.levels (alternative)
# Add env.levels (new - based on nestedness)
env.levels <- season_narm_r %>%
  dplyr::select(moisture_mean, ESA_cell, Site, Plot)

season_narm_r$envlevel <- 0

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.plots = length(unique(moisture_mean)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1],
         1,
         season_narm_r$envlevel)

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(moisture_mean)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1],
         2,
         season_narm_r$envlevel)

# Add categories

# season_narm_r$Cat <-
#   ifelse(
#     season_narm_r$MultiplePlots_Region == 0 &
#       season_narm_r$MultipleSites == 0,
#     1,
#     # No nesting - automatically at plot level
#     ifelse(
#       season_narm_r$MultiplePlots == 1 &
#         season_narm_r$MultipleSites == 0 &
#         season_narm_r$envlevel == 2,
#       2,
#       # Plot in site, plot level env data
#       ifelse(
#         season_narm_r$MultiplePlots == 1 &
#           season_narm_r$MultipleSites == 0 &
#           season_narm_r$envlevel != 2,
#         3,
#         # Plot in site, site / region level env data
#         ifelse(
#           season_narm_r$MultipleSites == 1 &
#             season_narm_r$MultiplePlots == 0 &
#             season_narm_r$envlevel == 2,
#           4,
#           ifelse(
#             season_narm_r$MultipleSites == 1 &
#               season_narm_r$MultiplePlots == 0 &
#               season_narm_r$envlevel == 1,
#             4,
#             ifelse(
#               season_narm_r$MultipleSites == 1 &
#                 season_narm_r$MultiplePlots == 0 &
#                 season_narm_r$envlevel == 0,
#               5,
#               ifelse(
#                 season_narm_r$MultipleSites == 1 &
#                   season_narm_r$MultiplePlots == 1 &
#                   season_narm_r$envlevel == 2,
#                 6,
#                 ifelse(
#                   season_narm_r$MultipleSites == 1 &
#                     season_narm_r$MultiplePlots == 1 &
#                     season_narm_r$envlevel == 1,
#                   7,
#                   ifelse(
#                     season_narm_r$MultipleSites == 1 &
#                       season_narm_r$MultiplePlots == 1 &
#                       season_narm_r$envlevel == 2,
#                     8,
#                     "NA"
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )

# Subset so only using data I want to check model

# season_narm_r<-subset(season_narm_r, Cat == 1 | Cat == 2 | Cat == 3)

# AB: REMOVE MISSING VALUES OF moisture moisture AND TEMPERATURE FOR THE moisture X TEMPERATURE INTERACTION MODEL
season_narm_r <-
  season_narm_r[!is.na(season_narm_r$moisture_mean),]

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum),]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum),]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum),]

# Centre values - AB note: Either don't name this the same thing or save the amount you center by first so we can add it to the xhats later
moisture_cent_amount <-
  attr(scale(
    season_narm_r$moisture_mean,
    center = TRUE,
    scale = FALSE
  ),
  'scaled:center')
season_narm_r$moisture_mean <-
  scale(season_narm_r$moisture_mean,
        center = TRUE,
        scale = FALSE)
days_cent_amount <-
  attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE),
       'scaled:center')
season_narm_r$Days <-
  scale(season_narm_r$Days, center = TRUE, scale = FALSE)

# AB: caluclate mean and sd per site - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_sites <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::summarise(
    moisture_mean_site = mean(moisture_mean),
    moisture_sd_site = sd(moisture_mean)
  )

season_narm_r$moisture_mean_site <-
  season_narm_r_sites$moisture_mean_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$moisture_sd_site <-
  season_narm_r_sites$moisture_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]

season_narm_r$moisture_sd_site[season_narm_r$moisture_sd_site == 0] <-
  mean(season_narm_r$moisture_sd_site[season_narm_r$moisture_sd_site > 0], na.rm = T)
season_narm_r$moisture_sd_site[is.na(season_narm_r$moisture_sd_site)] <-
  0.01

# AB: caluclate mean and sd per region - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_regions <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::summarise(
    moisture_mean_region = mean(moisture_mean),
    moisture_sd_region = sd(moisture_mean)
  )

season_narm_r$moisture_mean_region <-
  season_narm_r_regions$moisture_mean_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$moisture_sd_region <-
  season_narm_r_regions$moisture_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]

season_narm_r$moisture_sd_region[season_narm_r$moisture_sd_region == 0] <-
  mean(season_narm_r$moisture_sd_region[season_narm_r$moisture_sd_region >
                                          0], na.rm = T)
season_narm_r$moisture_sd_region[is.na(season_narm_r$moisture_sd_region)] <-
  0.01

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::mutate(SiteDays = mean(Days),
                SiteDays_sd = sd(Days))

season_narm_r$SiteDays_sd[season_narm_r$SiteDays_sd == 0 |
                            is.na(season_narm_r$SiteDays_sd)] <-
  0.001

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::mutate(RegionDays = mean(Days),
                RegionDays_sd = sd(Days))

season_narm_r$RegionDays_sd[season_narm_r$RegionDays_sd == 0 |
                              is.na(season_narm_r$RegionDays_sd)] <-
  0.001

mean_burial <- mean(season_narm_r$Days)
min_moisture <- min(season_narm_r$moisture_mean, na.rm = TRUE)
max_moisture <- max(season_narm_r$moisture_mean, na.rm = TRUE)
min_moisture <- min(season_narm_r$moisture_mean, na.rm = TRUE)
max_moisture <- max(season_narm_r$moisture_mean, na.rm = TRUE)

xhats <-
  expand.grid(xhat1 = seq(min_moisture, max_moisture, by = 0.01),
              xhat3 = mean_burial) # AB: predicting moisture moisture at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

# Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multsites_lobs = season_narm_r$MultipleSites,
  multsites_lplot = season_narm_r$MultipleSites[!duplicated(season_narm_r$PlotNum)],
  multsites_lsite = season_narm_r$MultipleSites[!duplicated(season_narm_r$SiteNum)],
  multsites_lregion = season_narm_r$MultipleSites[!duplicated(season_narm_r$RegionNum)],
  multplots_lobs = season_narm_r$MultiplePlots,
  multplots_lplot = season_narm_r$MultiplePlots[!duplicated(season_narm_r$PlotNum)],
  multplots_lsite = season_narm_r$MultiplePlots[!duplicated(season_narm_r$SiteNum)],
  multplots_lregion = season_narm_r$MultiplePlots[!duplicated(season_narm_r$RegionNum)],
  multplots_region_lobs = season_narm_r$MultiplePlots_Region,
  multplots_region_lplot = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$PlotNum)],
  multplots_region_lsite = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$SiteNum)],
  multplots_region_lregion = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$RegionNum)],
  traitobs = season_narm_r$Loss,
  # temp_plot=as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum),]$moisture_mean),
  # temp_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$moisture_mean),
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$moisture_mean_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$moisture_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$moisture_mean_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$moisture_sd_site),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum),]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum),]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum),]$envlevel,
  meanT = mean(as.numeric(season_narm_r$moisture_mean[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_moisture <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/moisture_fits_year.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_moisture <- cout[cout$Param %in% c("preds"),]
predsout.space_moisture$moisture <-
  rep(jags.dat_moisture$xhat1, each = 2)
predsout.space_moisture$Temp <-
  rep(jags.dat_moisture$xhat2, each = 2)
predsout.space_moisture$Tea_TypeNum <-
  rep(c(1, 2), times = (length(predsout.space_moisture$mean) / 2))
predsout.space_moisture$Tea_Type <-
  ifelse(predsout.space_moisture$Tea_TypeNum == 1, "Green", "Rooibos")

season_narm_r_moisture <- season_narm_r

# Load figures
(
  moisture <- ggplot() +
    geom_point(
      data = season_narm_r_moisture,
      aes(
        x = jitter(moisture_mean + moisture_cent_amount, amount = 0.05),
        y = jitter(Loss * 100, amount = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.5
    ) +
    geom_ribbon(
      data = predsout.space_moisture,
      aes(
        x = moisture + moisture_cent_amount,
        ymin = (`2.5%` * 100),
        ymax = (`97.5%` * 100),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_moisture,
      aes(
        x = moisture + moisture_cent_amount,
        y = mean * 100,
        colour = Tea_Type
      ),
      alpha = 0.8,
      lwd = 1.5
    ) +
    theme_classic() +
    coord_cartesian(y = c(0, 100)) +
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "moisture",
      labels = c("low", "high")
    ) +
    labs(x = "Site summer soil moist. (%)", y = "Mass loss (%, year)") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("c)")
)

# CHELSA_year_temp ----

season_narm <- year %>%
  filter(is.finite(year[, var.num]), is.finite(CHELSA_year_temp))

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

# AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

# Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

# Multiple plots per Site (more than 1)
count.plots <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(Plot)))

season_narm_r$MultiplePlots <-
  ifelse(season_narm_r$Site %in% count.plots$Site[count.plots$n.plots > 1], 1, 0)

# Add env.levels (original)
# season_narm_r$envlevel <-
#   ifelse(
#     season_narm_r$CHELSA_year_temp_var_level == "Region",
#     0,
#     ifelse(season_narm_r$CHELSA_year_temp_var_level ==
#              "Site", 1, 2)
#   )

# Add env.levels (alternative)
# Add env.levels (new - based on nestedness)
env.levels <- season_narm_r %>%
  dplyr::select(CHELSA_year_temp, ESA_cell, Site, Plot)

season_narm_r$envlevel <- 0

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.plots = length(unique(CHELSA_year_temp)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1],
         1,
         season_narm_r$envlevel)

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(CHELSA_year_temp)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1],
         2,
         season_narm_r$envlevel)

# Add categories

# season_narm_r$Cat <-
#   ifelse(
#     season_narm_r$MultiplePlots_Region == 0 &
#       season_narm_r$MultipleSites == 0,
#     1,
#     # No nesting - automatically at plot level
#     ifelse(
#       season_narm_r$MultiplePlots == 1 &
#         season_narm_r$MultipleSites == 0 &
#         season_narm_r$envlevel == 2,
#       2,
#       # Plot in site, plot level env data
#       ifelse(
#         season_narm_r$MultiplePlots == 1 &
#           season_narm_r$MultipleSites == 0 &
#           season_narm_r$envlevel != 2,
#         3,
#         # Plot in site, site / region level env data
#         ifelse(
#           season_narm_r$MultipleSites == 1 &
#             season_narm_r$MultiplePlots == 0 &
#             season_narm_r$envlevel == 2,
#           4,
#           ifelse(
#             season_narm_r$MultipleSites == 1 &
#               season_narm_r$MultiplePlots == 0 &
#               season_narm_r$envlevel == 1,
#             4,
#             ifelse(
#               season_narm_r$MultipleSites == 1 &
#                 season_narm_r$MultiplePlots == 0 &
#                 season_narm_r$envlevel == 0,
#               5,
#               ifelse(
#                 season_narm_r$MultipleSites == 1 &
#                   season_narm_r$MultiplePlots == 1 &
#                   season_narm_r$envlevel == 2,
#                 6,
#                 ifelse(
#                   season_narm_r$MultipleSites == 1 &
#                     season_narm_r$MultiplePlots == 1 &
#                     season_narm_r$envlevel == 1,
#                   7,
#                   ifelse(
#                     season_narm_r$MultipleSites == 1 &
#                       season_narm_r$MultiplePlots == 1 &
#                       season_narm_r$envlevel == 2,
#                     8,
#                     "NA"
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )

# Subset so only using data I want to check model

# season_narm_r<-subset(season_narm_r, Cat == 1 | Cat == 2 | Cat == 3)

# AB: REMOVE MISSING VALUES OF CHELSA_year_temp CHELSA_year_temp AND TEMPERATURE FOR THE CHELSA_year_temp X TEMPERATURE INTERACTION MODEL
season_narm_r <-
  season_narm_r[!is.na(season_narm_r$CHELSA_year_temp),]

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum),]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum),]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum),]

# Centre values - AB note: Either don't name this the same thing or save the amount you center by first so we can add it to the xhats later
CHELSA_year_temp_cent_amount <-
  attr(scale(
    season_narm_r$CHELSA_year_temp,
    center = TRUE,
    scale = FALSE
  ),
  'scaled:center')
season_narm_r$CHELSA_year_temp <-
  scale(season_narm_r$CHELSA_year_temp,
        center = TRUE,
        scale = FALSE)
days_cent_amount <-
  attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE),
       'scaled:center')
season_narm_r$Days <-
  scale(season_narm_r$Days, center = TRUE, scale = FALSE)

# AB: caluclate mean and sd per site - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_sites <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::summarise(
    CHELSA_year_temp_site = mean(CHELSA_year_temp),
    CHELSA_year_temp_sd_site = sd(CHELSA_year_temp)
  )

season_narm_r$CHELSA_year_temp_site <-
  season_narm_r_sites$CHELSA_year_temp_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$CHELSA_year_temp_sd_site <-
  season_narm_r_sites$CHELSA_year_temp_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]

season_narm_r$CHELSA_year_temp_sd_site[season_narm_r$CHELSA_year_temp_sd_site ==
                                         0] <-
  mean(season_narm_r$CHELSA_year_temp_sd_site[season_narm_r$CHELSA_year_temp_sd_site >
                                                0], na.rm = T)
season_narm_r$CHELSA_year_temp_sd_site[is.na(season_narm_r$CHELSA_year_temp_sd_site)] <-
  0.01

# AB: caluclate mean and sd per region - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_regions <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::summarise(
    CHELSA_year_temp_region = mean(CHELSA_year_temp),
    CHELSA_year_temp_sd_region = sd(CHELSA_year_temp)
  )

season_narm_r$CHELSA_year_temp_region <-
  season_narm_r_regions$CHELSA_year_temp_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$CHELSA_year_temp_sd_region <-
  season_narm_r_regions$CHELSA_year_temp_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]

season_narm_r$CHELSA_year_temp_sd_region[season_narm_r$CHELSA_year_temp_sd_region ==
                                           0] <-
  mean(season_narm_r$CHELSA_year_temp_sd_region[season_narm_r$CHELSA_year_temp_sd_region >
                                                  0], na.rm = T)
season_narm_r$CHELSA_year_temp_sd_region[is.na(season_narm_r$CHELSA_year_temp_sd_region)] <-
  0.01

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::mutate(SiteDays = mean(Days),
                SiteDays_sd = sd(Days))

season_narm_r$SiteDays_sd[season_narm_r$SiteDays_sd == 0 |
                            is.na(season_narm_r$SiteDays_sd)] <-
  0.001

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::mutate(RegionDays = mean(Days),
                RegionDays_sd = sd(Days))

season_narm_r$RegionDays_sd[season_narm_r$RegionDays_sd == 0 |
                              is.na(season_narm_r$RegionDays_sd)] <-
  0.001

mean_burial <- mean(season_narm_r$Days)
min_CHELSA_year_temp <-
  min(season_narm_r$CHELSA_year_temp, na.rm = TRUE)
max_CHELSA_year_temp <-
  max(season_narm_r$CHELSA_year_temp, na.rm = TRUE)
min_CHELSA_year_temp <-
  min(season_narm_r$CHELSA_year_temp, na.rm = TRUE)
max_CHELSA_year_temp <-
  max(season_narm_r$CHELSA_year_temp, na.rm = TRUE)

xhats <-
  expand.grid(
    xhat1 = seq(min_CHELSA_year_temp, max_CHELSA_year_temp, by = 0.01),
    xhat3 = mean_burial
  ) # AB: predicting CHELSA_year_temp CHELSA_year_temp at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

# Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multsites_lobs = season_narm_r$MultipleSites,
  multsites_lplot = season_narm_r$MultipleSites[!duplicated(season_narm_r$PlotNum)],
  multsites_lsite = season_narm_r$MultipleSites[!duplicated(season_narm_r$SiteNum)],
  multsites_lregion = season_narm_r$MultipleSites[!duplicated(season_narm_r$RegionNum)],
  multplots_lobs = season_narm_r$MultiplePlots,
  multplots_lplot = season_narm_r$MultiplePlots[!duplicated(season_narm_r$PlotNum)],
  multplots_lsite = season_narm_r$MultiplePlots[!duplicated(season_narm_r$SiteNum)],
  multplots_lregion = season_narm_r$MultiplePlots[!duplicated(season_narm_r$RegionNum)],
  multplots_region_lobs = season_narm_r$MultiplePlots_Region,
  multplots_region_lplot = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$PlotNum)],
  multplots_region_lsite = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$SiteNum)],
  multplots_region_lregion = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$RegionNum)],
  traitobs = season_narm_r$Loss,
  # temp_plot=as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum),]$CHELSA_year_temp),
  # temp_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$CHELSA_year_temp),
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$CHELSA_year_temp_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$CHELSA_year_temp_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$CHELSA_year_temp_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$CHELSA_year_temp_sd_site),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum),]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum),]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum),]$envlevel,
  meanT = mean(as.numeric(season_narm_r$CHELSA_year_temp[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_CHELSA_year_temp <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/CHELSA_year_temp_fits_year.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_CHELSA_year_temp <-
  cout[cout$Param %in% c("preds"),]
predsout.space_CHELSA_year_temp$CHELSA_year_temp <-
  rep(jags.dat_CHELSA_year_temp$xhat1, each = 2)
predsout.space_CHELSA_year_temp$Temp <-
  rep(jags.dat_CHELSA_year_temp$xhat2, each = 2)
predsout.space_CHELSA_year_temp$Tea_TypeNum <-
  rep(c(1, 2), times = (length(predsout.space_CHELSA_year_temp$mean) / 2))
predsout.space_CHELSA_year_temp$Tea_Type <-
  ifelse(predsout.space_CHELSA_year_temp$Tea_TypeNum == 1,
         "Green",
         "Rooibos")

season_narm_r_CHELSA_year_temp <- season_narm_r

# Load figures
(
  CHELSA_year_temp <- ggplot() +
    geom_point(
      data = season_narm_r_CHELSA_year_temp,
      aes(
        x = jitter(CHELSA_year_temp + CHELSA_year_temp_cent_amount, amount = 0.05),
        y = jitter(Loss * 100, amount = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.4
    ) +
    geom_ribbon(
      data = predsout.space_CHELSA_year_temp,
      aes(
        x = CHELSA_year_temp + CHELSA_year_temp_cent_amount,
        ymin = (`2.5%` * 100),
        ymax = (`97.5%` * 100),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_CHELSA_year_temp,
      aes(
        x = CHELSA_year_temp + CHELSA_year_temp_cent_amount,
        y = mean * 100,
        colour = Tea_Type
      ),
      alpha = 0.8,
      lwd = 1.5
    ) +
    theme_classic() +
    coord_cartesian(y = c(0, 100)) +
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "CHELSA_year_temp",
      labels = c("low", "high")
    ) +
    labs(x = "Gridded year air temp. (°C)", y = "Mass loss (%, year)") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("d)")
)

# CHELSA_year_precip ----

season_narm <- year %>%
  filter(is.finite(year[, var.num]), is.finite(CHELSA_year_precip))

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

# AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

# Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

# Multiple plots per Site (more than 1)
count.plots <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(Plot)))

season_narm_r$MultiplePlots <-
  ifelse(season_narm_r$Site %in% count.plots$Site[count.plots$n.plots > 1], 1, 0)

# Add env.levels (original)
# season_narm_r$envlevel <-
#   ifelse(
#     season_narm_r$CHELSA_year_precip_var_level == "Region",
#     0,
#     ifelse(season_narm_r$CHELSA_year_precip_var_level ==
#              "Site", 1, 2)
#   )

# Add env.levels (alternative)
# Add env.levels (new - based on nestedness)
env.levels <- season_narm_r %>%
  dplyr::select(CHELSA_year_precip, ESA_cell, Site, Plot)

season_narm_r$envlevel <- 0

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.plots = length(unique(CHELSA_year_precip)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1],
         1,
         season_narm_r$envlevel)

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(CHELSA_year_precip)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1],
         2,
         season_narm_r$envlevel)

# Add categories

# season_narm_r<-subset(season_narm_r, Cat == 1 | Cat == 2 | Cat == 3)

# AB: REMOVE MISSING VALUES OF CHELSA_year_precip CHELSA_year_precip AND TEMPERATURE FOR THE CHELSA_year_precip X TEMPERATURE INTERACTION MODEL
season_narm_r <-
  season_narm_r[!is.na(season_narm_r$CHELSA_year_precip),]

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum),]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum),]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum),]

# Centre values - AB note: Either don't name this the same thing or save the amount you center by first so we can add it to the xhats later
CHELSA_year_precip_cent_amount <-
  attr(
    scale(
      season_narm_r$CHELSA_year_precip,
      center = TRUE,
      scale = FALSE
    ),
    'scaled:center'
  )
season_narm_r$CHELSA_year_precip <-
  scale(season_narm_r$CHELSA_year_precip,
        center = TRUE,
        scale = FALSE)
days_cent_amount <-
  attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE),
       'scaled:center')
season_narm_r$Days <-
  scale(season_narm_r$Days, center = TRUE, scale = FALSE)

# AB: caluclate mean and sd per site - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_sites <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::summarise(
    CHELSA_year_precip_site = mean(CHELSA_year_precip),
    CHELSA_year_precip_sd_site = sd(CHELSA_year_precip)
  )

season_narm_r$CHELSA_year_precip_site <-
  season_narm_r_sites$CHELSA_year_precip_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$CHELSA_year_precip_sd_site <-
  season_narm_r_sites$CHELSA_year_precip_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]

season_narm_r$CHELSA_year_precip_sd_site[season_narm_r$CHELSA_year_precip_sd_site ==
                                           0] <-
  mean(season_narm_r$CHELSA_year_precip_sd_site[season_narm_r$CHELSA_year_precip_sd_site >
                                                  0], na.rm = T)
season_narm_r$CHELSA_year_precip_sd_site[is.na(season_narm_r$CHELSA_year_precip_sd_site)] <-
  0.01

# AB: caluclate mean and sd per region - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_regions <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::summarise(
    CHELSA_year_precip_region = mean(CHELSA_year_precip),
    CHELSA_year_precip_sd_region = sd(CHELSA_year_precip)
  )

season_narm_r$CHELSA_year_precip_region <-
  season_narm_r_regions$CHELSA_year_precip_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$CHELSA_year_precip_sd_region <-
  season_narm_r_regions$CHELSA_year_precip_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]

season_narm_r$CHELSA_year_precip_sd_region[season_narm_r$CHELSA_year_precip_sd_region ==
                                             0] <-
  mean(season_narm_r$CHELSA_year_precip_sd_region[season_narm_r$CHELSA_year_precip_sd_region >
                                                    0], na.rm = T)
season_narm_r$CHELSA_year_precip_sd_region[is.na(season_narm_r$CHELSA_year_precip_sd_region)] <-
  0.01

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::mutate(SiteDays = mean(Days),
                SiteDays_sd = sd(Days))

season_narm_r$SiteDays_sd[season_narm_r$SiteDays_sd == 0 |
                            is.na(season_narm_r$SiteDays_sd)] <-
  0.001

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::mutate(RegionDays = mean(Days),
                RegionDays_sd = sd(Days))

season_narm_r$RegionDays_sd[season_narm_r$RegionDays_sd == 0 |
                              is.na(season_narm_r$RegionDays_sd)] <-
  0.001

mean_burial <- mean(season_narm_r$Days)
min_CHELSA_year_precip <-
  min(season_narm_r$CHELSA_year_precip, na.rm = TRUE)
max_CHELSA_year_precip <-
  max(season_narm_r$CHELSA_year_precip, na.rm = TRUE)
min_CHELSA_year_precip <-
  min(season_narm_r$CHELSA_year_precip, na.rm = TRUE)
max_CHELSA_year_precip <-
  max(season_narm_r$CHELSA_year_precip, na.rm = TRUE)

xhats <-
  expand.grid(
    xhat1 = seq(min_CHELSA_year_precip, max_CHELSA_year_precip, by = 0.01),
    xhat3 = mean_burial
  ) # AB: predicting CHELSA_year_precip CHELSA_year_precip at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

# Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multsites_lobs = season_narm_r$MultipleSites,
  multsites_lplot = season_narm_r$MultipleSites[!duplicated(season_narm_r$PlotNum)],
  multsites_lsite = season_narm_r$MultipleSites[!duplicated(season_narm_r$SiteNum)],
  multsites_lregion = season_narm_r$MultipleSites[!duplicated(season_narm_r$RegionNum)],
  multplots_lobs = season_narm_r$MultiplePlots,
  multplots_lplot = season_narm_r$MultiplePlots[!duplicated(season_narm_r$PlotNum)],
  multplots_lsite = season_narm_r$MultiplePlots[!duplicated(season_narm_r$SiteNum)],
  multplots_lregion = season_narm_r$MultiplePlots[!duplicated(season_narm_r$RegionNum)],
  multplots_region_lobs = season_narm_r$MultiplePlots_Region,
  multplots_region_lplot = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$PlotNum)],
  multplots_region_lsite = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$SiteNum)],
  multplots_region_lregion = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$RegionNum)],
  traitobs = season_narm_r$Loss,
  # temp_plot=as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum),]$CHELSA_year_precip),
  # temp_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$CHELSA_year_precip),
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$CHELSA_year_precip_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$CHELSA_year_precip_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$CHELSA_year_precip_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$CHELSA_year_precip_sd_site),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum),]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum),]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum),]$envlevel,
  meanT = mean(as.numeric(season_narm_r$CHELSA_year_precip[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_CHELSA_year_precip <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/CHELSA_year_precip_fits_year.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_CHELSA_year_precip <-
  cout[cout$Param %in% c("preds"),]
predsout.space_CHELSA_year_precip$CHELSA_year_precip <-
  rep(jags.dat_CHELSA_year_precip$xhat1, each = 2)
predsout.space_CHELSA_year_precip$Temp <-
  rep(jags.dat_CHELSA_year_precip$xhat2, each = 2)
predsout.space_CHELSA_year_precip$Tea_TypeNum <-
  rep(c(1, 2), times = (length(predsout.space_CHELSA_year_precip$mean) /
                          2))
predsout.space_CHELSA_year_precip$Tea_Type <-
  ifelse(predsout.space_CHELSA_year_precip$Tea_TypeNum == 1,
         "Green",
         "Rooibos")

season_narm_r_CHELSA_year_precip <- season_narm_r

# Load figures
(
  CHELSA_year_precip <- ggplot() +
    geom_point(
      data = season_narm_r_CHELSA_year_precip,
      aes(
        x = jitter(CHELSA_year_precip + CHELSA_year_precip_cent_amount, amount = 0.05),
        y = jitter(Loss * 100, amount = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.4
    ) +
    geom_ribbon(
      data = predsout.space_CHELSA_year_precip,
      aes(
        x = CHELSA_year_precip + CHELSA_year_precip_cent_amount,
        ymin = (`2.5%` * 100),
        ymax = (`97.5%` * 100),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_CHELSA_year_precip,
      aes(
        x = CHELSA_year_precip + CHELSA_year_precip_cent_amount,
        y = mean * 100,
        colour = Tea_Type
      ),
      alpha = 0.8,
      lwd = 1.5
    ) +
    theme_classic() +
    coord_cartesian(y = c(0, 100)) +
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "CHELSA_year_precip",
      labels = c("low", "high")
    ) +
    labs(x = "Gridded year precip. (mm)", y = "Mass loss (%, year)") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("e)")
)

# ESA_moisture ----

season_narm <- year %>%
  filter(is.finite(year[, var.num]), is.finite(ESA_moisture_year))

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

# AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

# Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

# Multiple plots per Site (more than 1)
count.plots <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(Plot)))

season_narm_r$MultiplePlots <-
  ifelse(season_narm_r$Site %in% count.plots$Site[count.plots$n.plots > 1], 1, 0)

# Add env.levels (original)
# season_narm_r$envlevel <-
#   ifelse(
#     season_narm_r$ESA_moisture_year_var_level == "Region",
#     0,
#     ifelse(season_narm_r$ESA_moisture_year_var_level ==
#              "Site", 1, 2)
#   )

# Add env.levels (alternative)
# Add env.levels (new - based on nestedness)
env.levels <- season_narm_r %>%
  dplyr::select(ESA_moisture_year, ESA_cell, Site, Plot)

season_narm_r$envlevel <- 0

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.plots = length(unique(ESA_moisture_year)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1],
         1,
         season_narm_r$envlevel)

env.levels2 <- env.levels %>%
  dplyr::group_by(ESA_cell, Site) %>%
  dplyr::summarise(n.plots = length(unique(ESA_moisture_year)))

season_narm_r$envlevel <-
  ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1],
         2,
         season_narm_r$envlevel)

# Add categories

# season_narm_r$Cat <-
#   ifelse(
#     season_narm_r$MultiplePlots_Region == 0 &
#       season_narm_r$MultipleSites == 0,
#     1,
#     # No nesting - automatically at plot level
#     ifelse(
#       season_narm_r$MultiplePlots == 1 &
#         season_narm_r$MultipleSites == 0 &
#         season_narm_r$envlevel == 2,
#       2,
#       # Plot in site, plot level env data
#       ifelse(
#         season_narm_r$MultiplePlots == 1 &
#           season_narm_r$MultipleSites == 0 &
#           season_narm_r$envlevel != 2,
#         3,
#         # Plot in site, site / region level env data
#         ifelse(
#           season_narm_r$MultipleSites == 1 &
#             season_narm_r$MultiplePlots == 0 &
#             season_narm_r$envlevel == 2,
#           4,
#           ifelse(
#             season_narm_r$MultipleSites == 1 &
#               season_narm_r$MultiplePlots == 0 &
#               season_narm_r$envlevel == 1,
#             4,
#             ifelse(
#               season_narm_r$MultipleSites == 1 &
#                 season_narm_r$MultiplePlots == 0 &
#                 season_narm_r$envlevel == 0,
#               5,
#               ifelse(
#                 season_narm_r$MultipleSites == 1 &
#                   season_narm_r$MultiplePlots == 1 &
#                   season_narm_r$envlevel == 2,
#                 6,
#                 ifelse(
#                   season_narm_r$MultipleSites == 1 &
#                     season_narm_r$MultiplePlots == 1 &
#                     season_narm_r$envlevel == 1,
#                   7,
#                   ifelse(
#                     season_narm_r$MultipleSites == 1 &
#                       season_narm_r$MultiplePlots == 1 &
#                       season_narm_r$envlevel == 2,
#                     8,
#                     "NA"
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )

# Subset so only using data I want to check model

# season_narm_r<-subset(season_narm_r, Cat == 1 | Cat == 2 | Cat == 3)

# AB: REMOVE MISSING VALUES OF SOIL ESA_moisture_year AND TEMPERATURE FOR THE ESA_moisture_year X TEMPERATURE INTERACTION MODEL
season_narm_r <-
  season_narm_r[!is.na(season_narm_r$ESA_moisture_year),]

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum),]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum),]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum),]

# Centre values - AB note: Either don't name this the same thing or save the amount you center by first so we can add it to the xhats later
ESA_moisture_year_cent_amount <-
  attr(scale(
    season_narm_r$ESA_moisture_year,
    center = TRUE,
    scale = FALSE
  ),
  'scaled:center')
season_narm_r$ESA_moisture_year <-
  scale(season_narm_r$ESA_moisture_year,
        center = TRUE,
        scale = FALSE)
days_cent_amount <-
  attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE),
       'scaled:center')
season_narm_r$Days <-
  scale(season_narm_r$Days, center = TRUE, scale = FALSE)

# AB: caluclate mean and sd per site - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_sites <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::summarise(
    ESA_moisture_year_site = mean(ESA_moisture_year),
    ESA_moisture_year_sd_site = sd(ESA_moisture_year)
  )

season_narm_r$ESA_moisture_year_site <-
  season_narm_r_sites$ESA_moisture_year_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$ESA_moisture_year_sd_site <-
  season_narm_r_sites$ESA_moisture_year_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]

season_narm_r$ESA_moisture_year_sd_site[season_narm_r$ESA_moisture_year_sd_site ==
                                          0] <-
  mean(season_narm_r$ESA_moisture_year_sd_site[season_narm_r$ESA_moisture_year_sd_site >
                                                 0], na.rm = T)
season_narm_r$ESA_moisture_year_sd_site[is.na(season_narm_r$ESA_moisture_year_sd_site)] <-
  0.01

# AB: caluclate mean and sd per region - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_regions <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::summarise(
    ESA_moisture_year_region = mean(ESA_moisture_year),
    ESA_moisture_year_sd_region = sd(ESA_moisture_year)
  )

season_narm_r$ESA_moisture_year_region <-
  season_narm_r_regions$ESA_moisture_year_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$ESA_moisture_year_sd_region <-
  season_narm_r_regions$ESA_moisture_year_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]

season_narm_r$ESA_moisture_year_sd_region[season_narm_r$ESA_moisture_year_sd_region ==
                                            0] <-
  mean(season_narm_r$ESA_moisture_year_sd_region[season_narm_r$ESA_moisture_year_sd_region >
                                                   0], na.rm = T)
season_narm_r$ESA_moisture_year_sd_region[is.na(season_narm_r$ESA_moisture_year_sd_region)] <-
  0.01

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(SiteNum) %>%
  dplyr::mutate(SiteDays = mean(Days),
                SiteDays_sd = sd(Days))

season_narm_r$SiteDays_sd[season_narm_r$SiteDays_sd == 0 |
                            is.na(season_narm_r$SiteDays_sd)] <-
  0.001

# Add mean days per region
season_narm_r <- season_narm_r %>%
  dplyr::group_by(RegionNum) %>%
  dplyr::mutate(RegionDays = mean(Days),
                RegionDays_sd = sd(Days))

season_narm_r$RegionDays_sd[season_narm_r$RegionDays_sd == 0 |
                              is.na(season_narm_r$RegionDays_sd)] <-
  0.001

mean_burial <- mean(season_narm_r$Days)
min_soil <- min(season_narm_r$ESA_moisture_year, na.rm = TRUE)
max_soil <- max(season_narm_r$ESA_moisture_year, na.rm = TRUE)
min_ESA_moisture_year <-
  min(season_narm_r$ESA_moisture_year, na.rm = TRUE)
max_ESA_moisture_year <-
  max(season_narm_r$ESA_moisture_year, na.rm = TRUE)

xhats <-
  expand.grid(
    xhat1 = seq(min_ESA_moisture_year, max_ESA_moisture_year, by = 0.01),
    xhat3 = mean_burial
  ) # AB: predicting soil ESA_moisture_year at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

# Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multsites_lobs = season_narm_r$MultipleSites,
  multsites_lplot = season_narm_r$MultipleSites[!duplicated(season_narm_r$PlotNum)],
  multsites_lsite = season_narm_r$MultipleSites[!duplicated(season_narm_r$SiteNum)],
  multsites_lregion = season_narm_r$MultipleSites[!duplicated(season_narm_r$RegionNum)],
  multplots_lobs = season_narm_r$MultiplePlots,
  multplots_lplot = season_narm_r$MultiplePlots[!duplicated(season_narm_r$PlotNum)],
  multplots_lsite = season_narm_r$MultiplePlots[!duplicated(season_narm_r$SiteNum)],
  multplots_lregion = season_narm_r$MultiplePlots[!duplicated(season_narm_r$RegionNum)],
  multplots_region_lobs = season_narm_r$MultiplePlots_Region,
  multplots_region_lplot = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$PlotNum)],
  multplots_region_lsite = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$SiteNum)],
  multplots_region_lregion = season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$RegionNum)],
  traitobs = season_narm_r$Loss,
  # temp_plot=as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum),]$ESA_moisture_year),
  # temp_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$ESA_moisture_year),
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$ESA_moisture_year_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$ESA_moisture_year_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$ESA_moisture_year_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$ESA_moisture_year_sd_site),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum),]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum),]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum),]$envlevel,
  meanT = mean(as.numeric(season_narm_r$ESA_moisture_year[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_ESA_moisture <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/ESA_moisture_year_fits_year.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_ESA_moisture <- cout[cout$Param %in% c("preds"),]
predsout.space_ESA_moisture$ESA_moisture <-
  rep(jags.dat_ESA_moisture$xhat1, each = 2)
predsout.space_ESA_moisture$Temp <-
  rep(jags.dat_ESA_moisture$xhat2, each = 2)
predsout.space_ESA_moisture$Tea_TypeNum <-
  rep(c(1, 2), times = (length(predsout.space_ESA_moisture$mean) / 2))
predsout.space_ESA_moisture$Tea_Type <-
  ifelse(predsout.space_ESA_moisture$Tea_TypeNum == 1,
         "Green",
         "Rooibos")

season_narm_r_ESA_moisture <- season_narm_r

# Load figures
(
  ESA_moisture <- ggplot() +
    geom_point(
      data = season_narm_r_ESA_moisture,
      aes(
        x = jitter(ESA_moisture_year + ESA_moisture_year_cent_amount, amount = 0.05),
        y = jitter(Loss * 100, amount = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.4
    ) +
    geom_ribbon(
      data = predsout.space_ESA_moisture,
      aes(
        x = ESA_moisture + ESA_moisture_year_cent_amount,
        ymin = (`2.5%` * 100),
        ymax = (`97.5%` * 100),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_ESA_moisture,
      aes(
        x = ESA_moisture + ESA_moisture_year_cent_amount,
        y = mean * 100,
        colour = Tea_Type
      ),
      alpha = 0.8,
      lwd = 1.5
    ) +
    theme_classic() +
    coord_cartesian(y = c(0, 100)) +
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "ESA_moisture",
      labels = c("low", "high")
    ) +
    labs(x = "Gridded summer soil moist. (%)", y = "Mass loss (%, year)") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("f)")
)

pdf(file = "users/hthomas/tea/figures/Env_biome_year.pdf",
    width = 10.5,
    height = 7.5)
grid.arrange(air,
             soil,
             moisture,
             CHELSA_year_temp,
             CHELSA_year_precip,
             ESA_moisture,
             nrow = 2)
dev.off()
