# Tea - Figure compiling script
# Sleeping willow should have paid more attention at Coding Club

rm(list = ls())

# packages ----
library(tidyverse)
library(gridExtra)

# Read in tea
tea <-
  read.csv("users/hthomas/tea/data/combined_tea.csv",
           stringsAsFactors = F)

# Remove daily tea - too confusing!
tea <- subset(tea, !grepl("CG_DT_HT", tea$Plot))

# Remove sub zero plots
tea <- subset(tea, Loss > 0)
tea[tea$Tea_Type == "Rooibos" & tea$Loss > 0.5, ]$Loss <- NA

# Make sure only using control plots
ambient <- subset(tea, Treatment == "None")

# Split into seasons to make things easier
summer <- subset(ambient, Season == "Summer")
year <- subset(ambient, Season == "Year")
winter <- subset(ambient, Season == "Winter")


##  STAN MODEL - soil temperature ----
# soil temperature#
var.list <- c("Loss", "Loss_Day", "k", "TBI_k", "TBI_S")

# Calculate mean burial length

# Get column number
i = 1
var.num <- which(colnames(summer) == var.list[i])

season_narm <- summer %>%
  filter(is.finite(summer[, var.num]), is.finite(airtemp_mean))

# Normaise everything within each region
season_normal <- season_narm %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::mutate(
    Loss = scale(Loss),
    airtemp_mean = scale(airtemp_mean),
    airtemp_mean = scale(airtemp_mean),
    airtemp_mean = scale(airtemp_mean),
    CHELSA_summer_temp = scale(CHELSA_summer_temp),
    CHELSA_summer_precip = scale(CHELSA_summer_precip),
    ESA_moisture = scale(ESA_moisture)
  ) %>%
  ungroup()

season_narm <- season_normal

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

#  AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

#  Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

#  Multiple plots per Site (more than 1)
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

#  AB: REMOVE MISSING VALUES OF air airtemp AND TEMPERATURE FOR THE airtemp X TEMPERATURE INTERACTION MODEL
season_narm_r <- season_narm_r[!is.na(season_narm_r$airtemp_mean), ]

season_narm_r <-
  subset(season_narm_r, MultiplePlots == 1 |
           MultipleSites == 1 | envlevel > 0)

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum), ]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum), ]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum), ]

# Only use data with multiple plots / sites per ESA cell
season_narm_r <-
  subset(season_narm_r, MultiplePlots > 0 | MultipleSites > 0)

# AB: caluclate mean and sd per plot - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_plots <- season_narm_r %>%
  dplyr::group_by(PlotNum) %>%
  dplyr::summarise(
    airtemp_mean_plot = mean(airtemp_mean),
    airtemp_sd_plot = sd(airtemp_mean)
  )

season_narm_r$airtemp_mean_plot <-
  season_narm_r_plots$airtemp_mean_plot[match(season_narm_r$PlotNum, season_narm_r_plots$PlotNum)]
season_narm_r$airtemp_sd_plot <-
  season_narm_r_plots$airtemp_sd_plot[match(season_narm_r$PlotNum, season_narm_r_plots$PlotNum)]

season_narm_r$airtemp_sd_plot[season_narm_r$airtemp_sd_plot == 0] <-
  mean(season_narm_r$airtemp_sd_plot[season_narm_r$airtemp_sd_plot > 0], na.rm = T)
season_narm_r$airtemp_sd_plot[is.na(season_narm_r$airtemp_sd_plot)] <-
  0.01

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

# Add mean days per plot
season_narm_r <- season_narm_r %>%
  dplyr::group_by(PlotNum) %>%
  dplyr::mutate(plotDays = mean(Days),
                plotDays_sd = sd(Days))

season_narm_r$plotDays_sd[season_narm_r$plotDays_sd == 0 |
                            is.na(season_narm_r$plotDays_sd)] <-
  0.001

# Add mean days per site
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
min_air <- min(season_narm_r$airtemp_mean, na.rm = TRUE)
max_air <- max(season_narm_r$airtemp_mean, na.rm = TRUE)
min_airtemp <- min(season_narm_r$airtemp_mean, na.rm = TRUE)
max_airtemp <- max(season_narm_r$airtemp_mean, na.rm = TRUE)

# xhats <-
#   expand.grid(xhat1 = seq(min_airtemp, max_airtemp, by = 0.01),
#               xhat3 = mean_burial) # AB: predicting air airtemp at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

xhats <-
  expand.grid(xhat1 = seq(min_airtemp, max_airtemp, length.out = 552),
              xhat3 = mean_burial)

#  Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NPlotDays = length(unique(season_narm_r$SiteDays)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  plotDays = season_narm_r$plotDays[!duplicated(season_narm_r$PlotNum)],
  plotDays_sd = season_narm_r$plotDays_sd[!duplicated(season_narm_r$PlotNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_plot = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$PlotNum)] ==
                           "Green", 1, 2),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multobs_lsite = season_narm_r$MultipleObs[!duplicated(season_narm_r$SiteNum)],
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
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum), ]$airtemp_mean_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum), ]$airtemp_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum), ]$airtemp_mean_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum), ]$airtemp_sd_site),
  temp_mean_plot = as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum), ]$airtemp_mean_plot),
  temp_sd_plot = as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum), ]$airtemp_sd_plot),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum), ]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum), ]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum), ]$envlevel,
  meanT = mean(as.numeric(season_narm_r$airtemp_mean[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_air <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/airtemp_fits_within.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_air <- cout[cout$Param %in% c("preds"), ]
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
        x = jitter(airtemp_mean, amount = 0.05),
        y = jitter(Loss, factor = 0.05),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.3
    ) +
    geom_ribbon(
      data = predsout.space_air,
      aes(
        x = airtemp,
        ymin = (`2.5%`),
        ymax = (`97.5%`),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_air,
      aes(x = airtemp, y = mean, colour = Tea_Type),
      alpha = 1,
      lwd = 1.5
    ) +
    theme_classic() +
    # coord_cartesian(y = c(0,1))+
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "airtemp",
      labels = c("low", "high")
    ) +
    labs(x = "Standardised site summer air temp.", y = "Standardised mass loss") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("a)")
)

#  Soil ----

season_narm <- summer %>%
  filter(is.finite(summer[, var.num]), is.finite(soiltemp_mean))

# Normaise everything within each region
season_normal <- season_narm %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::mutate(
    Loss = scale(Loss),
    soiltemp_mean = scale(soiltemp_mean),
    soiltemp_mean = scale(soiltemp_mean),
    soiltemp_mean = scale(soiltemp_mean),
    CHELSA_summer_temp = scale(CHELSA_summer_temp),
    CHELSA_summer_precip = scale(CHELSA_summer_precip),
    ESA_moisture = scale(ESA_moisture)
  ) %>%
  ungroup()

season_narm <- season_normal

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

#  AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

#  Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

#  Multiple plots per Site (more than 1)
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

#  AB: REMOVE MISSING VALUES OF air soiltemp AND TEMPERATURE FOR THE soiltemp X TEMPERATURE INTERACTION MODEL
season_narm_r <-
  season_narm_r[!is.na(season_narm_r$soiltemp_mean), ]

season_narm_r <-
  subset(season_narm_r, MultiplePlots == 1 |
           MultipleSites == 1 | envlevel > 0)

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum), ]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum), ]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum), ]

# Only use data with multiple plots / sites per ESA cell
season_narm_r <-
  subset(season_narm_r, MultiplePlots > 0 | MultipleSites > 0)

# AB: caluclate mean and sd per plot - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_plots <- season_narm_r %>%
  dplyr::group_by(PlotNum) %>%
  dplyr::summarise(
    soiltemp_mean_plot = mean(soiltemp_mean),
    soiltemp_sd_plot = sd(soiltemp_mean)
  )

season_narm_r$soiltemp_mean_plot <-
  season_narm_r_plots$soiltemp_mean_plot[match(season_narm_r$PlotNum, season_narm_r_plots$PlotNum)]
season_narm_r$soiltemp_sd_plot <-
  season_narm_r_plots$soiltemp_sd_plot[match(season_narm_r$PlotNum, season_narm_r_plots$PlotNum)]

season_narm_r$soiltemp_sd_plot[season_narm_r$soiltemp_sd_plot == 0] <-
  mean(season_narm_r$soiltemp_sd_plot[season_narm_r$soiltemp_sd_plot > 0], na.rm = T)
season_narm_r$soiltemp_sd_plot[is.na(season_narm_r$soiltemp_sd_plot)] <-
  0.01

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

# Add mean days per plot
season_narm_r <- season_narm_r %>%
  dplyr::group_by(PlotNum) %>%
  dplyr::mutate(plotDays = mean(Days),
                plotDays_sd = sd(Days))

season_narm_r$plotDays_sd[season_narm_r$plotDays_sd == 0 |
                            is.na(season_narm_r$plotDays_sd)] <-
  0.001

# Add mean days per site
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
min_air <- min(season_narm_r$soiltemp_mean, na.rm = TRUE)
max_air <- max(season_narm_r$soiltemp_mean, na.rm = TRUE)
min_soiltemp <- min(season_narm_r$soiltemp_mean, na.rm = TRUE)
max_soiltemp <- max(season_narm_r$soiltemp_mean, na.rm = TRUE)

# xhats <-
#   expand.grid(xhat1 = seq(min_soiltemp, max_soiltemp, by = 0.01),
#               xhat3 = mean_burial) # AB: predicting air soiltemp at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

xhats <-
  expand.grid(xhat1 = seq(min_soiltemp, max_soiltemp, length.out = 528),
              xhat3 = mean_burial)

#  Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NPlotDays = length(unique(season_narm_r$SiteDays)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  plotDays = season_narm_r$plotDays[!duplicated(season_narm_r$PlotNum)],
  plotDays_sd = season_narm_r$plotDays_sd[!duplicated(season_narm_r$PlotNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_plot = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$PlotNum)] ==
                           "Green", 1, 2),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multobs_lsite = season_narm_r$MultipleObs[!duplicated(season_narm_r$SiteNum)],
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
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum), ]$soiltemp_mean_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum), ]$soiltemp_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum), ]$soiltemp_mean_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum), ]$soiltemp_sd_site),
  temp_mean_plot = as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum), ]$soiltemp_mean_plot),
  temp_sd_plot = as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum), ]$soiltemp_sd_plot),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum), ]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum), ]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum), ]$envlevel,
  meanT = mean(as.numeric(season_narm_r$soiltemp_mean[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_soil <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/soiltemp_fits_within.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

predsout.space_soil <- cout[cout$Param %in% c("preds"), ]
predsout.space_soil$soiltemp <- rep(jags.dat_soil$xhat1, each = 2)
predsout.space_soil$Temp <- rep(jags.dat_soil$xhat3, each = 2)
predsout.space_soil$Tea_TypeNum <-
  rep(c(1, 2), times = (length(predsout.space_soil$mean) / 2))
predsout.space_soil$Tea_Type <-
  ifelse(predsout.space_soil$Tea_TypeNum == 1, "Green", "Rooibos")

season_narm_r_soil <- season_narm_r

# Load figures
(
  soil <- ggplot() +
    geom_point(
      data = season_narm_r_soil,
      aes(
        x = jitter(soiltemp_mean, amount = 0.05),
        y = jitter(Loss, amount = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.5
    ) +
    geom_ribbon(
      data = predsout.space_soil,
      aes(
        x = soiltemp,
        ymin = (`2.5%`),
        ymax = (`97.5%`),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_soil,
      aes(x = soiltemp, y = mean, colour = Tea_Type),
      alpha = 0.8,
      lwd = 1.5
    ) +
    theme_classic() +
    # coord_cartesian(y = c(0,1))+
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "soiltemp",
      labels = c("low", "high")
    ) +
    labs(x = "Standardised site summer soil temp.", y = "Standardised mass loss") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("b)")
)

#  moisture ----

season_narm <- summer %>%
  filter(is.finite(summer[, var.num]), is.finite(moisture_mean))

# Normaise everything within each region
season_normal <- season_narm %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::mutate(
    Loss = scale(Loss),
    moisture_mean = scale(moisture_mean),
    moisture_mean = scale(moisture_mean),
    moisture_mean = scale(moisture_mean),
    CHELSA_summer_temp = scale(CHELSA_summer_temp),
    CHELSA_summer_precip = scale(CHELSA_summer_precip),
    ESA_moisture = scale(ESA_moisture)
  ) %>%
  ungroup()

season_narm <- season_normal

# Subset for tea types
# season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") # AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <-
  season_narm # just so I don't have to rename everything

#  AB: MULTIPLE OBSERVATION
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(NObsPlot = length(Loss))

season_narm_r$MultipleObs <-
  ifelse(season_narm_r$NObsPlot > 4, 1, 0)

#  Multiple Sites
count.sites <- season_narm_r %>%
  dplyr::group_by(ESA_cell) %>%
  dplyr::summarise(n.sub = length(unique(Site)))

season_narm_r$MultipleSites <-
  ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

#  Multiple plots per Site (more than 1)
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

#  AB: REMOVE MISSING VALUES OF air moisture AND TEMPERATURE FOR THE moisture X TEMPERATURE INTERACTION MODEL
season_narm_r <-
  season_narm_r[!is.na(season_narm_r$moisture_mean), ]

season_narm_r <-
  subset(season_narm_r, MultiplePlots == 1 |
           MultipleSites == 1 | envlevel > 0)

# Add Region numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Tea_Type) %>%
  dplyr::mutate(RegionNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$RegionNum), ]

# Add Site numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Tea_Type) %>%
  dplyr::mutate(SiteNum = cur_group_id())

# Reorder by site number
season_narm_r <- season_narm_r[order(season_narm_r$SiteNum), ]

# Add Plot numbers
season_narm_r <- season_narm_r %>%
  dplyr::group_by(ESA_cell, Site, Plot, Tea_Type) %>%
  dplyr::mutate(PlotNum = cur_group_id())

# AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

# Reorder by plot number
season_narm_r <- season_narm_r[order(season_narm_r$PlotNum), ]

# Only use data with multiple plots / sites per ESA cell
season_narm_r <-
  subset(season_narm_r, MultiplePlots > 0 | MultipleSites > 0)

# AB: caluclate mean and sd per plot - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_plots <- season_narm_r %>%
  dplyr::group_by(PlotNum) %>%
  dplyr::summarise(
    moisture_mean_plot = mean(moisture_mean),
    moisture_sd_plot = sd(moisture_mean)
  )

season_narm_r$moisture_mean_plot <-
  season_narm_r_plots$moisture_mean_plot[match(season_narm_r$PlotNum, season_narm_r_plots$PlotNum)]
season_narm_r$moisture_sd_plot <-
  season_narm_r_plots$moisture_sd_plot[match(season_narm_r$PlotNum, season_narm_r_plots$PlotNum)]

season_narm_r$moisture_sd_plot[season_narm_r$moisture_sd_plot == 0] <-
  mean(season_narm_r$moisture_sd_plot[season_narm_r$moisture_sd_plot > 0], na.rm = T)
season_narm_r$moisture_sd_plot[is.na(season_narm_r$moisture_sd_plot)] <-
  0.01

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

# Add mean days per plot
season_narm_r <- season_narm_r %>%
  dplyr::group_by(PlotNum) %>%
  dplyr::mutate(plotDays = mean(Days),
                plotDays_sd = sd(Days))

season_narm_r$plotDays_sd[season_narm_r$plotDays_sd == 0 |
                            is.na(season_narm_r$plotDays_sd)] <-
  0.001

# Add mean days per site
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
min_air <- min(season_narm_r$moisture_mean, na.rm = TRUE)
max_air <- max(season_narm_r$moisture_mean, na.rm = TRUE)
min_moisture <- min(season_narm_r$moisture_mean, na.rm = TRUE)
max_moisture <- max(season_narm_r$moisture_mean, na.rm = TRUE)

# xhats <-
#   expand.grid(xhat1 = seq(min_moisture, max_moisture, by = 0.01),
#               xhat3 = mean_burial) # AB: predicting air moisture at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want

xhats <-
  expand.grid(xhat1 = seq(min_moisture, max_moisture, length.out = 437),
              xhat3 = mean_burial)

#  Third attempt - adding temperature levels ----
jags.dat <- list(
  Nobs = nrow(season_narm_r),
  NSite = length(unique(season_narm_r$SiteNum)),
  NRegion = length(unique(season_narm_r$RegionNum)),
  NPlot = length(unique(season_narm_r$PlotNum)),
  NPlotDays = length(unique(season_narm_r$SiteDays)),
  NSiteDays = length(unique(season_narm_r$SiteDays)),
  NRegionDays = length(unique(season_narm_r$RegionDays)),
  NTea = length(unique(season_narm_r$Tea_Type)),
  Region = season_narm_r$RegionNum,
  Site = season_narm_r$SiteNum,
  Plot = season_narm_r$PlotNum,
  SiteDays = season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd = season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  plotDays = season_narm_r$plotDays[!duplicated(season_narm_r$PlotNum)],
  plotDays_sd = season_narm_r$plotDays_sd[!duplicated(season_narm_r$PlotNum)],
  RegionDays = season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd = season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short = season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short = unique(season_narm_r$PlotNum),
  tea_type_plot = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$PlotNum)] ==
                           "Green", 1, 2),
  tea_type_site = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)] ==
                           "Green", 1, 2),
  tea_type_region = ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)] ==
                             "Green", 1, 2),
  multobs_lobs = season_narm_r$MultipleObs,
  multobs_lplot = season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multobs_lsite = season_narm_r$MultipleObs[!duplicated(season_narm_r$SiteNum)],
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
  temp_mean_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum), ]$moisture_mean_region),
  temp_sd_region = as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum), ]$moisture_sd_region),
  temp_mean_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum), ]$moisture_mean_site),
  temp_sd_site = as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum), ]$moisture_sd_site),
  temp_mean_plot = as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum), ]$moisture_mean_plot),
  temp_sd_plot = as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum), ]$moisture_sd_plot),
  obs_envlevel = season_narm_r$envlevel,
  plot_envlevel = season_narm_r[!duplicated(season_narm_r$PlotNum), ]$envlevel,
  site_envlevel = season_narm_r[!duplicated(season_narm_r$SiteNum), ]$envlevel,
  region_envlevel = season_narm_r[!duplicated(season_narm_r$RegionNum), ]$envlevel,
  meanT = mean(as.numeric(season_narm_r$moisture_mean[!duplicated(season_narm_r$ESA_cell)])),
  xhat1 = xhats$xhat1,
  xhat3 = xhats$xhat3,
  Nxhat = length(xhats$xhat1)
)

str(jags.dat)

# save key objects
jags.dat_moisture <- jags.dat

# Load figure data

load("users/hthomas/tea/Stan_outputs/moisture_fits_within.Rdata")

subset(cout, Param == "gamma0")
subset(cout, Param == "gamma1")

# Get gammas
g1 <- cout[cout$Param %in% c("gamma1"), ]
(g1$mean[1]) * 100
(g1$mean[2]) * 100
(g1$`97.5%`[1] - g1$mean[1]) * 100
(g1$`97.5%`[2] - g1$mean[2]) * 100

predsout.space_moisture <- cout[cout$Param %in% c("preds"), ]
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
        x = jitter(moisture_mean, amount = 0.05),
        y = jitter(Loss, amount = 0.01),
        colour = factor(Tea_Type)
      ),
      pch = 16 ,
      alpha = 0.5
    ) +
    geom_ribbon(
      data = predsout.space_moisture,
      aes(
        x = moisture,
        ymin = (`2.5%`),
        ymax = (`97.5%`),
        fill = factor(Tea_Type)
      ),
      alpha = 0.5
    ) +
    geom_line(
      data = predsout.space_moisture,
      aes(x = moisture, y = mean, colour = Tea_Type),
      alpha = 0.8,
      lwd = 1.5
    ) +
    theme_classic() +
    # coord_cartesian(y = c(0,1))+
    scale_colour_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_fill_manual(values = c("#00b100", "#9A0C0C"), name = "Tea Type") +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      name = "moisture",
      labels = c("low", "high")
    ) +
    labs(x = "Standardised site summer soil moist.", y = "Standardised mass loss") +
    theme(legend.position = "none") +
    theme(
      title = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("c)")
)

pdf(file = "users/hthomas/tea/figures/Env_biome_within.pdf",
    width = 4,
    height = 11)
grid.arrange(air, soil, moisture,
             nrow = 3)
dev.off()
