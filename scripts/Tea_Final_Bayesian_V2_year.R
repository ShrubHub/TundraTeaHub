#Tea bag full analysis script
#20 Nov 2017
#Sleeping willow is making a brew

#Detach packages####
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()


#Question 1 - Differences between teas

####Open packages####
library(raster)
library(rgdal)
library(lme4)
library(nlme)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
require(gridExtra)
#library(brms)
#library(rstan)
#library(StanHeaders)
library(MuMIn)
library(MCMCglmm)
library(postMCMCglmm)

#Set some custom functions#
`%notin%` <- function(x,y) !(x %in% y)
se <- function(x) sqrt(var(x, na.rm=T)/length(x))

#Import data####
tea<-read.csv(file="scripts/users/hthomas/data/Tea/teabag_data_update.csv")
tea$Tea_init<-as.numeric(as.character(tea$Tea_init)) #Convert to number
tea$Tea_final<-as.numeric(as.character(tea$Tea_final)) #Convert to number
tea$Loss<-1-(tea$Tea_final / tea$Tea_init) #Recalculate loss using full values
tea$Days<-as.numeric(as.character(tea$Days)) #Convert to number
tea<-tea[!is.na(tea$Loss),] #Remove NAs (4859 -> 4728)
tea<-tea[!is.na(tea$Days),] #Remove NAs (none)
tea$Burial<-as.Date(tea$Burial, format = "%d/%m/%Y") #Convert date format
tea$Recovery<-as.Date(tea$Recovery, format = "%d/%m/%Y") #Convert date format
tea$latlon<-paste(tea$Lat,"_",tea$Lon,sep="") #Add unique coordinate column

#Test decay rates (Common garden data)####
#Check tea decay rates for Common Garden

Daily_Tea<-tea[grep("DT", tea$Plot),] #Daily tea only
Common_garden<-tea[grep("CG", tea$Plot),] #All common garden ambient plots
two_yr_Kluane<-filter(tea, Site == "Kluane Plateau")
two_yr_Common_Garden<-filter(tea, Plot == "CG_HT_year"|
                               Plot == "CG_HT_year"|
                               Plot == "CG_2Y"|
                               Plot == "CG_Y1_3m"|
                               Plot == "CG_Y2_3m")

daily<-ggplot(Daily_Tea,aes(Days,(1-Loss)*100,colour=factor(Tea_Type)))+
  geom_point()+
  geom_smooth(method="lm", formula= (y ~ (log(x))))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="Days")+
  ggtitle("Common Garden - Two months")

Kluane<-ggplot(two_yr_Kluane,aes(Days,(1-Loss)*100,colour=factor(Tea_Type)))+
  geom_point()+
  geom_smooth(method="lm", formula= (y ~ (log(x))))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Remaining",x="Days")+
  ggtitle("Kluane Plateau - Two years")

Common<-ggplot(two_yr_Common_Garden,aes(Days,(1-Loss)*100,colour=factor(Tea_Type)))+
  geom_point()+
  geom_smooth(method="lm", formula= (y ~ (log(x))))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="Days")+
  ggtitle("Common Garden - Two years")

grid.arrange(daily,Common,Kluane,ncol=3)

#Clean data####
#Set minimum of three weeks incubation (i.e. first few DTs)
tea<-tea[tea$Days>21,]

#Calculate mass loss per day 
tea$Loss_Day<-tea$Loss / tea$Days

#Calculate k score (one pool exponential model)
tea$k<--log(1-(tea$Loss))*(1/(tea$Days))

#Shift points that models will think are in the sea
# tea[tea$Site=="Storfjord, Lofoten Island",]$Lon<-15.75397
# tea$latlon<-paste(tea$Lat,"_",tea$Lon,sep="")

#Check meta data####

length(unique(tea$Region)) #38 regions
length(unique(tea$Site)) #361 sites (unique coordinates)
length(unique(tea$latlon)) #340 unique coordinates (unique coordinates)

#Make sure plots aren't replicated across incubation lengths
tea$Plot_unique<-paste(tea$Plot,tea$Season,sep="_")

length(unique(tea$Plot_unique)) #648 unique plots (unique coordinates - 649 but includes NA)

#Add tea bag index variables####

#Tea bag hydrolisable fractions:
Hg<-0.842
Hr<-0.552

#Note that I have code to group tea in multiple ways (pairwise, by plot, by site etc), 
#but here I am grouping by plot

plot_means<-tea %>% 
  group_by(Site,Plot_unique,Tea_Type,Days) %>% 
  select(Tea_ID,Site,Plot_unique,Days,Tea_Type,Loss) %>%
  dplyr::summarise(plot_mean = mean(Loss)) %>%
  tidyr::spread(Tea_Type, plot_mean) %>%
  arrange(Site) %>%
  ungroup()

#Calculate TBI values
plot_means$S<-1-(plot_means$Green/Hg)
plot_means$ar<-Hr*(1-plot_means$S)
plot_means$k<-log(plot_means$ar/((1-plot_means$Rooibos)-(1-plot_means$ar)))/plot_means$Days

#Add data to plot_means
plot_means$Region<-tea$Region[match(plot_means$Plot_unique,tea$Plot_unique)] #Add region
plot_means$Season<-tea$Season[match(plot_means$Plot_unique,tea$Plot_unique)] #Add season

#Examine realtionship between S & K#
str(plot_means)

ggplot(plot_means,aes(S,k))+
  geom_point(aes(colour=factor(Season)),alpha=0.5)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("S vs k - All time periods")

ggplot(plot_means[plot_means$Season=="Year",],aes(S,k))+
  geom_point(aes(colour=factor(Region)),alpha=0.5)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("S vs k - Year")

#Add back to tea#
tea$TBI_S<-plot_means$S[match(tea$Plot_unique,plot_means$Plot_unique)]
tea$TBI_k<-plot_means$k[match(tea$Plot_unique,plot_means$Plot_unique)]

#Add climatic information####

#1) Field measured variables ####
##NB - these could be updated with 2017 values
#Add plot-measured variables####
Plot_Variables_Soil<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Soil_Temps.csv")
Plot_Variables_Air<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Air_Temps.csv")
Plot_Variables_Moisture<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Moisture.csv")



#Soil
tea$soiltemp_mean<-Plot_Variables_Soil$Mean_Temperature[match(tea$Plot,Plot_Variables_Soil$Plot)]
tea$soiltemp_GDD5<-Plot_Variables_Soil$GDD_5[match(tea$Plot,Plot_Variables_Soil$Plot)]
tea$soiltemp_source<-Plot_Variables_Soil$source[match(tea$Plot,Plot_Variables_Soil$Plot)]

#Air
tea$airtemp_mean<-Plot_Variables_Air$Mean_Temperature[match(tea$Plot,Plot_Variables_Air$Plot)]
tea$airtemp_GDD5<-Plot_Variables_Air$GDD_5[match(tea$Plot,Plot_Variables_Air$Plot)]
tea$airtemp_source<-Plot_Variables_Air$source[match(tea$Plot,Plot_Variables_Air$Plot)]

#Moisture
tea$moisture_mean<-Plot_Variables_Moisture$Mean_moisture[match(tea$Plot,Plot_Variables_Moisture$Plot)]
tea$moisture_mean_source<-Plot_Variables_Moisture$source[match(tea$Plot,Plot_Variables_Moisture$Plot)]

moisture<-subset(tea,!is.na(moisture_mean))
#Convert HOBO to %


#NB - could decide to exclude all (online) weather station data at this point#

#2) CHELSA data ####
chelsa_year<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_1.tif")
#chelsa_summer<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_10.tif")
#chelsa_summer_tundra<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_temp_arctic_10corrected.tif")
#chelsa_winter<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_11.tif") 
chelsa_precip_year<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_12.tif")
#chelsa_precip_summer<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_18.tif")
#chelsa_precip_winter<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_19.tif")
#chelsa_precip_summer_tundra<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_precip_arctic_10corrected.tif")

#Crop for non-summer
chelsa_year_tundra<-crop(chelsa_year, extent(-180.000, 180, 45, 84))
chelsa_precip_year_tundra<-crop(chelsa_precip_year, extent(-180.000, 180, 45, 84))

#NB - May also want to create composite rasters - e.g. JJA, or !JJA / Spring

tea_coords<-cbind(tea$Lon, tea$Lat)

tea$CHELSA_year_temp<-extract(chelsa_year,tea_coords)/10
#tea$CHELSA_summer_temp<-extract(chelsa_summer,tea_coords)/10
#tea$CHELSA_winter_temp<-extract(chelsa_year,tea_coords)/10
tea$CHELSA_year_precip<-extract(chelsa_precip_year,tea_coords)/10
#tea$CHELSA_summer_precip<-extract(chelsa_precip_summer,tea_coords)/10
#tea$CHELSA_winter_precip<-extract(chelsa_precip_year,tea_coords)/10

#3) Soil moisture data
#ESA_year_NH<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_JJA.tif")
#ESA_year_SH<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_DJF_SH.tif")

ESA_year<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1978_2015_mean.tif")

#Add to tea object
tea$ESA_moisture<-extract(ESA_year,tea_coords)*100

##############Relationships############################

#Make sure only using control plots
ambient<-subset(tea,Treatment=="None")

#Split into seasons to make things easier

summer<-subset(ambient,Season=="Summer")
winter<-subset(ambient,Season=="Winter")
year<-subset(ambient,Season=="Year")

#Classic density plots####

all<-ggplot(tea, aes(x=Loss*100, y = ..density..,fill=factor(Tea_Type)))+
  geom_histogram(alpha=0.8,position = 'identity')+
  geom_density(alpha=0.25, adjust=3)+
  scale_x_continuous(limits = c(0,100))+
  scale_fill_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Desnity",x="% Mass Loss")+
  ggtitle("All incubations")

ambs<-ggplot(ambient, aes(x=Loss*100, y = ..density..,fill=factor(Tea_Type)))+
  geom_histogram(alpha=0.8,position = 'identity')+
  geom_density(alpha=0.25, adjust=3)+
  scale_x_continuous(limits = c(0,100))+
  scale_fill_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Desnity",x="% Mass Loss")+
  ggtitle("Ambient incubations")

year_dens<-ggplot(year, aes(x=Loss*100, y = ..density..,fill=factor(Tea_Type)))+
  geom_histogram(alpha=0.8,position = 'identity')+
  geom_density(alpha=0.25, adjust=3)+
  scale_x_continuous(limits = c(0,100))+
  scale_fill_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Desnity",x="% Mass Loss")+
  ggtitle("Year incubations")

grid.arrange(all,ambs,year_dens)

#pdf(file="scripts/users/hthomas/Output_Images/Tea/tea_dists.pdf", width = 3, height = 3)
ggplot(tea, aes(x=Loss*100, y = ..density..*100,fill=factor(Tea_Type)))+
  geom_histogram(alpha=0.8,position = 'identity')+
  geom_density(alpha=0.25, adjust=3)+
  scale_x_continuous(limits = c(0,100))+
  scale_fill_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Proportion of teabags (%)",x="Mass Loss (%)")+
  theme(legend.position="none")
#dev.off()

#Do teas correlate?####

tea_grouped_plots<-year %>%
  group_by(Site,Plot,Tea_Type,Region) %>%
  summarise(Loss = mean(Loss))

tea_grouped_red<-filter(tea_grouped_plots,Tea_Type == "Rooibos")
names(tea_grouped_red)[5]<-"Red"
tea_grouped_green<-filter(tea_grouped_plots,Tea_Type == "Green")
names(tea_grouped_green)[5]<-"Green"

tea_grouped_green$Red<-tea_grouped_red$Red[match(tea_grouped_green$Plot,tea_grouped_red$Plot)]

pdf(file="scripts/users/hthomas/Output_Images/Tea/tea_correlations_year.pdf", width = 8, height = 5)
ggplot(tea_grouped_green,aes(Red,Green))+
  geom_point(aes(colour = Region))+
  stat_smooth(method = "lm")+
  stat_smooth(aes(colour = Region),method = "lm", se=FALSE)+
  theme_bw()+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Green Tea",x="Rooibos Tea")+
  ggtitle("Rooibos vs green tea decomposition")
dev.off()

#Relationships: 1) Air Temp, 2) Soil Temp, 3) Soil Moisture, 4) CHELSA Temp####
#5) CHELSA Precip, 6) ESA Moisture####

#Seasons: i) Year, ii) Year, iii) Year

#Set season
season.list <- c("summer", "winter", "year")

#VARIABLES: Loss, Loss_Day, k, TBI_k, TBI_S####

#Set Variable
var.list <- c("Loss", "Loss_Day", "k", "TBI_k", "TBI_S")
plots <- list()

for(i in 1:5){
  #Get column number
  var.num<-which(colnames(year)==var.list[i])
  
  #1) AIR TEMP#################################################
  
  #Remove na values from season object
  season_narm<-get(season.list[3]) %>%
    filter(is.finite(get(season.list[3])[,var.num]),is.finite(airtemp_mean))
  
  #Define interest variable
  names(season_narm)[var.num]<-"var"
  
  #MCMC model
  MC_LM_air <- MCMCglmm(var ~ Tea_Type + airtemp_mean + Days, data = season_narm, family = "gaussian", pr=TRUE, nitt = 15000, burnin = 5000) #Run
  summary(MC_LM_air) #Summarise
  
  #Calculate mean burial length
  mean_burial<-mean(season_narm$Days)
  min_air<-min(season_narm$airtemp_mean,na.rm=TRUE)
  max_air<-max(season_narm$airtemp_mean,na.rm=TRUE)
  
  #Run for green
  MC_ML_air_g<-MCMCglmm(var ~ airtemp_mean + Days,  data = season_narm[season_narm$Tea_Type=="Green",], nitt = 15000, burnin = 5000)
  summary(MC_ML_air_g)
  
  #Model preditions
  mm.green <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ airtemp_mean + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <- mm %*% fixef(MC_ML_air_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_air_g)$solutions[,2]
  mm.green$phi <- mm %*% summary(MC_ML_air_g)$solutions[,3]
  
  #Run for rooibos
  MC_ML_air_r<-MCMCglmm(var ~ airtemp_mean + Days, data = season_narm[season_narm$Tea_Type=="Rooibos",], nitt = 15000, burnin = 5000)
  summary(MC_ML_air_r)
  
  #Model preditions
  mm.red <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ airtemp_mean + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <- mm %*% fixef(MC_ML_air_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_air_r)$solutions[,2]
  mm.red$phi <- mm %*% summary(MC_ML_air_r)$solutions[,3]
  
  #Create dataframe for plotting so ggplot doesn't overwrite
  plotting_data<-get(season.list[3]) #copy dataset
  names(plotting_data)[var.num]<-"var" #rename variable of interest
  
  # Graph
  #When recreating object with loop, use assign: assign(paste(season, tea variable, output))
  (sum_air <- ggplot()+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(airtemp_mean,var*100),method="lm",colour="forestgreen",size=0.6, fill = "darkgreen", alpha = 0.3)+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(airtemp_mean,var*100),method="lm",colour="red",size=0.6, fill = "red", alpha = 0.3)+
      #geom_ribbon(data = mm.red, mapping = aes(x = airtemp_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = airtemp_mean, y=var*100), colour="red3", size=1) +
      #geom_ribbon(data = mm.green, mapping = aes(x = airtemp_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
      #geom_line(data = mm.green, mapping = aes(x = airtemp_mean,y=var*100), colour="darkgreen", size=1)+
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(airtemp_mean,var*100),colour="green3",alpha=0.3,pch=16)+
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(airtemp_mean,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
      labs(y="% Mass Loss",x="Air Temeprature °C")+
      ggtitle("Measured Air Temperature"))
  
  
  assign(paste("air_fig",var.list[i],season.list[3],sep="_"),sum_air)
  
  #b) SOIL TEMP ONLY#################################################
  
  #Remove na values from season object
  season_narm<-get(season.list[3]) %>%
    filter(is.finite(get(season.list[3])[,var.num]),is.finite(soiltemp_mean))
  
  #Define interest variable
  names(season_narm)[var.num]<-"var"
  
  #Create model
  MC_LM_soil <- MCMCglmm(var ~ Tea_Type + soiltemp_mean + Days, data = season_narm, family = "gaussian", pr=TRUE, nitt = 15000, burnin = 5000) #Run
  summary(MC_LM_soil) #Summarise
  
  #Calculate mean burial length
  mean_burial<-mean(season_narm$Days)
  min_soil<-min(season_narm$soiltemp_mean,na.rm=TRUE)
  max_soil<-max(season_narm$soiltemp_mean,na.rm=TRUE)
  
  #Run for green
  MC_ML_soil_g<-MCMCglmm(var ~ soiltemp_mean + Days, data = season_narm[season_narm$Tea_Type=="Green",], nitt = 15000, burnin = 5000)
  summary(MC_ML_soil_g)
  
  #Model preditions
  mm.green <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ soiltemp_mean + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <- mm %*% fixef(MC_ML_soil_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_soil_g)$solutions[,2]
  mm.green$phi <- mm %*% summary(MC_ML_soil_g)$solutions[,3]
  
  #Run for rooibos
  MC_ML_soil_r<-MCMCglmm(var ~ soiltemp_mean + Days, data = season_narm[season_narm$Tea_Type=="Rooibos",], nitt = 15000, burnin = 5000)
  summary(MC_ML_soil_r)
  
  #Model preditions
  mm.red <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ soiltemp_mean + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <- mm %*% fixef(MC_ML_soil_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_soil_r)$solutions[,2]
  mm.red$phi <- mm %*% summary(MC_ML_soil_r)$solutions[,3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/soil_temp_MCMC.pdf", width = 3, height = 3)
  (sum_soil_t <- ggplot()+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(soiltemp_mean,var*100),method="lm",colour="forestgreen",size=0.6, fill = "darkgreen", alpha = 0.3)+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(soiltemp_mean,var*100),method="lm",colour="red",size=0.6, fill = "red", alpha = 0.3)+
      #geom_ribbon(data = mm.red, mapping = aes(x = soiltemp_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = soiltemp_mean, y=var*100), colour="red3", size=1) +
      #geom_ribbon(data = mm.green, mapping = aes(x = soiltemp_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
      #geom_line(data = mm.green, mapping = aes(x = soiltemp_mean,y=var*100), colour="darkgreen", size=1) +
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(soiltemp_mean,var*100),colour="green3",alpha=0.3,pch=16)+
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(soiltemp_mean,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
      labs(y="% Mass Loss",x="Soil Temeprature °C")+
      ggtitle("Measured Soil Temperature"))
  #dev.off()
  
  assign(paste("soil_t_fig",var.list[i],season.list[3],sep="_"),sum_soil_t)
  
  
  #c) SOIL MOISTURE ONLY###################################################
  
  #Remove na values from season object
  season_narm<-get(season.list[3]) %>%
    filter(is.finite(get(season.list[3])[,var.num]),is.finite(moisture_mean))
  
  #Define interest variable
  names(season_narm)[var.num]<-"var"
  
  #Create model
  MC_LM_moisture <- MCMCglmm(var ~ Tea_Type + moisture_mean + Days, data = season_narm, family = "gaussian", pr=TRUE, nitt = 15000, burnin = 5000)
  summary(MC_LM_moisture)
  
  #Calculate mean burial length
  mean_burial<-mean(season_narm$Days)
  min_moisture<-min(season_narm$moisture_mean,na.rm=TRUE)
  max_moisture<-max(season_narm$moisture_mean,na.rm=TRUE)
  
  #Run for green
  MC_ML_moisture_g<-MCMCglmm(var ~ moisture_mean + Days, data = season_narm[season_narm$Tea_Type=="Green",], nitt = 15000, burnin = 5000)
  summary(MC_ML_moisture_g)
  
  #Model preditions
  mm.green <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ moisture_mean + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <- mm %*% fixef(MC_ML_moisture_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_moisture_g)$solutions[,2]
  mm.green$phi <- mm %*% summary(MC_ML_moisture_g)$solutions[,3]
  
  #Run for rooibos
  MC_ML_moisture_r<-MCMCglmm(var ~ moisture_mean + Days, data = season_narm[season_narm$Tea_Type=="Rooibos",], nitt = 15000, burnin = 5000)
  summary(MC_ML_moisture_r)
  
  #Model preditions
  mm.red <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ moisture_mean + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <- mm %*% fixef(MC_ML_moisture_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_moisture_r)$solutions[,2]
  mm.red$phi <- mm %*% summary(MC_ML_moisture_r)$solutions[,3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (sum_soil_m <- ggplot()+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(moisture_mean,var*100),method="lm",colour="forestgreen",size=0.6, fill = "darkgreen", alpha = 0.3)+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(moisture_mean,var*100),method="lm",colour="red",size=0.6, fill = "red", alpha = 0.3)+
      #geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = moisture_mean, y=var*100), colour="red3", size=1) +
      #geom_ribbon(data = mm.green, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
      #geom_line(data = mm.green, mapping = aes(x = moisture_mean,y=var*100), colour="darkgreen", size=1) +
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(moisture_mean,var*100),colour="green3",alpha=0.3,pch=16)+
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(moisture_mean,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
      labs(y="% Mass Loss",x="Soil Moisture Content (%)")+
      ggtitle("Measured Soil Moisture"))
  #dev.off()
  
  assign(paste("soil_m_fig",var.list[i],season.list[3],sep="_"),sum_soil_m)
  
  #d) CHELSA AIR###################################################
  
  #Remove na values from season object
  season_narm<-get(season.list[3]) %>%
    filter(is.finite(get(season.list[3])[,var.num]))
  
  #Define interest variable
  names(season_narm)[var.num]<-"var"
  
  #Create model
  MC_LM_CHELSA_temp <- MCMCglmm(var ~ Tea_Type + CHELSA_year_temp  + Days, data = season_narm, family = "gaussian", pr=TRUE, nitt = 15000, burnin = 5000)
  summary(MC_LM_CHELSA_temp)
  
  #Calculate mean burial length
  mean_burial<-mean(season_narm$Days)
  min_CH_temp<-min(season_narm$CHELSA_year_temp,na.rm=TRUE)
  max_CH_temp<-max(season_narm$CHELSA_year_temp,na.rm=TRUE)
  
  #Run for green
  MC_ML_CH_T_g<-MCMCglmm(var ~ CHELSA_year_temp  + Days, data = season_narm[season_narm$Tea_Type=="Green",], nitt = 15000, burnin = 5000)
  summary(MC_ML_CH_T_g)
  
  #Model preditions
  mm.green <- expand.grid(CHELSA_year_temp = seq(min_CH_temp, max_CH_temp, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ CHELSA_year_temp + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <- mm %*% fixef(MC_ML_CH_T_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_CH_T_g)$solutions[,2]
  mm.green$phi <- mm %*% summary(MC_ML_CH_T_g)$solutions[,3]
  
  #Run for rooibos
  MC_ML_CH_T_r<-MCMCglmm(var ~ CHELSA_year_temp  + Days, data = season_narm[season_narm$Tea_Type=="Rooibos",], nitt = 15000, burnin = 5000)
  summary(MC_ML_CH_T_r)
  
  #Model preditions
  mm.red <- expand.grid(CHELSA_year_temp = seq(min_CH_temp, max_CH_temp, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ CHELSA_year_temp + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <- mm %*% fixef(MC_ML_CH_T_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_CH_T_r)$solutions[,2]
  mm.red$phi <- mm %*% summary(MC_ML_CH_T_r)$solutions[,3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (sum_CH_T <- ggplot()+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(CHELSA_year_temp,var*100),method="lm",colour="forestgreen",size=0.6, fill = "darkgreen", alpha = 0.3)+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(CHELSA_year_temp,var*100),method="lm",colour="red",size=0.6, fill = "red", alpha = 0.3)+
      #geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_year_temp, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = CHELSA_year_temp, y=var*100), colour="red3", size=1) +
      #geom_ribbon(data = mm.green, mapping = aes(x = CHELSA_year_temp, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
      #geom_line(data = mm.green, mapping = aes(x = CHELSA_year_temp,y=var*100), colour="darkgreen", size=1) +
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(CHELSA_year_temp,var*100),colour="green3",alpha=0.3,pch=16)+
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(CHELSA_year_temp,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
      labs(y="% Mass Loss",x="CHELSA Year Temperature (°C)")+
      ggtitle("CHELSA Year Temperature"))
  #dev.off()
  
  assign(paste("CH_t_fig",var.list[i],season.list[3],sep="_"),sum_CH_T)
  
  #e) CHELSA PRECIP###################################################
  
  #Remove na values from season object
  season_narm<-get(season.list[3]) %>%
    filter(is.finite(get(season.list[3])[,var.num]))
  
  #Define interest variable
  names(season_narm)[var.num]<-"var"
  
  #Create model
  MC_LM_CHELSA_precip <- MCMCglmm(var ~ Tea_Type + CHELSA_year_precip  + Days, data = season_narm, family = "gaussian", pr=TRUE, nitt = 15000, burnin = 5000)
  summary(MC_LM_CHELSA_precip)
  
  #Calculate mean burial length
  mean_burial<-mean(season_narm$Days)
  min_CH_precip<-min(season_narm$CHELSA_year_precip,na.rm=TRUE)
  max_CH_precip<-max(season_narm$CHELSA_year_precip,na.rm=TRUE)
  
  #Run for green
  MC_ML_CH_P_g<-MCMCglmm(var ~ CHELSA_year_precip  + Days, data = season_narm[season_narm$Tea_Type=="Green",], nitt = 15000, burnin = 5000)
  summary(MC_ML_CH_P_g)
  
  #Model preditions
  mm.green <- expand.grid(CHELSA_year_precip = seq(min_CH_precip, max_CH_precip, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ CHELSA_year_precip + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <- mm %*% fixef(MC_ML_CH_P_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_CH_P_g)$solutions[,2]
  mm.green$phi <- mm %*% summary(MC_ML_CH_P_g)$solutions[,3]
  
  #Run for rooibos
  MC_ML_CH_P_r<-MCMCglmm(var ~ CHELSA_year_precip  + Days, data = season_narm[season_narm$Tea_Type=="Rooibos",], nitt = 15000, burnin = 5000)
  summary(MC_ML_CH_P_r)
  
  #Model preditions
  mm.red <- expand.grid(CHELSA_year_precip = seq(min_CH_precip, max_CH_precip, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ CHELSA_year_precip + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <- mm %*% fixef(MC_ML_CH_P_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_CH_P_r)$solutions[,2]
  mm.red$phi <- mm %*% summary(MC_ML_CH_P_r)$solutions[,3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (sum_CH_P <- ggplot()+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(CHELSA_year_precip,var*100),method="lm",colour="forestgreen",size=0.6, fill = "darkgreen", alpha = 0.3)+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(CHELSA_year_precip,var*100),method="lm",colour="red",size=0.6, fill = "red", alpha = 0.3)+
      #geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_year_precip, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = CHELSA_year_precip, y=var*100), colour="red3", size=1) +
      #geom_ribbon(data = mm.green, mapping = aes(x = CHELSA_year_precip, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
      #geom_line(data = mm.green, mapping = aes(x = CHELSA_year_precip,y=var*100), colour="darkgreen", size=1) +
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(CHELSA_year_precip,var*100),colour="green3",alpha=0.3,pch=16)+
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(CHELSA_year_precip,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
      labs(y="% Mass Loss",x="CHELSA Year Precipitation (mm)")+
      ggtitle("CHELSA Year Precipitation"))
  #dev.off()
  
  assign(paste("CH_p_fig",var.list[i],season.list[3],sep="_"),sum_CH_P)
  
  #f) ESA Moisture###################################################
  
  #Remove na values from season object
  season_narm<-get(season.list[3]) %>%
    filter(is.finite(get(season.list[3])[,var.num]),is.finite(ESA_moisture))
  
  
  #Define interest variable
  names(season_narm)[var.num]<-"var"
  
  #Create model
  MC_LM_ESA_moist <- MCMCglmm(var ~ Tea_Type + ESA_moisture  + Days, data = season_narm, family = "gaussian", pr=TRUE, nitt = 15000, burnin = 5000)
  summary(MC_LM_ESA_moist)
  
  #Calculate mean burial length
  mean_burial<-mean(season_narm$Days)
  min_ESA_moist<-min(season_narm$ESA_moisture,na.rm=TRUE)
  max_ESA_moist<-max(season_narm$ESA_moisture,na.rm=TRUE)
  
  #Run for green
  MC_ML_ESA_moist_g<-MCMCglmm(var ~ ESA_moisture  + Days, data = season_narm[season_narm$Tea_Type=="Green",], nitt = 15000, burnin = 5000)
  summary(MC_ML_ESA_moist_g)
  
  #Model preditions
  mm.green <- expand.grid(ESA_moisture = seq(min_ESA_moist, max_ESA_moist, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ ESA_moisture + Days), mm.green)  # Create matrix of relevant effect sizes
  mm.green$var <- mm %*% fixef(MC_ML_ESA_moist_g, use = "mean")  # Calculate based on the relevant effect sizes
  mm.green$plo <- mm %*% summary(MC_ML_ESA_moist_g)$solutions[,2]
  mm.green$phi <- mm %*% summary(MC_ML_ESA_moist_g)$solutions[,3]
  
  #Run for rooibos
  MC_ML_ESA_moist_r<-MCMCglmm(var ~ ESA_moisture  + Days, data = season_narm[season_narm$Tea_Type=="Rooibos",], nitt = 15000, burnin = 5000)
  summary(MC_ML_ESA_moist_r)
  
  #Model preditions
  mm.red <- expand.grid(ESA_moisture = seq(min_ESA_moist, max_ESA_moist, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
  mm <- model.matrix(terms(var ~ ESA_moisture + Days), mm.red)  # Create matrix of relevant effect sizes
  mm.red$var <- mm %*% fixef(MC_ML_ESA_moist_r, use = "mean")  # Calculate based on the relevant effect sizes
  mm.red$plo <- mm %*% summary(MC_ML_ESA_moist_r)$solutions[,2]
  mm.red$phi <- mm %*% summary(MC_ML_ESA_moist_r)$solutions[,3]
  
  # Graph
  #pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (sum_ESA_moist <- ggplot()+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(ESA_moisture,var*100),method="lm",colour="forestgreen",size=0.6, fill = "darkgreen", alpha = 0.3)+
      stat_smooth(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(ESA_moisture,var*100),method="lm",colour="red",size=0.6, fill = "red", alpha = 0.3)+
      #geom_ribbon(data = mm.red, mapping = aes(x = ESA_moisture, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
      #geom_line(data = mm.red, mapping = aes(x = ESA_moisture, y=var*100), colour="red3", size=1) +
      #geom_ribbon(data = mm.green, mapping = aes(x = ESA_moisture, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
      #geom_line(data = mm.green, mapping = aes(x = ESA_moisture,y=var*100), colour="darkgreen", size=1) +
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Green",],aes(ESA_moisture,var*100),colour="green3",alpha=0.3,pch=16)+
      geom_point(data=plotting_data[plotting_data$Tea_Type=="Rooibos",],aes(ESA_moisture,var*100),colour="indianred1",alpha=0.3,pch=16)+
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
      labs(y="% Mass Loss",x="ESA Soil Moisture (%)")+
      ggtitle("ESA Soil Moisture"))
  #dev.off()
  
  assign(paste("ESA_moist_fig",var.list[i],season.list[3],sep="_"),sum_ESA_moist)
}

grid.arrange(air_fig_Loss_year, soil_t_fig_Loss_year, soil_m_fig_Loss_year, CH_p_fig_Loss_year, CH_t_fig_Loss_year, ESA_moist_fig_Loss_year)


#Overall model (Loss - year)####

year_narm<-year%>%
  filter(is.finite(Loss),is.finite(soiltemp_mean),is.finite(moisture_mean), is.finite(Days))

Loss_year<-MCMCglmm(scale(Loss) ~ Tea_Type + scale(soiltemp_mean) + scale(moisture_mean) + scale(Days), random = ~Region,  data = year_narm, nitt = 15000, burnin = 5000)
summary(Loss_year)

#NB - I'm not sure whether it is better to use days as a fixed or random effect

Loss_year_fixefs<-as.data.frame(abs(summary(Loss_year)$solution[,1]))
names(Loss_year_fixefs)<-"Effect_Size"
rownames(Loss_year_fixefs)<-c("Intercept","Tea Type","Soil Temperature","Soil Moisture", "Incubation Length")
Loss_year_fixefs$Variable<-rownames(Loss_year_fixefs)
Loss_year_fixefs$lower<-abs(summary(Loss_year)$solution[,2])
Loss_year_fixefs$upper<-abs(summary(Loss_year)$solution[,3])

pdf(file="scripts/users/hthomas/Output_Images/Tea/Effect_sizes_year.pdf", width = 4, height = 3)
(year_effects<-ggplot(Loss_year_fixefs[-1,], aes(Variable,Effect_Size))+
    geom_errorbar(data=Loss_year_fixefs[-1,], mapping=aes(x=Variable, ymin=upper, ymax=lower), linetype=1, width=0, size=1)+
    coord_flip()+
    geom_hline(yintercept=0, linetype="dashed")+
    theme_bw()+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(y = "Standardised Effect Size"))
dev.off()


#Overall model (Loss - all)####

ambient_narm<-ambient%>%
  filter(is.finite(Loss),is.finite(soiltemp_mean),is.finite(moisture_mean), is.finite(Days))

Loss_year<-MCMCglmm(scale(Loss) ~ Tea_Type + scale(soiltemp_mean) + scale(moisture_mean)  + scale(Days), random = ~Region + Season , data = ambient_narm, nitt = 10000, burnin = 2000)
summary(Loss_year)

#NB - I'm not sure whether it is better to use days as a fixed or random effect

Loss_year_fixefs<-as.data.frame(abs(summary(Loss_year)$solution[,1]))
names(Loss_year_fixefs)<-"Effect_Size"
rownames(Loss_year_fixefs)<-c("Intercept","Tea Type","Soil Temperature","Soil Moisture", "Incubation Length")
Loss_year_fixefs$Variable<-rownames(Loss_year_fixefs)
Loss_year_fixefs$lower<-abs(summary(Loss_year)$solution[,2])
Loss_year_fixefs$upper<-abs(summary(Loss_year)$solution[,3])


ggplot(Loss_year_fixefs[-1,], aes(Variable,Effect_Size))+
  geom_errorbar(data=Loss_year_fixefs[-1,], mapping=aes(x=Variable, ymin=upper, ymax=lower), linetype=1, width=0, size=1)+
  coord_flip()+
  ylim(0,2)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Effect Sizes (Year Mass Loss)")+
  labs(y = "Standardised Effect Size")


#############With CHELSA data##########################################################

#Chelsa year data vs measured year data - air####

#Remove NAs
year_narm<-year%>%
  filter(is.finite(CHELSA_year_temp),is.finite(airtemp_mean))

#Find only unique plot temperatures
year_narm_unique<-year_narm[!duplicated(year_narm[c("CHELSA_year_temp","airtemp_mean")]),]

#Take extent
min_air<-min(year_narm_unique$airtemp_mean,na.rm=TRUE)
max_air<-max(year_narm_unique$airtemp_mean,na.rm=TRUE)

#Run model
lm_air_chelsa_year<-lmer(CHELSA_year_temp ~ airtemp_mean + (1|Region), data=year_narm_unique)
#lm_air_chelsa_year<-lm(CHELSA_year_temp ~ airtemp_mean, data=year_narm_unique)
summary(lm_air_chelsa_year); r.squaredGLMM(lm_air_chelsa_year)

# Predictions
mm.air <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), CHELSA_year_temp = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_air_chelsa_year), mm.air)  # Create matrix of relevant effect sizes
mm.air$CHELSA_year_temp <- mm %*% fixef(lm_air_chelsa_year)  # Calculate height based on the relevant effect sizes
pvar.mm.air <- diag(mm %*% tcrossprod(vcov(lm_air_chelsa_year), mm))
mm.air <- data.frame(mm.air, plo = mm.air$CHELSA_year_temp-1.96*sqrt(pvar.mm.air),
                     phi = mm.air$CHELSA_year_temp+1.96*sqrt(pvar.mm.air))  # Add errors

(air<-ggplot(year_narm_unique, aes(airtemp_mean, CHELSA_year_temp))+
    geom_ribbon(data = mm.air, mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi), alpha = 0.3) +
    geom_line(data = mm.air, mapping = aes(x = airtemp_mean), colour="red", size=1) +
    #stat_smooth(method = "lm", colour = "red")+
    geom_jitter(width = 0.1, height = 0.1, alpha=0.6, size=2, pch=16, colour="red")+
    theme_bw()+
    geom_abline(slope=1, intercept=0, linetype = "dashed")+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(y="CHELSA Air Temperature °C (1979-2013)",x="Measured Air Temeprature °C")+
    ggtitle("Measured vs CHELSA - Air"))

#Chelsa year data vs measured year data - soil####

#Remove NAs
year_narm<-year%>%
  filter(is.finite(CHELSA_year_temp),is.finite(soiltemp_mean))

#Find only unique plot temperatures
year_narm_unique<-year_narm[!duplicated(year_narm[c("CHELSA_year_temp","soiltemp_mean")]),]

#Take extent
min_soil<-min(year_narm_unique$soiltemp_mean,na.rm=TRUE)
max_soil<-max(year_narm_unique$soiltemp_mean,na.rm=TRUE)

#Run model
lm_soil_chelsa_year<-lmer(CHELSA_year_temp ~ soiltemp_mean + (1|Region), data=year_narm_unique)
#lm_soil_chelsa_year<-lm(CHELSA_year_temp ~ soiltemp_mean, data=year_narm_unique)
summary(lm_soil_chelsa_year); r.squaredGLMM(lm_soil_chelsa_year)

#Predictions
mm.soil <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), CHELSA_year_temp = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_soil_chelsa_year), mm.soil)  # Create matrix of relevant effect sizes
mm.soil$CHELSA_year_temp <- mm %*% fixef(lm_soil_chelsa_year)  # Calculate height based on the relevant effect sizes
pvar.mm.soil <- diag(mm %*% tcrossprod(vcov(lm_soil_chelsa_year), mm))
mm.soil <- data.frame(mm.soil, plo = mm.soil$CHELSA_year_temp-1.96*sqrt(pvar.mm.soil),
                      phi = mm.soil$CHELSA_year_temp+1.96*sqrt(pvar.mm.soil))  # Add errors

(soil<-ggplot(year_narm_unique, aes(soiltemp_mean, CHELSA_year_temp))+
    #stat_smooth(method = "lm", colour = "goldenrod")+
    geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), alpha = 0.3) +
    geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean), colour="goldenrod", size=1) +
    geom_jitter(width = 0.1, height = 0.1, alpha=0.6, size=2, pch=16, colour="goldenrod")+
    theme_bw()+
    geom_abline(slope=1, intercept=0, linetype = "dashed")+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(y="CHELSA Air Temperature °C (1979-2013)",x="Measured Soil Temeprature °C")+
    ggtitle("Measured vs CHELSA - Soil"))

#Chelsa year data vs measured year data - moisture####

#Remove NAs
year_narm<-year%>%
  filter(is.finite(CHELSA_year_precip),is.finite(moisture_mean))

#Find only unique plot temperatures
year_narm_unique<-year_narm[!duplicated(year_narm[c("CHELSA_year_precip","moisture_mean")]),]

#Take extent
min_moisture<-min(year_narm_unique$moisture_mean,na.rm=TRUE)
max_moisture<-max(year_narm_unique$moisture_mean,na.rm=TRUE)

#Run model
lm_moisture_chelsa_year<-lmer(CHELSA_year_precip ~ moisture_mean + (1|Region), data=year_narm_unique)
#lm_moisture_chelsa_year<-lm(CHELSA_year_precip ~ moisture_mean, data=year_narm_unique)
summary(lm_moisture_chelsa_year); r.squaredGLMM(lm_moisture_chelsa_year)

# Predictions
mm.moisture <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), CHELSA_year_precip = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_moisture_chelsa_year), mm.moisture)  # Create matrix of relevant effect sizes
mm.moisture$CHELSA_year_precip <- mm %*% fixef(lm_moisture_chelsa_year)  # Calculate height based on the relevant effect sizes
pvar.mm.moisture <- diag(mm %*% tcrossprod(vcov(lm_moisture_chelsa_year), mm))
mm.moisture <- data.frame(mm.moisture, plo = mm.moisture$CHELSA_year_precip-1.96*sqrt(pvar.mm.moisture),
                          phi = mm.moisture$CHELSA_year_precip+1.96*sqrt(pvar.mm.moisture))  # Add errors

(precip<-ggplot(year_narm_unique, aes(moisture_mean, CHELSA_year_precip))+
    #geom_ribbon(data = mm.moisture, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), alpha = 0.3) +
    #geom_line(data = mm.moisture, mapping = aes(x = moisture_mean), colour="dodgerblue", size=1) +
    stat_smooth(method = "lm", colour = "dodgerblue")+
    geom_jitter(width = 0.1, height = 0.1, alpha=0.6, size=2, pch=16, colour="dodgerblue")+
    theme_bw()+
    #geom_abline(slope=1, intercept=0, linetype = "dashed")+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(y="CHELSA Precipitation (mm) (1979-2013)",x="Measured Soil Moisture (%)")+
    ggtitle("Measured vs CHELSA - moisture"))

#ESA year data vs measured year data - moisture####

#Remove NAs
year_narm<-year%>%
  filter(is.finite(ESA_moisture),is.finite(moisture_mean))

#Find only unique plot temperatures
year_narm_unique<-year_narm[!duplicated(year_narm[c("ESA_moisture","moisture_mean")]),]

#Take extent
min_moisture<-min(year_narm_unique$moisture_mean,na.rm=TRUE)
max_moisture<-max(year_narm_unique$moisture_mean,na.rm=TRUE)

#Run model
lm_moisture_ESA_year<-lmer(ESA_moisture ~ moisture_mean + (1|Region), data=year_narm_unique)
#lm_moisture_ESA_year<-lm(ESA_moisture ~ moisture_mean, data=year_narm_unique)
summary(lm_moisture_ESA_year); r.squaredGLMM(lm_moisture_ESA_year)

# Predictions
mm.moisture <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), ESA_moisture = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_moisture_ESA_year), mm.moisture)  # Create matrix of relevant effect sizes
mm.moisture$ESA_moisture <- mm %*% fixef(lm_moisture_ESA_year)  # Calculate height based on the relevant effect sizes
pvar.mm.moisture <- diag(mm %*% tcrossprod(vcov(lm_moisture_ESA_year), mm))
mm.moisture <- data.frame(mm.moisture, plo = mm.moisture$ESA_moisture-1.96*sqrt(pvar.mm.moisture),
                          phi = mm.moisture$ESA_moisture+1.96*sqrt(pvar.mm.moisture))  # Add errors

(moisture<-ggplot(year_narm_unique, aes(moisture_mean, ESA_moisture))+
    #geom_ribbon(data = mm.moisture, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), alpha = 0.3) +
    #geom_line(data = mm.moisture, mapping = aes(x = moisture_mean), colour="blue", size=1) +
    stat_smooth(method = "lm", colour = "blue")+
    geom_jitter(width = 0.1, height = 0.1, alpha=0.6, size=2, pch=16, colour="blue")+
    theme_bw()+
    #geom_abline(slope=1, intercept=0, linetype = "dashed")+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(y="ESA Soil Moisture (%) (1979-2013)",x="Measured Soil Moisture (%)")+
    ggtitle("Measured vs ESA - moisture"))

grid.arrange(air,soil,precip,moisture,ncol=4)



#Create blank
library(grid)
blank <- grid.rect(gp=gpar(col="white"))

#Combine figures######
pdf(file="scripts/users/hthomas/Output_Images/Tea/Env_vars_year.pdf", width = 12.5, height = 10)
grid.arrange(air_fig_Loss_year,soil_t_fig_Loss_year,soil_m_fig_Loss_year,soil_m_fig_Loss_year,
             CH_t_fig_Loss_year,CH_t_fig_Loss_year,CH_p_fig_Loss_year,ESA_moist_fig_Loss_year,
             air,soil,precip,moisture,ncol=4)
dev.off()


#Create effect-size figure
year_dens<-year_dens +
  theme(legend.position = "none")
year_dens <- ggplot_gtable(ggplot_build(year_dens))

year_effects <- ggplot_gtable(ggplot_build(year_effects))

maxWidth = unit.pmax(year_dens$widths[2:3], year_effects$widths[2:3])

year_dens$widths[2:3] <- maxWidth
year_effects$widths[2:3] <- maxWidth

pdf(file="scripts/users/hthomas/Output_Images/Tea/Env_vars_effects_year.pdf", width = 3, height = 3)
grid.arrange(year_dens,year_effects,nrow=2)
dev.off()


####WITHIN-SITE ANALYSIS####---------------------------------------------------

#Normalise everything within each site
ambient_normal <- ambient %>%
  group_by(Region,Tea_Type,Season) %>%
  mutate(Loss_normal = scale(Loss), 
         airtemp_mean_normal = scale(airtemp_mean),
         soiltemp_mean_normal = scale(soiltemp_mean),
         moisture_mean_normal = scale(moisture_mean),
         CHELSA_year_temp_normal = scale(CHELSA_year_temp),
         CHELSA_year_precip_normal = scale(CHELSA_year_precip),
         ESA_moisture_normal = scale(ESA_moisture))

year_normal<-subset(ambient_normal,Season=="Year")

#Run within-site models####≈
#a) Air temp
season_narm<-year_normal %>%
  filter(is.finite(Loss_normal),is.finite(airtemp_mean_normal))

#air_normal<-MCMCglmm(Loss_normal ~ airtemp_mean_normal, random = ~Region + Site +Plot, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
air_normal<-lmer(Loss_normal ~ airtemp_mean_normal + (1|Region/Site/Plot), data = season_narm)
summary(air_normal)

#Take extent
min_air<-min(season_narm$airtemp_mean_normal,na.rm=TRUE)
max_air<-max(season_narm$airtemp_mean_normal,na.rm=TRUE)

# #Model predictions
# mm.air <- expand.grid(airtemp_mean_normal = seq(min_air, max_air, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ airtemp_mean_normal), mm.air)  # Create matrix of relevant effect sizes
# mm.air$Loss_normal <- mm %*% fixef(air_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.air$plo <- mm %*% summary(air_normal)$solutions[,2]
# mm.air$phi <- mm %*% summary(air_normal)$solutions[,3]

# Predictions
mm.air <- expand.grid(airtemp_mean_normal = seq(min_air, max_air, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(air_normal), mm.air)  # Create matrix of relevant effect sizes
mm.air$Loss_normal <- mm %*% fixef(air_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.air <- diag(mm %*% tcrossprod(vcov(air_normal), mm))
mm.air <- data.frame(mm.air, plo = mm.air$Loss_normal-1.96*sqrt(pvar.mm.air),
                     phi = mm.air$Loss_normal+1.96*sqrt(pvar.mm.air))  # Add errors


#Test plot - tea vs air temp
(air_normal_fig<-ggplot(year_normal, aes(airtemp_mean_normal, Loss_normal))+
    geom_point(aes(colour = factor(Region)), alpha = 0.5, pch = 16)+
    geom_ribbon(data = mm.air, mapping = aes(x = airtemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "red") +
    geom_line(data = mm.air, mapping = aes(x = airtemp_mean_normal, y=Loss_normal), size=1, colour = "red") +
    theme_classic()+
    #stat_smooth(method = "lm", aes(colour = Region))+
    #stat_smooth(method = "lm")+
    ggtitle("Air Temperature (within sites)")+
    theme(legend.position = "none")+
    labs(x = "Normalilsed Air Temperature", y = "Normalised Mass Loss_normal"))

#b) Soil temp
season_narm<-year_normal %>%
  filter(is.finite(Loss_normal),is.finite(soiltemp_mean_normal))

soil_normal<-lmer(Loss_normal ~ soiltemp_mean_normal + (1|Region/Site/Plot), data = season_narm)
#soil_normal<-MCMCglmm(Loss_normal ~ soiltemp_mean_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(soil_normal)

#Take extent
min_soil<-min(season_narm$soiltemp_mean_normal,na.rm=TRUE)
max_soil<-max(season_narm$soiltemp_mean_normal,na.rm=TRUE)

# #Model predictions
# mm.soil <- expand.grid(soiltemp_mean_normal = seq(min_soil, max_soil, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ soiltemp_mean_normal), mm.soil)  # Create matrix of relevant effect sizes
# mm.soil$Loss_normal <- mm %*% fixef(soil_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.soil$plo <- mm %*% summary(soil_normal)$solutions[,2]
# mm.soil$phi <- mm %*% summary(soil_normal)$solutions[,3]

# Predictions
mm.soil <- expand.grid(soiltemp_mean_normal = seq(min_soil, max_soil, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(soil_normal), mm.soil)  # Create matrix of relevant effect sizes
mm.soil$Loss_normal <- mm %*% fixef(soil_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.soil <- diag(mm %*% tcrossprod(vcov(soil_normal), mm))
mm.soil <- data.frame(mm.soil, plo = mm.soil$Loss_normal-1.96*sqrt(pvar.mm.soil),
                      phi = mm.soil$Loss_normal+1.96*sqrt(pvar.mm.soil))  # Add errors


#Test plot - tea vs soil temp
(soil_normal_fig<-ggplot()+
    geom_point(data = year_normal, aes(soiltemp_mean_normal, Loss_normal,colour = factor(Region)), alpha = 0.5, pch = 16)+
    geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "goldenrod") +
    geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean_normal, y=Loss_normal), size=1, colour = "goldenrod") +
    theme_classic()+
    ggtitle("Soil Temperature (within sites)")+
    theme(legend.position = "none")+
    labs(x = "Normalilsed Soil Temperature", y = "Normalised Mass Loss_normal"))

#c) Soil Moisture
season_narm<-year_normal %>%
  filter(is.finite(Loss_normal),is.finite(moisture_mean_normal))

moisture_normal<-lmer(Loss_normal ~ moisture_mean_normal + (1|Plot), data = season_narm)
#moisture_normal<-MCMCglmm(Loss_normal ~ moisture_mean_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(moisture_normal)

#Take extent
min_moisture<-min(season_narm$moisture_mean_normal,na.rm=TRUE)
max_moisture<-max(season_narm$moisture_mean_normal,na.rm=TRUE)

# #Model predictions
# mm.moisture <- expand.grid(moisture_mean_normal = seq(min_moisture, max_moisture, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ moisture_mean_normal), mm.moisture)  # Create matrix of relevant effect sizes
# mm.moisture$Loss_normal <- mm %*% fixef(moisture_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.moisture$plo <- mm %*% summary(moisture_normal)$solutions[,2]
# mm.moisture$phi <- mm %*% summary(moisture_normal)$solutions[,3]

# Predictions
mm.moisture <- expand.grid(moisture_mean_normal = seq(min_moisture, max_moisture, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(moisture_normal), mm.moisture)  # Create matrix of relevant effect sizes
mm.moisture$Loss_normal <- mm %*% fixef(moisture_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.moisture <- diag(mm %*% tcrossprod(vcov(moisture_normal), mm))
mm.moisture <- data.frame(mm.moisture, plo = mm.moisture$Loss_normal-1.96*sqrt(pvar.mm.moisture),
                          phi = mm.moisture$Loss_normal+1.96*sqrt(pvar.mm.moisture))  # Add errors


#Test plot - tea vs moisture temp
(moisture_normal_fig<-ggplot()+
    geom_point(data = year_normal, aes(moisture_mean_normal, Loss_normal,colour = factor(Region)), alpha = 0.5, pch = 16)+
    geom_ribbon(data = mm.moisture, mapping = aes(x = moisture_mean_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "dodgerblue") +
    geom_line(data = mm.moisture, mapping = aes(x = moisture_mean_normal, y=Loss_normal), size=1, colour = "dodgerblue") +
    theme_classic()+
    #stat_smooth(data = year_normal, aes(moisture_mean_normal, Loss_normal), method = "lm", alpha = 0.3, fill = "dodgerblue", size=1, colour = "dodgerblue")+
    ggtitle("Soil Moisture (within sites)")+
    theme(legend.position = "none")+
    labs(x = "Normalilsed Soil Moisture", y = "Normalised Mass Loss_normal"))

#d) CHELSA Temperatire
season_narm<-year_normal %>%
  filter(is.finite(Loss_normal),is.finite(CHELSA_year_temp_normal))

CH_t_normal<-lmer(Loss_normal ~ CHELSA_year_temp_normal + (1|Region/Site/Plot), data = season_narm)
#CH_t_normal<-MCMCglmm(Loss_normal ~ CHELSA_year_temp_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(CH_t_normal)

#Take extent
min_CH_t<-min(season_narm$CHELSA_year_temp_normal,na.rm=TRUE)
max_CH_t<-max(season_narm$CHELSA_year_temp_normal,na.rm=TRUE)

# #Model predictions
# mm.CH_t <- expand.grid(CHELSA_year_temp_normal = seq(min_CH_t, max_CH_t, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ CHELSA_year_temp_normal), mm.CH_t)  # Create matrix of relevant effect sizes
# mm.CH_t$Loss_normal <- mm %*% fixef(CH_t_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.CH_t$plo <- mm %*% summary(CH_t_normal)$solutions[,2]
# mm.CH_t$phi <- mm %*% summary(CH_t_normal)$solutions[,3]

# Predictions
mm.CH_t <- expand.grid(CHELSA_year_temp_normal = seq(min_CH_t, max_CH_t, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(CH_t_normal), mm.CH_t)  # Create matrix of relevant effect sizes
mm.CH_t$Loss_normal <- mm %*% fixef(CH_t_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.CH_t <- diag(mm %*% tcrossprod(vcov(CH_t_normal), mm))
mm.CH_t <- data.frame(mm.CH_t, plo = mm.CH_t$Loss_normal-1.96*sqrt(pvar.mm.CH_t),
                      phi = mm.CH_t$Loss_normal+1.96*sqrt(pvar.mm.CH_t))  # Add errors


#Test plot - tea vs moisture temp
(CH_t_normal_fig<-ggplot()+
    geom_point(data = year_normal, aes(CHELSA_year_temp_normal, Loss_normal,colour = factor(Region)), alpha = 0.5, pch = 16)+
    geom_ribbon(data = mm.CH_t, mapping = aes(x = CHELSA_year_temp_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "red") +
    geom_line(data = mm.CH_t, mapping = aes(x = CHELSA_year_temp_normal, y=Loss_normal), size=1, colour = "red") +
    theme_classic()+
    stat_smooth(method = "lm")+
    ggtitle("CHELSA Temperature (within sites)")+
    theme(legend.position = "none")+
    labs(x = "Normalilsed CHELSA Air Temperature", y = "Normalised Mass Loss_normal"))

#e) CHELSA Precip
season_narm<-year_normal %>%
  filter(is.finite(Loss_normal),is.finite(CHELSA_year_precip_normal))

CH_p_normal<-lmer(Loss_normal ~ CHELSA_year_precip_normal + (1|Region/Site/Plot), data = season_narm)
#CH_p_normal<-MCMCglmm(Loss_normal ~ CHELSA_year_precip_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(CH_p_normal)

#Take extent
min_CH_p<-min(season_narm$CHELSA_year_precip_normal,na.rm=TRUE)
max_CH_p<-max(season_narm$CHELSA_year_precip_normal,na.rm=TRUE)

# #Model predictions
# mm.CH_p <- expand.grid(CHELSA_year_precip_normal = seq(min_CH_p, max_CH_p, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ CHELSA_year_precip_normal), mm.CH_p)  # Create matrix of relevant effect sizes
# mm.CH_p$Loss_normal <- mm %*% fixef(CH_p_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.CH_p$plo <- mm %*% summary(CH_p_normal)$solutions[,2]
# mm.CH_p$phi <- mm %*% summary(CH_p_normal)$solutions[,3]

# Predictions
mm.CH_p <- expand.grid(CHELSA_year_precip_normal = seq(min_CH_p, max_CH_p, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(CH_p_normal), mm.CH_p)  # Create matrix of relevant effect sizes
mm.CH_p$Loss_normal <- mm %*% fixef(CH_p_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.CH_p <- diag(mm %*% tcrossprod(vcov(CH_p_normal), mm))
mm.CH_p <- data.frame(mm.CH_p, plo = mm.CH_p$Loss_normal-1.96*sqrt(pvar.mm.CH_p),
                      phi = mm.CH_p$Loss_normal+1.96*sqrt(pvar.mm.CH_p))  # Add errors


#Test plot - tea vs moisture precip
(CH_p_normal_fig<-ggplot()+
    geom_point(data = year_normal, aes(CHELSA_year_precip_normal, Loss_normal,colour = factor(Region)), alpha = 0.5, pch = 16)+
    geom_ribbon(data = mm.CH_p, mapping = aes(x = CHELSA_year_precip_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "dodgerblue") +
    geom_line(data = mm.CH_p, mapping = aes(x = CHELSA_year_precip_normal, y=Loss_normal), size=1, colour = "dodgerblue") +
    theme_classic()+
    stat_smooth(method = "lm")+
    ggtitle("CHELSA Precip (within sites)")+
    theme(legend.position = "none")+
    labs(x = "Normalilsed CHELSA Precipitation", y = "Normalised Mass Loss_normal"))

#f) ESA moisture
season_narm<-year_normal %>%
  filter(is.finite(Loss_normal),is.finite(ESA_moisture_normal))

ESA_m_normal<-lmer(Loss_normal ~ ESA_moisture_normal + (1|Region), data = season_narm)
#ESA_m_normal<-MCMCglmm(Loss_normal ~ ESA_moisture_normal, random = ~Region, data = season_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000)
summary(ESA_m_normal)

#Take extent
min_ESA_m<-min(season_narm$ESA_moisture_normal,na.rm=TRUE)
max_ESA_m<-max(season_narm$ESA_moisture_normal,na.rm=TRUE)

# #Model predictions
# mm.ESA_m <- expand.grid(ESA_moisture_normal = seq(min_ESA_m, max_ESA_m, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
# mm <- model.matrix(terms(Loss_normal ~ ESA_moisture_normal), mm.ESA_m)  # Create matrix of relevant effect sizes
# mm.ESA_m$Loss_normal <- mm %*% fixef(ESA_m_normal, use = "mean")  # Calculate based on the relevant effect sizes
# mm.ESA_m$plo <- mm %*% summary(ESA_m_normal)$solutions[,2]
# mm.ESA_m$phi <- mm %*% summary(ESA_m_normal)$solutions[,3]

# Predictions
mm.ESA_m <- expand.grid(ESA_moisture_normal = seq(min_ESA_m, max_ESA_m, 0.1), Loss_normal = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(ESA_m_normal), mm.ESA_m)  # Create matrix of relevant effect sizes
mm.ESA_m$Loss_normal <- mm %*% fixef(ESA_m_normal)  # Calculate height based on the relevant effect sizes
pvar.mm.ESA_m <- diag(mm %*% tcrossprod(vcov(ESA_m_normal), mm))
mm.ESA_m <- data.frame(mm.ESA_m, plo = mm.ESA_m$Loss_normal-1.96*sqrt(pvar.mm.ESA_m),
                       phi = mm.ESA_m$Loss_normal+1.96*sqrt(pvar.mm.ESA_m))  # Add errors


#Test plot - tea vs moisture precip
(ESA_m_normal_fig<-ggplot()+
    geom_point(data = year_normal, aes(ESA_moisture_normal, Loss_normal,colour = factor(Region)), alpha = 0.5, pch = 16)+
    geom_ribbon(data = mm.ESA_m, mapping = aes(x = ESA_moisture_normal, ymin = plo, ymax = phi), alpha = 0.3, fill = "blue") +
    geom_line(data = mm.ESA_m, mapping = aes(x = ESA_moisture_normal, y=Loss_normal), size=1, colour = "blue") +
    theme_classic()+
    stat_smooth(method = "lm")+
    ggtitle("ESA Soil Moisture (within sites)")+
    theme(legend.position = "none")+
    labs(x = "Normalilsed ESA Soil Moisture", y = "Normalised Mass Loss_normal"))


#Arrange plots
pdf(file="scripts/users/hthomas/Output_Images/Tea/normalised_relationships_year.pdf", width = 10, height =7.5)
grid.arrange(air_normal_fig, soil_normal_fig, moisture_normal_fig, CH_t_normal_fig, CH_p_normal_fig, ESA_m_normal_fig,nrow=2)
dev.off()
