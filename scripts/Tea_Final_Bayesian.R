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

#Set some custom functions#
`%notin%` <- function(x,y) !(x %in% y)
se <- function(x) sqrt(var(x, na.rm=T)/length(x))

#Import data####
tea<-read.csv(file="users/hthomas/data/Tea/teabag_data.csv")

tea_iceland <- tea %>% filter(Region == "Auokiluheioi")


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

ggplot(plot_means[plot_means$Season=="Summer",],aes(S,k))+
  geom_point(aes(colour=factor(Region)),alpha=0.5)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("S vs k - Summer")

#Add back to tea#
tea$TBI_S<-plot_means$S[match(tea$Plot_unique,plot_means$Plot_unique)]
tea$TBI_k<-plot_means$k[match(tea$Plot_unique,plot_means$Plot_unique)]

#Add climatic information####

#1) Field measured variables ####
##NB - these could be updated with 2017 values
#Add plot-measured variables####
Plot_Variables_Soil<-read.csv(file="users/hthomas/data/Tea/Env.Vars/Soil_Temps.csv")
Plot_Variables_Air<-read.csv(file="users/hthomas/data/Tea/Env.Vars/Air_Temps.csv")
Plot_Variables_Moisture<-read.csv(file="users/hthomas/data/Tea/Env.Vars/Moisture.csv")

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
#chelsa_year<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_1.tif")
chelsa_summer<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_10.tif")
chelsa_summer_tundra<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_temp_arctic_10corrected.tif")
#chelsa_winter<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_11.tif") 
#chelsa_precip_year<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_12.tif")
chelsa_precip_summer<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_18.tif")
chelsa_precip_summer_tundra<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_precip_arctic_10corrected.tif")


#NB - May also want to create composite rasters - e.g. JJA, or !JJA / Spring

tea_coords<-cbind(tea$Lon, tea$Lat)

#tea$CHELSA_year_temp<-extract(chelsa_year,tea_coords)/10
tea$CHELSA_summer_temp<-extract(chelsa_summer,tea_coords)/10
tea$CHELSA_winter_temp<-extract(chelsa_winter,tea_coords)/10
#tea$CHELSA_year_precip<-extract(chelsa_precip_year,tea_coords)/10
tea$CHELSA_summer_precip<-extract(chelsa_precip_summer,tea_coords)/10

#3) Soil moisture data
#ESA_year<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_1.tif")

##############Relationships############################

#Make sure only using control plots
ambient<-subset(tea,Treatment=="None")

#Split into seasons to make things easier

summer<-subset(ambient,Season=="Summer")
year<-subset(ambient,Season=="Year")
winter<-subset(ambient,Season=="Winter")

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

summer_dens<-ggplot(summer, aes(x=Loss*100, y = ..density..,fill=factor(Tea_Type)))+
  geom_histogram(alpha=0.8,position = 'identity')+
  geom_density(alpha=0.25, adjust=3)+
  scale_x_continuous(limits = c(0,100))+
  scale_fill_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Desnity",x="% Mass Loss")+
  ggtitle("Summer incubations")

grid.arrange(all,ambs,summer_dens)

pdf(file="scripts/users/hthomas/Output_Images/Tea/tea_dists.pdf", width = 3, height = 3)
ggplot(tea, aes(x=Loss*100, y = ..density..*100,fill=factor(Tea_Type)))+
  geom_histogram(alpha=0.8,position = 'identity')+
  geom_density(alpha=0.25, adjust=3)+
  scale_x_continuous(limits = c(0,100))+
  scale_fill_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Proportion of teabags (%)",x="Mass Loss (%)")+
  theme(legend.position="none")
dev.off()

#Do teas correlate?####

tea_grouped_plots<-summer %>%
  group_by(Site,Plot,Tea_Type,Region) %>%
  summarise(Loss = mean(Loss))

tea_grouped_red<-filter(tea_grouped_plots,Tea_Type == "Rooibos")
names(tea_grouped_red)[5]<-"Red"
tea_grouped_green<-filter(tea_grouped_plots,Tea_Type == "Green")
names(tea_grouped_green)[5]<-"Green"

tea_grouped_green$Red<-tea_grouped_red$Red[match(tea_grouped_green$Plot,tea_grouped_red$Plot)]


ggplot(tea_grouped_green,aes(Red,Green))+
  geom_point(aes(colour = Region))+
  stat_smooth(method = "lm")+
  stat_smooth(aes(colour = Region),method = "lm", se=FALSE)+
  theme_bw()+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Green Tea",x="Rooibos Tea")+
  ggtitle("Rooibos vs green tea decomposition")

#Relationships: 1) Air Temp, 2) Soil Temp, 3) Soil Moisture, 4) CHELSA Temp####
#5) CHELSA Precip, 6) ESA Moisture####

#VARIABLES: Loss, Loss_Day, k, TBI_k, TBI_S####

#Set Variable
var.list <- c("Loss", "Loss_Day", "k", "TBI_k", "TBI_S")
#Get column number
var.num<-which(colnames(summer)==var.list[1])
  


#1) AIR TEMP#################################################

#Summer#

#Linear model use lme4
f <- paste(var.list[1], "~", "Tea_Type + airtemp_mean  + Days + (1|Region)") #Create model
lm_ML_air<-lmer(f, data=summer) #Run model
summary(lm_ML_air); r.squaredGLMM(lm_ML_air) #Summarise model

#Remove na values from summer object
summer_narm<-summer %>%
  filter(is.finite(summer[,var.num]),is.finite(airtemp_mean))


f <- as.formula(paste(var.list[1], "~ Tea_Type + airtemp_mean + Days")) #Create formula
MC_LM_air <- MCMCglmm(f, random = ~Region, data = summer_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 1000) #Run
summary(MC_LM_air) #Summarise

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_air<-min(summer$airtemp_mean,na.rm=TRUE)
max_air<-max(summer$airtemp_mean,na.rm=TRUE)

#Run for green
f <- as.formula(paste(var.list[1], "~ airtemp_mean + Days")) #Create formula
MC_ML_air_g<-MCMCglmm(f, random=~Region ,  data = summer_narm[summer_narm$Tea_Type=="Green",], nitt = 100000, burnin = 20000)
summary(MC_ML_air_g)

fixef(MC_ML_air_g, use = "mean")

?model.matrix

str(summary(MC_ML_air_g))
(summary(MC_ML_air_g))$solutions

#Alternative model preditions
mm.green <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
names(mm.green)[3]<-var.list[1]
mm <- model.matrix(terms(Loss ~ airtemp_mean + Days), mm.green)  # Create matrix of relevant effect sizes
mm.green[3] <- mm %*% fixef(MC_ML_air_g, use = "mean")  # Calculate based on the relevant effect sizes
mm.green$plo <- mm %*% summary(MC_ML_air_g)$solutions[,2]
mm.green$phi <- mm %*% summary(MC_ML_air_g)$solutions[,3]

# Calculating model predictions
airtemp_values<-seq(min_air, max_air, 0.1)
nairtemp <- length(airtemp_values)
niter <- length(MC_ML_air_g$Sol[,"(Intercept)"])
decomp_preds <- array(NA, dim = c(niter,nairtemp))

for (i in 1:niter){
    for (j in unique(airtemp_values)){
    k=which(airtemp_values == j)
    decomp_preds[i,k] <- MC_ML_air_g$Sol[i,"(Intercept)"] + MC_ML_air_g$Sol[i,"airtemp_mean"]*j + mean_burial * MC_ML_air_g$Sol[i,"Days"]
  }
}

decomp_preds_df <- array(NA, dim = c(nairtemp,3))


for (i in unique(airtemp_values)){
  k=which(airtemp_values == i)
  decomp_preds_df[k,] <- quantile(decomp_preds[,i], c(0.025, 0.5, 0.975))
}

decomp_preds_df_g <- cbind.data.frame(lower = decomp_preds_df[,1],
                                    mean = decomp_preds_df[,2],
                                    upper = decomp_preds_df[,3],
                                    airtemp = seq(min_air, max_air, 0.1))

#Run for rooibos
f <- as.formula(paste(var.list[1], "~ airtemp_mean + Days")) #Create formula
MC_ML_air_r<-MCMCglmm(f, random=~Region , data = summer_narm[summer_narm$Tea_Type=="Rooibos",], nitt = 100000, burnin = 20000)
summary(MC_ML_air_r)

fixef(MC_ML_air_r, use = "mean")

#install.packages("devtools")
remove.packages(c("rlang", "pkgbuild", "pkgload", "devtools", "usethis"))
install.packages("devtools")
install.packages("rlang")
devtools::install_github("JWiley/postMCMCglmm")

require(postMCMCglmm)

fixef(lm_ML_air_g, use = "mean")

#Alternative model preditions
mm.green <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), Days = rep(mean_burial), var = 0)  # Create a blank dataset with the years we want
names(mm.green)[3]<-var.list[1]
mm <- model.matrix(terms(lm_ML_air_g), mm.green)  # Create matrix of relevant effect sizes
mm.green[3] <- mm %*% fixef(lm_ML_air_g, use = "mean")  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_air_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green[3]-1.96*sqrt(pvar.mm.green),
                       phi = mm.green[3]+1.96*sqrt(pvar.mm.green))  # Add errors

# # Calculating model predictions
# airtemp_values<-seq(min_air, max_air, 0.1)
# nairtemp <- length(airtemp_values)
# niter <- length(MC_ML_air_r$Sol[,"(Intercept)"])
# decomp_preds <- array(NA, dim = c(niter,nairtemp))
# 
# for (i in 1:niter){
#   for (j in unique(airtemp_values)){
#     k=which(airtemp_values == j)
#     decomp_preds[i,k] <- MC_ML_air_r$Sol[i,"(Intercept)"] + MC_ML_air_r$Sol[i,"airtemp_mean"]*j + mean_burial * MC_ML_air_r$Sol[i,"Days"]
#   }
# }
# 
# decomp_preds_df <- array(NA, dim = c(nairtemp,3))
# 
# 
# for (i in unique(airtemp_values)){
#   k=which(airtemp_values == i)
#   decomp_preds_df[k,] <- quantile(decomp_preds[,i], c(0.025, 0.5, 0.975))
# }
# 
# decomp_preds_df_r <- cbind.data.frame(lower = decomp_preds_df[,1],
#                                     mean = decomp_preds_df[,2],
#                                     upper = decomp_preds_df[,3],
#                                     airtemp = seq(min_air, max_air, 0.1))

# Graph
(sum_air <- ggplot() +
    geom_ribbon(data = mm.green, mapping = aes(x = airtemp_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3)+
    geom_line(data = mm.green, mapping = aes(x = airtemp_mean,y=Loss*100), colour="darkgreen", size=1)+
    geom_point(data=summer[summer$Tea_Type=="Green",],aes(airtemp_mean,Loss*100),colour="green3",alpha=0.3,pch=16))

    stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(airtemp_mean,get(var.list[1])*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
    geom_point(data=summer[summer$Tea_Type=="Rooibos",],aes(airtemp_mean,get(var.list[1])*100),colour="indianred1",alpha=0.3,pch=16)+
    stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(airtemp_mean,get(var.list[1])*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
    geom_ribbon(data = decomp_preds_df_g, aes(x = airtemp, ymin = lower*100, ymax = upper*100),
              fill = "darkgreen", alpha = 0.3) +
 geom_ribbon(data = decomp_preds_df_r, aes(x = airtemp, ymin = lower*100, ymax = upper*100),
                fill = "red3", alpha = 0.3) +
    geom_line(data = decomp_preds_df_r, aes(x = airtemp, y = mean*100), colour="red3", size=1) +
    theme_bw()+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
    labs(y="% Mass Loss",x="Air Temeprature °C")+
    ggtitle("Measured Air Temperature"))



#b) SOIL ONLY, MASS LOSS ONLY#################################################

#Summer#
summer_narm<-summer %>%
  filter(is.finite(Loss),is.finite(soiltemp_mean))

MC_LM_soil <- MCMCglmm(Loss ~ Tea_Type + soiltemp_mean + Days, random = ~Region, data = summer_narm, family = "gaussian", pr=TRUE, nitt = 100000, burnin = 20000)
summary(MC_LM_soil)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_soil<-min(summer$soiltemp_mean,na.rm=TRUE)
max_soil<-max(summer$soiltemp_mean,na.rm=TRUE)

#Run for green
MC_ML_soil_g<-MCMCglmm(Loss ~ soiltemp_mean, random=~Region , data = summer_narm[summer_narm$Tea_Type=="Green",], nitt = 100000, burnin = 20000)
summary(MC_ML_soil_g)

# Calculating model predictions
soiltemp_values<-seq(min_soil, max_soil, 0.1)
nsoiltemp <- length(soiltemp_values)
niter <- length(MC_ML_soil_g$Sol[,"(Intercept)"])
decomp_preds <- array(NA, dim = c(niter,nsoiltemp))

for (i in 1:niter){
  for (j in unique(soiltemp_values)){
    k=which(soiltemp_values == j)
    decomp_preds[i,k] <- MC_ML_soil_g$Sol[i,"(Intercept)"] + MC_ML_soil_g$Sol[i,"soiltemp_mean"]*j
  }
}

decomp_preds_df <- array(NA, dim = c(nsoiltemp,3))


for (i in unique(soiltemp_values)){
  k=which(soiltemp_values == i)
  decomp_preds_df[k,] <- quantile(decomp_preds[,k], c(0.025, 0.5, 0.975))
}

decomp_preds_df_g <- cbind.data.frame(lower = decomp_preds_df[,1], 
                                      mean = decomp_preds_df[,2], 
                                      upper = decomp_preds_df[,3], 
                                      soiltemp = seq(min_soil, max_soil, 0.1))

#Run for rooibos
MC_ML_soil_r<-MCMCglmm(Loss ~ soiltemp_mean + Days, random=~Region , data = summer_narm[summer_narm$Tea_Type=="Rooibos",], nitt = 100000, burnin = 20000)
summary(MC_ML_soil_r)

# Calculating model predictions
soiltemp_values<-seq(min_soil, max_soil, 0.1)
nsoiltemp <- length(soiltemp_values)
niter <- length(MC_ML_soil_r$Sol[,"(Intercept)"])
decomp_preds <- array(NA, dim = c(niter,nsoiltemp))

for (i in 1:niter){
  for (j in unique(soiltemp_values)){
    k=which(soiltemp_values == j)
    decomp_preds[i,k] <- MC_ML_soil_r$Sol[i,"(Intercept)"] + MC_ML_soil_r$Sol[i,"soiltemp_mean"]*j + mean_burial * MC_ML_soil_r$Sol[i,"Days"]
  }
}

decomp_preds_df <- array(NA, dim = c(nsoiltemp,3))


for (i in unique(soiltemp_values)){
  k=which(soiltemp_values == i)
  decomp_preds_df[k,] <- quantile(decomp_preds[,k], c(0.025, 0.5, 0.975))
}

decomp_preds_df_r <- cbind.data.frame(lower = decomp_preds_df[,1], 
                                      mean = decomp_preds_df[,2], 
                                      upper = decomp_preds_df[,3], 
                                      soiltemp = seq(min_soil, max_soil, 0.1))

# Graph
pdf(file="scripts/users/hthomas/Output_Images/Tea/soil_temp_MCMC.pdf", width = 3, height = 3)
(sum_soil <- ggplot() +
    geom_point(data=summer[summer$Tea_Type=="Green",],aes(soiltemp_mean,Loss*100),colour="darkgreen",alpha=0.3,pch=16)+
    #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(soiltemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
    geom_point(data=summer[summer$Tea_Type=="Rooibos",],aes(soiltemp_mean,Loss*100),colour="red",alpha=0.3,pch=16)+
    #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(soiltemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
    geom_ribbon(data = decomp_preds_df_g, aes(x = soiltemp, ymin = lower*100, ymax = upper*100), 
                fill = "darkgreen", alpha = 0.3) +
    geom_line(data = decomp_preds_df_g, aes(x = soiltemp, y = mean*100), colour="darkgreen", size=1) +
    geom_ribbon(data = decomp_preds_df_r, aes(x = soiltemp, ymin = lower*100, ymax = upper*100), 
                fill = "red3", alpha = 0.3) +
    geom_line(data = decomp_preds_df_r, aes(x = soiltemp, y = mean*100), colour="red3", size=1) +
    theme_bw()+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=10), axis.title=element_text(size=12))+
    scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
    labs(y="Mass Loss (%)",x="Soil Temeprature (°C)")+
    ggtitle(""))
dev.off()


#c) SOIL MOISTURE ONLY, MASS LOSS ONLY###################################################

#Summer#

  #Summer#
  summer_narm<-summer %>%
    filter(is.finite(Loss),is.finite(moisture_mean))
  
  MC_LM_moisture <- MCMCglmm(Loss ~ Tea_Type + moisture_mean + Days, random = ~Region, data = summer_narm, family = "gaussian", pr=TRUE, nitt = 10000, burnin = 20000)
  summary(MC_LM_moisture)
  
  #Calculate mean burial length
  mean_burial<-mean(summer$Days)
  min_moisture<-min(summer$moisture_mean,na.rm=TRUE)
  max_moisture<-max(summer$moisture_mean,na.rm=TRUE)
  
  #Run for green
  MC_ML_moisture_g<-MCMCglmm(Loss ~ moisture_mean, random=~Region , data = summer_narm[summer_narm$Tea_Type=="Green",], nitt = 100000, burnin = 20000)
  summary(MC_ML_moisture_g)
  
  # Calculating model predictions
  moisture_values<-seq(min_moisture, max_moisture, 0.1)
  nmoisture <- length(moisture_values)
  niter <- length(MC_ML_moisture_g$Sol[,"(Intercept)"])
  decomp_preds <- array(NA, dim = c(niter,nmoisture))
  
  for (i in 1:niter){
    for (j in unique(moisture_values)){
      k=which(moisture_values == j)
      decomp_preds[i,k] <- MC_ML_moisture_g$Sol[i,"(Intercept)"] + MC_ML_moisture_g$Sol[i,"moisture_mean"]*j
    }
  }
  
  decomp_preds_df <- array(NA, dim = c(nmoisture,3))
  
  
  for (i in unique(moisture_values)){
    k=which(moisture_values == i)
    decomp_preds_df[k,] <- quantile(decomp_preds[,k], c(0.025, 0.5, 0.975))
  }
  
  decomp_preds_df_g <- cbind.data.frame(lower = decomp_preds_df[,1], 
                                        mean = decomp_preds_df[,2], 
                                        upper = decomp_preds_df[,3], 
                                        moisture = seq(min_moisture, max_moisture, 0.1))
  
  #Run for rooibos
  MC_ML_moisture_r<-MCMCglmm(Loss ~ moisture_mean + Days, random=~Region , data = summer_narm[summer_narm$Tea_Type=="Rooibos",], nitt = 10000, burnin = 2000)
  summary(MC_ML_moisture_r)
  
  # Calculating model predictions
  moisture_values<-seq(min_moisture, max_moisture, 0.1)
  nmoisture <- length(moisture_values)
  niter <- length(MC_ML_moisture_r$Sol[,"(Intercept)"])
  decomp_preds <- array(NA, dim = c(niter,nmoisture))
  
  for (i in 1:niter){
    for (j in unique(moisture_values)){
      k=which(moisture_values == j)
      decomp_preds[i,k] <- MC_ML_moisture_r$Sol[i,"(Intercept)"] + MC_ML_moisture_r$Sol[i,"moisture_mean"]*j + mean_burial * MC_ML_moisture_r$Sol[i,"Days"]
    }
  }
  
  decomp_preds_df <- array(NA, dim = c(nmoisture,3))
  
  
  for (i in unique(moisture_values)){
    k=which(moisture_values == i)
    decomp_preds_df[k,] <- quantile(decomp_preds[,k], c(0.025, 0.5, 0.975))
  }
  
  decomp_preds_df_r <- cbind.data.frame(lower = decomp_preds_df[,1], 
                                        mean = decomp_preds_df[,2], 
                                        upper = decomp_preds_df[,3], 
                                        moisture = seq(min_moisture, max_moisture, 0.1))
  
  # Graph
  pdf(file="scripts/users/hthomas/Output_Images/Tea/moisture_temp_MCMC.pdf", width = 3, height = 3)
  (sum_moisture <- ggplot() +
      geom_point(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100),colour="darkgreen",alpha=0.3,pch=16)+
      #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
      geom_point(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100),colour="red",alpha=0.3,pch=16)+
      #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
      geom_ribbon(data = decomp_preds_df_g, aes(x = moisture, ymin = lower*100, ymax = upper*100), 
                  fill = "darkgreen", alpha = 0.3) +
      geom_line(data = decomp_preds_df_g, aes(x = moisture, y = mean*100), colour="darkgreen", size=1) +
      geom_ribbon(data = decomp_preds_df_r, aes(x = moisture, ymin = lower*100, ymax = upper*100), 
                  fill = "red3", alpha = 0.3) +
      geom_line(data = decomp_preds_df_r, aes(x = moisture, y = mean*100), colour="red3", size=1) +
      theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=10), axis.title=element_text(size=12))+
      scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
      labs(y="Mass Loss (%)",x="Soil Moisture Content (%)")+
      ggtitle(""))
  dev.off()

lm_ML_moisture<-lmer(Loss ~ Tea_Type + moisture_mean  + Days + (1|Region) , data = summer)
summary(lm_ML_moisture); r.squaredGLMM(lm_ML_moisture)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_moisture<-min(summer$moisture_mean,na.rm=TRUE)
max_moisture<-max(summer$moisture_mean,na.rm=TRUE)

#Run for green
lm_ML_moisture_g<-lmer(Loss ~ moisture_mean  + Days + (1|Region) , data = summer[summer$Tea_Type=="Green",])
summary(lm_ML_moisture_g); r.squaredGLMM(lm_ML_moisture_g)

# Predictions for Green
mm.green <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_moisture_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$Loss <- mm %*% fixef(lm_ML_moisture_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_moisture_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$Loss-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$Loss+1.96*sqrt(pvar.mm.green))  # Add errors

#Run for red
lm_ML_moisture_r<-lmer(Loss ~ moisture_mean  + Days + (1|Region) , data = summer[summer$Tea_Type=="Rooibos",])
summary(lm_ML_moisture_r); r.squaredGLMM(lm_ML_moisture_r)

# Predictions for red
mm.red <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_moisture_r), mm.red)  # Create matrix of relevant effect sizes
mm.red$Loss <- mm %*% fixef(lm_ML_moisture_r)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_ML_moisture_r), mm))
mm.red <- data.frame(mm.red, plo = mm.red$Loss-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$Loss+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
sum_moisture<-ggplot()+
  geom_point(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100),colour="green3",alpha=0.3,pch=16)+
  stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
  geom_point(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100),colour="indianred1",alpha=0.3,pch=16)+
  stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = moisture_mean, y=Loss*100), colour="red3", size=1) +
  geom_ribbon(data = mm.green, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = moisture_mean, y=Loss*100), colour="darkgreen", size=1) +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="Soil Moisture")+
  ggtitle("Measured Soil Moisture")

#Winter#

#Linear model use lme4
lm_ML_moisture<-lmer(Loss ~ Tea_Type + moisture_mean  + Days + (1|Region) , data = winter)
summary(lm_ML_moisture); r.squaredGLMM(lm_ML_moisture)

#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_moisture<-min(winter$moisture_mean,na.rm=TRUE)
max_moisture<-max(winter$moisture_mean,na.rm=TRUE)

#Run for green
lm_ML_moisture_g<-lmer(Loss ~ moisture_mean  + Days + (1|Region) , data = winter[winter$Tea_Type=="Green",])
summary(lm_ML_moisture_g); r.squaredGLMM(lm_ML_moisture_g)

# Predictions for Green
mm.green <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_moisture_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$Loss <- mm %*% fixef(lm_ML_moisture_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_moisture_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$Loss-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$Loss+1.96*sqrt(pvar.mm.green))  # Add errors

#Run for red
lm_ML_moisture_r<-lmer(Loss ~ moisture_mean  + Days + (1|Region) , data = winter[winter$Tea_Type=="Rooibos",])
summary(lm_ML_moisture_r); r.squaredGLMM(lm_ML_moisture_r)

# Predictions for red
mm.red <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_moisture_r), mm.red)  # Create matrix of relevant effect sizes
mm.red$Loss <- mm %*% fixef(lm_ML_moisture_r)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_ML_moisture_r), mm))
mm.red <- data.frame(mm.red, plo = mm.red$Loss-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$Loss+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
win_moisture<-ggplot()+
  geom_point(data=winter[winter$Tea_Type=="Green",],aes(moisture_mean,Loss*100),colour="green3",alpha=0.3,pch=16)+
  stat_smooth(data=winter[winter$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
  geom_point(data=winter[winter$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100),colour="indianred1",alpha=0.3,pch=16)+
  stat_smooth(data=winter[winter$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = moisture_mean, y=Loss*100), colour="red3", size=1) +
  geom_ribbon(data = mm.green, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = moisture_mean, y=Loss*100), colour="darkgreen", size=1) +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="moisture Temeprature")+
  ggtitle("Soil Moisture (winter)")

#year#

#Linear model use lme4
lm_ML_moisture<-lmer(Loss ~ Tea_Type + moisture_mean  + Days + (1|Region) , data = year)
summary(lm_ML_moisture); r.squaredGLMM(lm_ML_moisture)

#Calculate mean burial length
mean_burial<-mean(year$Days)
min_moisture<-min(year$moisture_mean,na.rm=TRUE)
max_moisture<-max(year$moisture_mean,na.rm=TRUE)

#Run for green
lm_ML_moisture_g<-lmer(Loss ~ moisture_mean  + Days + (1|Region) , data = year[year$Tea_Type=="Green",])
summary(lm_ML_moisture_g); r.squaredGLMM(lm_ML_moisture_g)

# Predictions for Green
mm.green <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_moisture_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$Loss <- mm %*% fixef(lm_ML_moisture_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_moisture_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$Loss-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$Loss+1.96*sqrt(pvar.mm.green))  # Add errors

#Run for red
lm_ML_moisture_r<-lmer(Loss ~ moisture_mean  + Days + (1|Region) , data = year[year$Tea_Type=="Rooibos",])
summary(lm_ML_moisture_r); r.squaredGLMM(lm_ML_moisture_r)

# Predictions for red
mm.red <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_moisture_r), mm.red)  # Create matrix of relevant effect sizes
mm.red$Loss <- mm %*% fixef(lm_ML_moisture_r)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_ML_moisture_r), mm))
mm.red <- data.frame(mm.red, plo = mm.red$Loss-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$Loss+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
yr_moisture<-ggplot()+
  geom_point(data=year[year$Tea_Type=="Green",],aes(moisture_mean,Loss*100),colour="green3",alpha=0.3,pch=16)+
  stat_smooth(data=year[year$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
  geom_point(data=year[year$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100),colour="indianred1",alpha=0.3,pch=16)+
  stat_smooth(data=year[year$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = moisture_mean, y=Loss*100), colour="red3", size=1) +
  geom_ribbon(data = mm.green, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = moisture_mean, y=Loss*100), colour="darkgreen", size=1) +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="moisture Temeprature")+
  ggtitle("Soil Moisture (year)")

pdf(file="scripts/users/hthomas/Output_Images/Tea/ML_lms.pdf", width = 7.5, height = 6)
(Mass_Loss_figs<-grid.arrange(sum_air,win_air,yr_air,
                              sum_soil,win_soil,yr_soil,
                              sum_moisture, win_moisture,yr_moisture,
                              nrow=3))
dev.off()




#Overall model (Loss - summer)####

summer_narm<-summer%>%
  filter(is.finite(Loss),is.finite(soiltemp_mean),is.finite(moisture_mean), is.finite(Days))

Loss_summer<-MCMCglmm(scale(Loss) ~ Tea_Type + scale(soiltemp_mean) + scale(moisture_mean)  + scale(Days), random = ~Region , data = summer_narm, nitt = 100000, burnin = 20000)
summary(Loss_summer)

#NB - I'm not sure whether it is better to use days as a fixed or random effect

Loss_summer_fixefs<-as.data.frame(abs(summary(Loss_summer)$solution[,1]))
names(Loss_summer_fixefs)<-"Effect_Size"
rownames(Loss_summer_fixefs)<-c("Intercept","Tea Type","Soil Temperature","Soil Moisture", "Incubation Length")
Loss_summer_fixefs$Variable<-rownames(Loss_summer_fixefs)
Loss_summer_fixefs$lower<-abs(summary(Loss_summer)$solution[,2])
Loss_summer_fixefs$upper<-abs(summary(Loss_summer)$solution[,3])

pdf(file="scripts/users/hthomas/Output_Images/Tea/Effect_sizes.pdf", width = 4, height = 3)
ggplot(Loss_summer_fixefs[-1,], aes(Variable,Effect_Size))+
  geom_errorbar(data=Loss_summer_fixefs[-1,], mapping=aes(x=Variable, ymin=upper, ymax=lower), linetype=1, width=0, size=1)+
  coord_flip()+
  ylim(0,2)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y = "Standardised Effect Size")
dev.off()


#Overall model (Loss - all)####
Loss_all<-lmer(scale(Loss) ~ Tea_Type + scale(soiltemp_mean) + scale(moisture_mean)   + scale(Days) + (1|Region) + (1|Season) , data = ambient)
summary(Loss_all); r.squaredGLMM(Loss_all)

#NB - I'm not sure whether it is better to use days as a fixed or random effect

Loss_all_fixefs<-as.data.frame(fixef(Loss_all))
names(Loss_all_fixefs)<-"Effect_Size"
rownames(Loss_all_fixefs)<-c("Intercept","Tea Type","Soil Temperature","Soil Moisture", "Incubation Length")
Loss_all_fixefs$Variable<-rownames(Loss_all_fixefs)
Loss_all_fixefs$se<-summary(Loss_all)$coefficients[,2]
Loss_all_fixefs[Loss_all_fixefs$Variable=="Tea Type",]$Effect_Size<-Loss_all_fixefs[Loss_all_fixefs$Variable=="Tea Type",]$Effect_Size*-1
Loss_all_fixefs$upper<-Loss_all_fixefs$Effect_Size+Loss_all_fixefs$se
Loss_all_fixefs$lower<-Loss_all_fixefs$Effect_Size-Loss_all_fixefs$se


ggplot(Loss_all_fixefs[-1,], aes(Variable,Effect_Size))+
  geom_errorbar(data=Loss_all_fixefs[-1,], mapping=aes(x=Variable, ymin=upper, ymax=lower), linetype=1, width=0, size=1)+
  coord_flip()+
  ylim(0,2)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Effect Sizes (Summer Mass Loss)")+
  labs(y = "Standardised Effect Size")

#############With CHELSA data##########################################################

#Chelsa summer data vs measured summer data####

lm_air_chelsa_summer<-lm(CHELSA_summer_temp ~ airtemp_mean, data=summer)
summary(lm_air_chelsa_summer); r.squaredGLMM(lm_air_chelsa_summer)

air<-ggplot(summer, aes(airtemp_mean, CHELSA_summer_temp))+
  geom_jitter(width = 0.1, height = 0.1, alpha=0.5, pch=16,colour = "red")+
  theme_bw()+
  stat_smooth(method="lm", colour="red")+
  geom_abline(slope=1, intercept=0, linetype = "dashed")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="CHELSA Air Temperature °C (1979-2013)",x="Measured Air Temeprature °C")+
  ggtitle("Measured vs CHELSA - Air")

soil<-ggplot(summer, aes(soiltemp_mean, CHELSA_summer_temp))+
  geom_jitter(width = 0.1, height = 0.1, alpha=0.5, pch=16, colour="goldenrod")+
  theme_bw()+
  stat_smooth(method="lm", colour="goldenrod")+
  geom_abline(slope=1, intercept=0, linetype = "dashed")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="CHELSA Air Temperature °C (1979-2013)",x="Measured Soil Temeprature °C")+
  ggtitle("Measured vs CHELSA - Soil")

moisture<-ggplot(summer, aes(moisture_mean, CHELSA_summer_precip))+
  geom_jitter(width = 0.1, height = 0.1, alpha=0.5, pch=16, colour="blue")+
  theme_bw()+
  stat_smooth(method="lm", colour="blue")+
  #geom_abline(slope=1, intercept=0, linetype = "dashed")+
  ylim(0,50)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="CHELSA Precipitation mm (1979-2013)",x="Measured Soil Moisture %")+
  ggtitle("Measured vs CHELSA - Moisture")

grid.arrange(air,soil,moisture,ncol=3)

#Summer, Temp only####

#Linear model use lme4
lm_ML_chelsa_temp<-lmer(Loss ~ Tea_Type + CHELSA_summer_temp  + Days + (1|Region) , data = summer)
summary(lm_ML_chelsa_temp); r.squaredGLMM(lm_ML_chelsa_temp)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_air<-min(summer$CHELSA_summer_temp,na.rm=TRUE)
max_air<-max(summer$CHELSA_summer_temp,na.rm=TRUE)

#Run for green
lm_ML_chelsa_temp_g<-lmer(Loss ~ CHELSA_summer_temp  + Days + (1|Region) , data = summer[summer$Tea_Type=="Green",])
summary(lm_ML_chelsa_temp_g); r.squaredGLMM(lm_ML_chelsa_temp_g)

# Predictions for Green
mm.green <- expand.grid(CHELSA_summer_temp = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_temp_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$Loss <- mm %*% fixef(lm_ML_chelsa_temp_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_temp_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$Loss-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$Loss+1.96*sqrt(pvar.mm.green))  # Add errors

#Run for red
lm_ML_chelsa_temp_r<-lmer(Loss ~ CHELSA_summer_temp  + Days + (1|Region) , data = summer[summer$Tea_Type=="Rooibos",])
summary(lm_ML_chelsa_temp_r); r.squaredGLMM(lm_ML_chelsa_temp_r)

# Predictions for red
mm.red <- expand.grid(CHELSA_summer_temp = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_temp_r), mm.red)  # Create matrix of relevant effect sizes
mm.red$Loss <- mm %*% fixef(lm_ML_chelsa_temp_r)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_temp_r), mm))
mm.red <- data.frame(mm.red, plo = mm.red$Loss-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$Loss+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
sum_chelsa_air<-ggplot()+
  geom_point(data=summer[summer$Tea_Type=="Green",],aes(CHELSA_summer_temp,Loss*100),colour="green3",alpha=0.3,pch=16)+
  stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(CHELSA_summer_temp,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
  geom_point(data=summer[summer$Tea_Type=="Rooibos",],aes(CHELSA_summer_temp,Loss*100),colour="indianred1",alpha=0.3,pch=16)+
  stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(CHELSA_summer_temp,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_summer_temp, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = CHELSA_summer_temp, y=Loss*100), colour="red3", size=1) +
  geom_ribbon(data = mm.green, mapping = aes(x = CHELSA_summer_temp, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = CHELSA_summer_temp,y=Loss*100), colour="darkgreen", size=1) +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="Air Temeprature °C")+
  ggtitle("CHELSA Air Temperature")

#Summer, precip only

#Linear model use lme4
lm_ML_chelsa_precip<-lmer(Loss ~ Tea_Type + CHELSA_summer_precip  + Days + (1|Region) , data = summer)
summary(lm_ML_chelsa_precip); r.squaredGLMM(lm_ML_chelsa_precip)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_air<-min(summer$CHELSA_summer_precip,na.rm=TRUE)
max_air<-max(summer$CHELSA_summer_precip,na.rm=TRUE)

#Run for green
lm_ML_chelsa_precip_g<-lmer(Loss ~ CHELSA_summer_precip  + Days + (1|Region) , data = summer[summer$Tea_Type=="Green",])
summary(lm_ML_chelsa_precip_g); r.squaredGLMM(lm_ML_chelsa_precip_g)

# Predictions for Green
mm.green <- expand.grid(CHELSA_summer_precip = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_precip_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$Loss <- mm %*% fixef(lm_ML_chelsa_precip_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_precip_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$Loss-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$Loss+1.96*sqrt(pvar.mm.green))  # Add errors

#Run for red
lm_ML_chelsa_precip_r<-lmer(Loss ~ CHELSA_summer_precip  + Days + (1|Region) , data = summer[summer$Tea_Type=="Rooibos",])
summary(lm_ML_chelsa_precip_r); r.squaredGLMM(lm_ML_chelsa_precip_r)

# Predictions for red
mm.red <- expand.grid(CHELSA_summer_precip = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_precip_r), mm.red)  # Create matrix of relevant effect sizes
mm.red$Loss <- mm %*% fixef(lm_ML_chelsa_precip_r)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_precip_r), mm))
mm.red <- data.frame(mm.red, plo = mm.red$Loss-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$Loss+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
sum_chelsa_precip<-ggplot()+
  geom_point(data=summer[summer$Tea_Type=="Green",],aes(CHELSA_summer_precip,Loss*100),colour="green3",alpha=0.3,pch=16)+
  stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(CHELSA_summer_precip,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
  geom_point(data=summer[summer$Tea_Type=="Rooibos",],aes(CHELSA_summer_precip,Loss*100),colour="indianred1",alpha=0.3,pch=16)+
  stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(CHELSA_summer_precip,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_summer_precip, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = CHELSA_summer_precip, y=Loss*100), colour="red3", size=1) +
  geom_ribbon(data = mm.green, mapping = aes(x = CHELSA_summer_precip, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = CHELSA_summer_precip,y=Loss*100), colour="darkgreen", size=1) +
  theme_bw()+
  stat_smooth(data=summer, aes(CHELSA_summer_precip,Loss*100, colour = Tea_Type), method="lm", alpha=0.2, size=0.5, linetype = "dashed")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme(legend.position="none")+
  labs(y="% Mass Loss",x="Precipitation mm")+
  ggtitle("CHELSA precipitation")

#Create blank
library(grid)
blank <- grid.rect(gp=gpar(col="white"))

#Combine figures######
pdf(file="scripts/users/hthomas/Output_Images/Tea/Env_vars.pdf", width = 12.5, height = 10)
grid.arrange(sum_air,sum_soil,sum_moisture,
             sum_chelsa_air,blank,sum_chelsa_precip,
             air,soil,moisture)
dev.off()

#winter, Temp only

#Linear model use lme4
lm_ML_chelsa_temp<-lmer(Loss ~ Tea_Type + CHELSA_winter_temp  + Days + (1|Region) , data = winter)
summary(lm_ML_chelsa_temp); r.squaredGLMM(lm_ML_chelsa_temp)

#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_air<-min(winter$CHELSA_winter_temp,na.rm=TRUE)
max_air<-max(winter$CHELSA_winter_temp,na.rm=TRUE)

#Run for green
lm_ML_chelsa_temp_g<-lmer(Loss ~ CHELSA_winter_temp  + Days + (1|Region) , data = winter[winter$Tea_Type=="Green",])
summary(lm_ML_chelsa_temp_g); r.squaredGLMM(lm_ML_chelsa_temp_g)

# Predictions for Green
mm.green <- expand.grid(CHELSA_winter_temp = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_temp_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$Loss <- mm %*% fixef(lm_ML_chelsa_temp_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_temp_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$Loss-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$Loss+1.96*sqrt(pvar.mm.green))  # Add errors

#Run for red
lm_ML_chelsa_temp_r<-lmer(Loss ~ CHELSA_winter_temp  + Days + (1|Region) , data = winter[winter$Tea_Type=="Rooibos",])
summary(lm_ML_chelsa_temp_r); r.squaredGLMM(lm_ML_chelsa_temp_r)

# Predictions for red
mm.red <- expand.grid(CHELSA_winter_temp = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_temp_r), mm.red)  # Create matrix of relevant effect sizes
mm.red$Loss <- mm %*% fixef(lm_ML_chelsa_temp_r)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_temp_r), mm))
mm.red <- data.frame(mm.red, plo = mm.red$Loss-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$Loss+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
win_chelsa_air<-ggplot()+
  geom_point(data=winter[winter$Tea_Type=="Green",],aes(CHELSA_winter_temp,Loss*100),colour="green3",alpha=0.3,pch=16)+
  stat_smooth(data=winter[winter$Tea_Type=="Green",],aes(CHELSA_winter_temp,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
  geom_point(data=winter[winter$Tea_Type=="Rooibos",],aes(CHELSA_winter_temp,Loss*100),colour="indianred1",alpha=0.3,pch=16)+
  stat_smooth(data=winter[winter$Tea_Type=="Rooibos",],aes(CHELSA_winter_temp,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_winter_temp, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = CHELSA_winter_temp, y=Loss*100), colour="red3", size=1) +
  geom_ribbon(data = mm.green, mapping = aes(x = CHELSA_winter_temp, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = CHELSA_winter_temp,y=Loss*100), colour="darkgreen", size=1) +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="Air Temeprature")+
  ggtitle("Air Temperature (winter)")

#year, Temp only

#Linear model use lme4
lm_ML_chelsa_temp<-lmer(Loss ~ Tea_Type + CHELSA_year_temp  + Days + (1|Region) , data = year)
summary(lm_ML_chelsa_temp); r.squaredGLMM(lm_ML_chelsa_temp)

#Calculate mean burial length
mean_burial<-mean(year$Days)
min_air<-min(year$CHELSA_year_temp,na.rm=TRUE)
max_air<-max(year$CHELSA_year_temp,na.rm=TRUE)

#Run for green
lm_ML_chelsa_temp_g<-lmer(Loss ~ CHELSA_year_temp  + Days + (1|Region) , data = year[year$Tea_Type=="Green",])
summary(lm_ML_chelsa_temp_g); r.squaredGLMM(lm_ML_chelsa_temp_g)

# Predictions for Green
mm.green <- expand.grid(CHELSA_year_temp = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_temp_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$Loss <- mm %*% fixef(lm_ML_chelsa_temp_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_temp_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$Loss-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$Loss+1.96*sqrt(pvar.mm.green))  # Add errors

#Run for red
lm_ML_chelsa_temp_r<-lmer(Loss ~ CHELSA_year_temp  + Days + (1|Region) , data = year[year$Tea_Type=="Rooibos",])
summary(lm_ML_chelsa_temp_r); r.squaredGLMM(lm_ML_chelsa_temp_r)

# Predictions for red
mm.red <- expand.grid(CHELSA_year_temp = seq(min_air, max_air, 0.1), Days = rep(mean_burial), Loss = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_ML_chelsa_temp_r), mm.red)  # Create matrix of relevant effect sizes
mm.red$Loss <- mm %*% fixef(lm_ML_chelsa_temp_r)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_ML_chelsa_temp_r), mm))
mm.red <- data.frame(mm.red, plo = mm.red$Loss-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$Loss+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
yr_chelsa_air<-ggplot()+
  geom_point(data=year[year$Tea_Type=="Green",],aes(CHELSA_year_temp,Loss*100),colour="green3",alpha=0.3,pch=16)+
  stat_smooth(data=year[year$Tea_Type=="Green",],aes(CHELSA_year_temp,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
  geom_point(data=year[year$Tea_Type=="Rooibos",],aes(CHELSA_year_temp,Loss*100),colour="indianred1",alpha=0.3,pch=16)+
  stat_smooth(data=year[year$Tea_Type=="Rooibos",],aes(CHELSA_year_temp,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = CHELSA_year_temp, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = CHELSA_year_temp, y=Loss*100), colour="red3", size=1) +
  geom_ribbon(data = mm.green, mapping = aes(x = CHELSA_year_temp, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = CHELSA_year_temp,y=Loss*100), colour="darkgreen", size=1) +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass Loss",x="Air Temeprature")+
  ggtitle("Air Temperature (year)")


#############Create map##########################################################

#load extra packages
library(rasterVis)
library(viridisLite) # for generating your own viridis colour scale
library(sp)
library(maps)
library(maptools)
library(mapproj)


####STEP 1####

#Model just for summer temp (green tea)
mean_burial<-mean(summer$Days) #Get mean incubation length

# #TEMP####
# #Step 3 - Make map
# predicted_summer_temp <- (summary(lm_ML_chelsa_temp_g)$coefficients[1,1] + summary(lm_ML_chelsa_temp_g)$coefficients[2,1] * chelsa_summer_tundra + (summary(lm_ML_chelsa_temp_g)$coefficients[3,1] * mean_burial)) 
# # Plot the map
# temp_map<-levelplot(predicted_summer_temp, margin = F, main = "Temperature")
# 
# #PRECIP####
# #Step 3 - Make map
# predicted_summer_precip <- (summary(lm_ML_chelsa_precip_g)$coefficients[1,1] + summary(lm_ML_chelsa_precip_g)$coefficients[2,1] * chelsa_precip_summer_tundra + (summary(lm_ML_chelsa_temp_g)$coefficients[3,1] * mean_burial)) 
# # Plot the map
# precip_map<-levelplot(predicted_summer_precip, margin = F, main = "Temperature")

#Combined####
#lm_ML_chelsa_both_g<-lmer((Loss*100) ~ CHELSA_summer_temp + CHELSA_summer_precip + Days + (1|Region), data = summer[summer$Tea_Type=="Green",])
#summary(lm_ML_chelsa_both_g); r.squaredGLMM(lm_ML_chelsa_both_g)

#decomp_map <- (summary(lm_ML_chelsa_both_g)$coefficients[1,1] + summary(lm_ML_chelsa_both_g)$coefficients[2,1] * chelsa_summer_tundra + summary(lm_ML_chelsa_both_g)$coefficients[3,1] * chelsa_precip_summer_tundra + summary(lm_ML_chelsa_both_g)$coefficients[4,1] * mean_burial) 

# Plot the map
levelplot(decomp_map, margin = F, main = "Decomposition Map")

summary(decomp_map)

#writeRaster(decomp_map, "/Volumes/Teamshrub/Haydn/decom_map.tif")
decomp_map<-raster("/Volumes/Teamshrub/Haydn/decom_map.tif")

# Make pretty themes
no_borders_theme <- rasterTheme(region = inferno(100, begin = 0, end = 1), # virdis scale
                                axis.line = list(col = 0),  # no axes, scales etc
                                par.main.text = list(font = 1, cex = 1)) # normal face title

no_borders_theme_white <- rasterTheme(region = "white", # virdis scale
                                      axis.line = list(col = 0),  # no axes, scales etc
                                      par.main.text = list(font = 1, cex = 1)) # normal face title

#Add tea sites###
mround <- function(x,base){ 
  base*round(x/base) 
} 

tea$Lat<-as.numeric(as.character(tea$Lat))
tea$Lon<-as.numeric(as.character(tea$Lon))

tea$Lat_round<-mround(tea$Lat,5)
tea$Lon_round<-mround(tea$Lon,5)
tea$latlon_round<-paste(tea$Lat_round,"_",tea$Lon_round,sep="")

coords.tea<-tea %>% group_by(latlon_round,Lat_round,Lon_round) %>% summarise(size=(length((latlon_round))))

coords.tea<-subset(coords.tea,!is.na(Lat_round))

#pdf(file="scripts/users/hthomas/Output_Images/Tea/map1.pdf", width = 7.5, height = 6)
(tea.map<-levelplot(decomp_map, margin = F, xlab=NULL, ylab=NULL, scales=list(draw=FALSE), par.settings = no_borders_theme)+
  layer(sp.polygons(vegmap3, fill='white', col = FALSE)))


#dev.off()

# Re-project to laea
# Takes a while to run
#newproj <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"
#decomp_map_new <- projectRaster(decomp_map, crs=newproj)

# Creates mask for everything below treeline
# Takes ages to run
 masked_raster <- mask(decomp_map, vegmap3, inverse = TRUE)
 masked_raster2 <- mask(masked_raster, vegmap, inverse = TRUE)
 
 pdf(file="scripts/users/hthomas/Output_Images/Tea/Map1.pdf",width=30,height=15)
 plot(masked_raster2, main = "", col = colfunc(30), frame.plot=FALSE, axes=FALSE, box=FALSE)
 plot(coord1, add = TRUE, pch=16, col=alpha("darkorchid4",0.5), bg="black", cex=sqrt((coords.tea$size/3)))
 plot(coord1, add = TRUE, pch=1, col=alpha("black",0.8), bg="black", cex=sqrt((coords.tea$size/3)))
 dev.off()


# Make world map in laea
wrld <- map(plot=FALSE, interior=FALSE, wrap=TRUE, ylim=c(50, 90), xlim=c(-180, 180))
wrld_sp <- map2SpatialLines(wrld)
proj4string(wrld_sp) <- CRS("+proj=longlat")
laea_wrld_sp <- spTransform(wrld_sp, CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m"))
plot(laea_wrld_sp, col="black")


# Load  data
vegmap <- readOGR("/Volumes/Teamshrub/Climate_Data/CAVM_map/cp_biozone_la_shp", "cavm_all polygon")
ogrInfo("/Volumes/Teamshrub/Climate_Data/CAVM_map/cp_biozone_la_shp", "cavm_all polygon")
vegmap2 <- spTransform(vegmap, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
vegmap3 <- vegmap2[which(vegmap2$ZONE < 1),]

# Plots orignal projection
levelplot(decomp_map, margin = F, xlab=NULL, ylab=NULL, scales=list(draw=FALSE), par.settings = no_borders_theme) +
  # Masks out everything below treeline
  levelplot(decomp_map, margin = F, par.settings = no_borders_theme_white) +
  # Masks out ice
  mask(sp.polygons(vegmap3, fill='white', col = FALSE))+
  # Adds land-ocean boarder
  layer(sp.polygons(laea_wrld_sp, col="black"))

#Step 4 - Save the raster
# writeRaster(predicted_raster, "/Volumes/csce/geos/groups/Teamshrub/Climate_Data/Chelsa/workshop/supercoolmap.tif")

writeRaster(masked_raster, "/Volumes/csce/geos/groups/Teamshrub/Climate_Data/Chelsa/workshop/supercoolmap_mask.tif")















#a) AIR ONLY, TBI S ONLY#################################################

#Summer#

#Linear model use lme4
lm_TS_air<-lmer(TBI_S ~ Tea_Type + airtemp_mean  + Days + (1|Region) , data = summer)
summary(lm_TS_air); r.squaredGLMM(lm_TS_air)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_air<-min(summer$airtemp_mean,na.rm=TRUE)
max_air<-max(summer$airtemp_mean,na.rm=TRUE)

#Run for green
lm_TS_air_g<-lmer(TBI_S ~ airtemp_mean  + Days + (1|Region) , data = summer[summer$Tea_Type=="Green",])
summary(lm_TS_air_g); r.squaredGLMM(lm_TS_air_g)

# Predictions for Green
mm.green <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_air_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_air_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_air_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors

#Plot
sum_air<-ggplot(summer[summer$Tea_Type=="Green",],aes(airtemp_mean,TBI_S))+
  geom_ribbon(data = mm.green, mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = airtemp_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="TBI_S",x="Air Temeprature")+
  ggtitle("Air Temperature (summer)")

#Winter#

#Linear model use lme4
lm_TS_air<-lmer(TBI_S ~ Tea_Type + airtemp_mean  + Days + (1|Region) , data = winter)
summary(lm_TS_air); r.squaredGLMM(lm_TS_air)

#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_air<-min(winter$airtemp_mean,na.rm=TRUE)
max_air<-max(winter$airtemp_mean,na.rm=TRUE)

#Run for green
lm_TS_air_g<-lmer(TBI_S ~ airtemp_mean  +  (1|Days) , data = winter[winter$Tea_Type=="Green",])
summary(lm_TS_air_g); r.squaredGLMM(lm_TS_air_g)

# Predictions for Green
mm.green <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_air_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_air_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_air_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors


#Plot
win_air<-ggplot(winter[winter$Tea_Type=="Green",],aes(airtemp_mean,TBI_S*100))+
  geom_ribbon(data = mm.green, mapping = aes(x = airtemp_mean, ymin = plo*100, ymax = phi*100), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = airtemp_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="TBI_S",x="Air Temeprature")+
  ggtitle("Air Temperature (winter)")

#year#

#Linear model use lme4
lm_TS_air<-lmer(TBI_S ~ Tea_Type + airtemp_mean  + Days + (1|Region) , data = year)
summary(lm_TS_air); r.squaredGLMM(lm_TS_air)

#Calculate mean burial length
mean_burial<-mean(year$Days)
min_air<-min(year$airtemp_mean,na.rm=TRUE)
max_air<-max(year$airtemp_mean,na.rm=TRUE)

#Run for green
lm_TS_air_g<-lmer(TBI_S ~ airtemp_mean  + Days + (1|Region) , data = year[year$Tea_Type=="Green",])
summary(lm_TS_air_g); r.squaredGLMM(lm_TS_air_g)

# Predictions for Green
mm.green <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_air_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_air_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_air_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors

#Plot
yr_air<-ggplot(year[year$Tea_Type=="Green",],aes(airtemp_mean,TBI_S))+
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  geom_ribbon(data = mm.green, mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = airtemp_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="TBI_S",x="Air Temeprature")+
  ggtitle("Air Temperature (year)")

#b) SOIL ONLY, MASS LOSS ONLY#################################################

#Summer#

#Linear model use lme4
lm_TS_soil<-lmer(TBI_S ~ Tea_Type + soiltemp_mean  + Days + (1|Region) , data = summer)
summary(lm_TS_soil); r.squaredGLMM(lm_TS_soil)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_soil<-min(summer$soiltemp_mean,na.rm=TRUE)
max_soil<-max(summer$soiltemp_mean,na.rm=TRUE)

#Run for green
lm_TS_soil_g<-lmer(TBI_S ~ soiltemp_mean  + Days + (1|Region) , data = summer[summer$Tea_Type=="Green",])
summary(lm_TS_soil_g); r.squaredGLMM(lm_TS_soil_g)

# Predictions for Green
mm.green <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_soil_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_soil_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_soil_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors


#Plot
sum_soil<-ggplot(summer[summer$Tea_Type=="Green",],aes(soiltemp_mean,TBI_S))+
  geom_ribbon(data = mm.green, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = soiltemp_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass TBI_S",x="soil Temeprature")+
  ggtitle("Soil Temperature (summer)")

#Winter#

#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_soil<-min(winter$soiltemp_mean,na.rm=TRUE)
max_soil<-max(winter$soiltemp_mean,na.rm=TRUE)

#Run for green
lm_TS_soil_g<-lmer(TBI_S ~ soiltemp_mean  + (1|Days) , data = winter[winter$Tea_Type=="Green",])
summary(lm_TS_soil_g); r.squaredGLMM(lm_TS_soil_g)

# Predictions for Green
mm.green <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_soil_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_soil_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_soil_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors

#Plot
win_soil<-ggplot(winter[winter$Tea_Type=="Green",],aes(soiltemp_mean,TBI_S))+
  geom_ribbon(data = mm.green, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = soiltemp_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="TBI_S",x="soil Temeprature")+
  ggtitle("Soil Temperature (winter)")

#year#

#Linear model use lme4
lm_TS_soil<-lmer(TBI_S ~ Tea_Type + soiltemp_mean  + Days + (1|Region) , data = year)
summary(lm_TS_soil); r.squaredGLMM(lm_TS_soil)

#Calculate mean burial length
mean_burial<-mean(year$Days)
min_soil<-min(year$soiltemp_mean,na.rm=TRUE)
max_soil<-max(year$soiltemp_mean,na.rm=TRUE)

#Run for green
lm_TS_soil_g<-lmer(TBI_S ~ soiltemp_mean  + Days + (1|Region) , data = year[year$Tea_Type=="Green",])
summary(lm_TS_soil_g); r.squaredGLMM(lm_TS_soil_g)

# Predictions for Green
mm.green <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_soil_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_soil_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_soil_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors

#Plot
yr_soil<-ggplot(year[year$Tea_Type=="Green",],aes(soiltemp_mean,TBI_S))+
  geom_ribbon(data = mm.green, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = soiltemp_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="TBI_S",x="soil Temeprature")+
  ggtitle("Soil Temperature (year)")


#c) SOIL MOISTURE ONLY, MASS LOSS ONLY###################################################

#Summer#

#Linear model use lme4
lm_TS_moisture<-lmer(TBI_S ~ Tea_Type + moisture_mean  + Days + (1|Region) , data = summer)
summary(lm_TS_moisture); r.squaredGLMM(lm_TS_moisture)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_moisture<-min(summer$moisture_mean,na.rm=TRUE)
max_moisture<-max(summer$moisture_mean,na.rm=TRUE)

#Run for green
lm_TS_moisture_g<-lmer(TBI_S ~ moisture_mean  + Days + (1|Region) , data = summer[summer$Tea_Type=="Green",])
summary(lm_TS_moisture_g); r.squaredGLMM(lm_TS_moisture_g)

# Predictions for Green
mm.green <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_moisture_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_moisture_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_moisture_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors


#Plot
sum_moisture<-ggplot(summer[summer$Tea_Type=="Green",],aes(moisture_mean,TBI_S))+
  geom_ribbon(data = mm.green, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = moisture_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="TBI_S",x="moisture Temeprature")+
  ggtitle("Soil Moisture (summer)")

#Winter#

#Linear model use lme4
#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_moisture<-min(winter$moisture_mean,na.rm=TRUE)
max_moisture<-max(winter$moisture_mean,na.rm=TRUE)

#Run for green
lm_TS_moisture_g<-lm(TBI_S ~ moisture_mean   , data = winter[winter$Tea_Type=="Green",])
summary(lm_TS_moisture_g); r.squaredGLMM(lm_TS_moisture_g)

#Plot
win_moisture<-ggplot(winter[winter$Tea_Type=="Green",],aes(moisture_mean,TBI_S))+
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  stat_smooth(method = "lm",colour = "darkgreen", fill = "darkgreen")+
  labs(y="TBI_S",x="Soil Moisture ")+
  ggtitle("Soil Moisture (winter)")

#year#

#Linear model use lme4
#Calculate mean burial length
mean_burial<-mean(year$Days)
min_moisture<-min(year$moisture_mean,na.rm=TRUE)
max_moisture<-max(year$moisture_mean,na.rm=TRUE)

#Run for green
lm_TS_moisture_g<-lmer(TBI_S ~ moisture_mean  +  (1|Days) , data = year[year$Tea_Type=="Green",])
summary(lm_TS_moisture_g); r.squaredGLMM(lm_TS_moisture_g)

# Predictions for Green
mm.green <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), TBI_S = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_TS_moisture_g), mm.green)  # Create matrix of relevant effect sizes
mm.green$TBI_S <- mm %*% fixef(lm_TS_moisture_g)  # Calculate height based on the relevant effect sizes
pvar.mm.green <- diag(mm %*% tcrossprod(vcov(lm_TS_moisture_g), mm))
mm.green <- data.frame(mm.green, plo = mm.green$TBI_S-1.96*sqrt(pvar.mm.green),
                       phi = mm.green$TBI_S+1.96*sqrt(pvar.mm.green))  # Add errors

#Plot
yr_moisture<-ggplot(year[year$Tea_Type=="Green",],aes(moisture_mean,TBI_S))+
  geom_ribbon(data = mm.green, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), fill = "darkgreen", alpha = 0.3) +
  geom_line(data = mm.green, mapping = aes(x = moisture_mean), colour="darkgreen", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  labs(y="% Mass TBI_S",x="moisture Temeprature")+
  ggtitle("Soil Moisture (year)")

pdf(file="scripts/users/hthomas/Output_Images/Tea/TBI_S_lms.pdf", width = 7.5, height = 6)
(TBI_S_figs<-grid.arrange(sum_air,win_air,yr_air,
                          sum_soil,win_soil,yr_soil,
                          sum_moisture, win_moisture,yr_moisture,
                          nrow=3))
dev.off()




#a) AIR ONLY, TBI S ONLY#################################################

#Summer#

#Linear model use lme4
lm_Tk_air<-lmer(TBI_k ~ Tea_Type + airtemp_mean  + Days + (1|Region) , data = summer)
summary(lm_Tk_air); r.squaredGLMM(lm_Tk_air)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_air<-min(summer$airtemp_mean,na.rm=TRUE)
max_air<-max(summer$airtemp_mean,na.rm=TRUE)

#Run for red
lm_Tk_air_g<-lmer(TBI_k ~ airtemp_mean  + (1|Region) + (1|Days), data = summer[summer$Tea_Type=="Rooibos",])
summary(lm_Tk_air_g); r.squaredGLMM(lm_Tk_air_g)

# Predictions for Green
mm.red <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_air_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_air_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_air_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
sum_air<-ggplot(summer[summer$Tea_Type=="Rooibos",],aes(airtemp_mean,TBI_k))+
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  geom_ribbon(data = mm.red, mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = airtemp_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red"),name="Tea Type")+
  labs(y="TBI_k",x="Air Temeprature")+
  ggtitle("Air Temperature (summer)")

#Winter#

#Linear model use lme4

#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_air<-min(winter$airtemp_mean,na.rm=TRUE)
max_air<-max(winter$airtemp_mean,na.rm=TRUE)

#Run for red
lm_Tk_air_g<-lmer(TBI_k ~ airtemp_mean  +  (1|Days) , data = winter[winter$Tea_Type=="Rooibos",])
summary(lm_Tk_air_g); r.squaredGLMM(lm_Tk_air_g)

# Predictions for Red
mm.red <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_air_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_air_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_air_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors


#Plot
win_air<-ggplot(winter[winter$Tea_Type=="Rooibos",],aes(airtemp_mean,TBI_k*100))+
  geom_ribbon(data = mm.red, mapping = aes(x = airtemp_mean, ymin = plo*100, ymax = phi*100), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = airtemp_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red"),name="Tea Type")+
  labs(y="TBI_k",x="Air Temeprature")+
  ggtitle("Air Temperature (winter)")

#year#

#Linear model use lme4
lm_Tk_air<-lmer(TBI_k ~ Tea_Type + airtemp_mean  + Days + (1|Region) , data = year)
summary(lm_Tk_air); r.squaredGLMM(lm_Tk_air)

#Calculate mean burial length
mean_burial<-mean(year$Days)
min_air<-min(year$airtemp_mean,na.rm=TRUE)
max_air<-max(year$airtemp_mean,na.rm=TRUE)

#Run for red
lm_Tk_air_g<-lmer(TBI_k ~ airtemp_mean  + (1|Days) + (1|Region) , data = year[year$Tea_Type=="Rooibos",])
summary(lm_Tk_air_g); r.squaredGLMM(lm_Tk_air_g)

# Predictions for Green
mm.red <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_air_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_air_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_air_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
yr_air<-ggplot(year[year$Tea_Type=="Rooibos",],aes(airtemp_mean,TBI_k))+
  geom_ribbon(data = mm.red, mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = airtemp_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red", "red"),name="Tea Type")+
  labs(y="TBI_k",x="Air Temeprature")+
  ggtitle("Air Temperature (year)")

#b) SOIL ONLY, MASS LOSS ONLY#################################################

#Summer#

#Linear model use lme4
lm_Tk_soil<-lmer(TBI_k ~ Tea_Type + soiltemp_mean  + Days + (1|Region) , data = summer)
summary(lm_Tk_soil); r.squaredGLMM(lm_Tk_soil)

#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_soil<-min(summer$soiltemp_mean,na.rm=TRUE)
max_soil<-max(summer$soiltemp_mean,na.rm=TRUE)

#Run for red
lm_Tk_soil_g<-lmer(TBI_k ~ soiltemp_mean  + (1|Days) + (1|Region) , data = summer[summer$Tea_Type=="Rooibos",])
summary(lm_Tk_soil_g); r.squaredGLMM(lm_Tk_soil_g)

# Predictions for Green
mm.red <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1),  TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_soil_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_soil_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_soil_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors


#Plot
sum_soil<-ggplot(summer[summer$Tea_Type=="Rooibos",],aes(soiltemp_mean,TBI_k))+
  geom_ribbon(data = mm.red, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = soiltemp_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red"),name="Tea Type")+
  labs(y="% Mass TBI_k",x="soil Temeprature")+
  ggtitle("Soil Temperature (summer)")

#Winter#

#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_soil<-min(winter$soiltemp_mean,na.rm=TRUE)
max_soil<-max(winter$soiltemp_mean,na.rm=TRUE)

#Run for red
lm_Tk_soil_g<-lmer(TBI_k ~ soiltemp_mean  + (1|Days) , data = winter[winter$Tea_Type=="Rooibos",])
summary(lm_Tk_soil_g); r.squaredGLMM(lm_Tk_soil_g)

# Predictions for Green
mm.red <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), Days = rep(mean_burial), TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_soil_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_soil_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_soil_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
win_soil<-ggplot(winter[winter$Tea_Type=="Rooibos",],aes(soiltemp_mean,TBI_k))+
  geom_ribbon(data = mm.red, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = soiltemp_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red"),name="Tea Type")+
  labs(y="TBI_k",x="soil Temeprature")+
  ggtitle("Soil Temperature (winter)")

#year#

#Linear model use lme4
#Calculate mean burial length
mean_burial<-mean(year$Days)
min_soil<-min(year$soiltemp_mean,na.rm=TRUE)
max_soil<-max(year$soiltemp_mean,na.rm=TRUE)

#Run for red
lm_Tk_soil_g<-lmer(TBI_k ~ soiltemp_mean  + (1|Days) + (1|Region) , data = year[year$Tea_Type=="Rooibos",])
summary(lm_Tk_soil_g); r.squaredGLMM(lm_Tk_soil_g)

# Predictions for Green
mm.red <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_soil_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_soil_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_soil_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
yr_soil<-ggplot(year[year$Tea_Type=="Rooibos",],aes(soiltemp_mean,TBI_k))+
  geom_ribbon(data = mm.red, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = soiltemp_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red", "red"),name="Tea Type")+
  labs(y="TBI_k",x="soil Temeprature")+
  ggtitle("Soil Temperature (year)")


#c) SOIL MOISTURE ONLY, MASS LOSS ONLY###################################################

#Summer#

#Linear model use lme4
#Calculate mean burial length
mean_burial<-mean(summer$Days)
min_moisture<-min(summer$moisture_mean,na.rm=TRUE)
max_moisture<-max(summer$moisture_mean,na.rm=TRUE)

#Run for red
lm_Tk_moisture_g<-lmer(TBI_k ~ moisture_mean  + (1|Days) + (1|Region) , data = summer[summer$Tea_Type=="Rooibos",])
summary(lm_Tk_moisture_g); r.squaredGLMM(lm_Tk_moisture_g)

# Predictions for Green
mm.red <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_moisture_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_moisture_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_moisture_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors


#Plot
sum_moisture<-ggplot(summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,TBI_k))+
  geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = moisture_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red", "red"),name="Tea Type")+
  labs(y="TBI_k",x="moisture Temeprature")+
  ggtitle("Soil Moisture (summer)")

#Winter#

#Linear model use lme4
#Calculate mean burial length
mean_burial<-mean(winter$Days)
min_moisture<-min(winter$moisture_mean,na.rm=TRUE)
max_moisture<-max(winter$moisture_mean,na.rm=TRUE)

#Run for red
lm_Tk_moisture_g<-lm(TBI_k ~ moisture_mean   , data = winter[winter$Tea_Type=="Rooibos",])
summary(lm_Tk_moisture_g); r.squaredGLMM(lm_Tk_moisture_g)

#Plot
win_moisture<-ggplot(winter[winter$Tea_Type=="Rooibos",],aes(moisture_mean,TBI_k))+
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red"),name="Tea Type")+
  stat_smooth(method = "lm",colour = "red", fill = "red")+
  labs(y="TBI_k",x="Soil Moisture ")+
  ggtitle("Soil Moisture (winter)")

#year#

#Linear model use lme4
#Calculate mean burial length
mean_burial<-mean(year$Days)
min_moisture<-min(year$moisture_mean,na.rm=TRUE)
max_moisture<-max(year$moisture_mean,na.rm=TRUE)

#Run for red
lm_Tk_moisture_g<-lmer(TBI_k ~ moisture_mean  +  (1|Days) , data = year[year$Tea_Type=="Rooibos",])
summary(lm_Tk_moisture_g); r.squaredGLMM(lm_Tk_moisture_g)

# Predictions for Green
mm.red <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), Days = rep(mean_burial), TBI_k = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_Tk_moisture_g), mm.red)  # Create matrix of relevant effect sizes
mm.red$TBI_k <- mm %*% fixef(lm_Tk_moisture_g)  # Calculate height based on the relevant effect sizes
pvar.mm.red <- diag(mm %*% tcrossprod(vcov(lm_Tk_moisture_g), mm))
mm.red <- data.frame(mm.red, plo = mm.red$TBI_k-1.96*sqrt(pvar.mm.red),
                     phi = mm.red$TBI_k+1.96*sqrt(pvar.mm.red))  # Add errors

#Plot
yr_moisture<-ggplot(year[year$Tea_Type=="Rooibos",],aes(moisture_mean,TBI_k))+
  geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.red, mapping = aes(x = moisture_mean), colour="red", size=1) +
  geom_point(aes(colour=Tea_Type), alpha=0.8)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("red", "red"),name="Tea Type")+
  labs(y="% Mass TBI_k",x="moisture Temeprature")+
  ggtitle("Soil Moisture (year)")

pdf(file="scripts/users/hthomas/Output_Images/Tea/TBI_k_lms.pdf", width = 7.5, height = 6)
(TBI_k_figs<-grid.arrange(sum_air,win_air,yr_air,
                          sum_soil,win_soil,yr_soil,
                          sum_moisture, win_moisture,yr_moisture,
                          nrow=3))
dev.off()



#Air
mean_burial<-mean(summer$Days)
min_air<-min(summer$airtemp_mean,na.rm=TRUE)
max_air<-max(summer$airtemp_mean,na.rm=TRUE)

lm_air_chelsa_summer<-lmer(CHELSA_summer_temp ~ airtemp_mean + (1|Region), data=summer)
summary(lm_air_chelsa_summer); r.squaredGLMM(lm_air_chelsa_summer)

# Predictions for Green
mm.air <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), CHELSA_summer_temp = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_air_chelsa_summer), mm.air)  # Create matrix of relevant effect sizes
mm.air$CHELSA_summer_temp <- mm %*% fixef(lm_air_chelsa_summer)  # Calculate  based on the relevant effect sizes
pvar.mm.air <- diag(mm %*% tcrossprod(vcov(lm_air_chelsa_summer), mm))
mm.air <- data.frame(mm.air, plo = mm.air$CHELSA_summer_temp-1.96*sqrt(pvar.mm.air),
                     phi = mm.air$CHELSA_summer_temp+1.96*sqrt(pvar.mm.air))  # Add errors

air<-ggplot(summer, aes(airtemp_mean, CHELSA_summer_temp))+
  geom_ribbon(data = mm.air, mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi), fill = "red", alpha = 0.3) +
  geom_line(data = mm.air, mapping = aes(x = airtemp_mean, y=CHELSA_summer_temp), colour="red", size=1) +
  geom_jitter(width = 0.1, height = 0.1, alpha=0.5, pch=16,colour = "red")+
  theme_bw()+
  geom_abline(slope=1, intercept=0, linetype = "dashed")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="CHELSA Air Temperature °C (1979-2013)",x="Measured Air Temeprature °C")+
  ggtitle("Air Temperature (summer)")

#Soil
mean_burial<-mean(summer$Days)
min_soil<-min(summer$soiltemp_mean,na.rm=TRUE)
max_soil<-max(summer$soiltemp_mean,na.rm=TRUE)

lm_soil_chelsa_summer<-lmer(CHELSA_summer_temp ~ soiltemp_mean + (1|Region), data=summer)
summary(lm_soil_chelsa_summer); r.squaredGLMM(lm_soil_chelsa_summer)

# Predictions for Green
mm.soil <- expand.grid(soiltemp_mean = seq(min_soil, max_soil, 0.1), CHELSA_summer_temp = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_soil_chelsa_summer), mm.soil)  # Create matrix of relevant effect sizes
mm.soil$CHELSA_summer_temp <- mm %*% fixef(lm_soil_chelsa_summer)  # Calculate  based on the relevant effect sizes
pvar.mm.soil <- diag(mm %*% tcrossprod(vcov(lm_soil_chelsa_summer), mm))
mm.soil <- data.frame(mm.soil, plo = mm.soil$CHELSA_summer_temp-1.96*sqrt(pvar.mm.soil),
                      phi = mm.soil$CHELSA_summer_temp+1.96*sqrt(pvar.mm.soil))  # Add errors

soil<-ggplot(summer, aes(soiltemp_mean, CHELSA_summer_temp))+
  geom_ribbon(data = mm.soil, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "goldenrod", alpha = 0.3) +
  geom_line(data = mm.soil, mapping = aes(x = soiltemp_mean, y=CHELSA_summer_temp), colour="goldenrod", size=1) +
  geom_jitter(width = 0.1, height = 0.1, alpha=0.5, pch=16, colour="goldenrod")+
  theme_bw()+
  geom_abline(slope=1, intercept=0, linetype = "dashed")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="CHELSA Air Temperature °C (1979-2013)",x="Measured Soil Temeprature °C")+
  ggtitle("Soil Temperature (summer)")

#Moisture
mean_burial<-mean(summer$Days)
min_moisture<-min(summer$moisture_mean,na.rm=TRUE)
max_moisture<-max(summer$moisture_mean,na.rm=TRUE)

lm_moisture_chelsa_summer<-lmer(CHELSA_summer_temp ~ moisture_mean + (1|Region), data=summer)
summary(lm_moisture_chelsa_summer); r.squaredGLMM(lm_moisture_chelsa_summer)

# Predictions for Green
mm.moisture <- expand.grid(moisture_mean = seq(min_moisture, max_moisture, 0.1), CHELSA_summer_precip = 0)  # Create a blank dataset with the years we want
mm <- model.matrix(terms(lm_moisture_chelsa_summer), mm.moisture)  # Create matrix of relevant effect sizes
mm.moisture$CHELSA_summer_precip <- mm %*% fixef(lm_moisture_chelsa_summer)  # Calculate  based on the relevant effect sizes
pvar.mm.moisture <- diag(mm %*% tcrossprod(vcov(lm_moisture_chelsa_summer), mm))
mm.moisture <- data.frame(mm.moisture, plo = mm.moisture$CHELSA_summer_precip-1.96*sqrt(pvar.mm.moisture),
                          phi = mm.moisture$CHELSA_summer_precip+1.96*sqrt(pvar.mm.moisture))  # Add errors


moisture<-ggplot(summer, aes(moisture_mean, CHELSA_summer_precip))+
  geom_ribbon(data = mm.moisture, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), fill = "blue", alpha = 0.3) +
  geom_line(data = mm.moisture, mapping = aes(x = moisture_mean, y=CHELSA_summer_precip), colour="blue", size=1) +
  geom_jitter(width = 0.1, height = 0.1, alpha=0.5, pch=16, colour="blue")+
  theme_bw()+
  #geom_abline(slope=1, intercept=0, linetype = "dashed")+
  ylim(0,50)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="CHELSA Precipitation mm (1979-2013)",x="Measured Soil Moisture %")+
  ggtitle("Soil Moisture (summer)")

grid.arrange(air,soil,moisture,ncol=3)