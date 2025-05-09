
      
      data {
      int<lower=0> Nobs; //Number of observations
      int<lower=0> NRegion; //Number of regions
      int<lower=0> NSite; //Number of sites
      int<lower=0> NPlot; //Number of plots
      int<lower=0> Nxhat; //No. predictor variables
      int<lower=0> NTea; //No. of tea types
      int<lower=0> NSiteDays; //No. of days
      int<lower=0> NRegionDays; //No. of days
      int<lower=1,upper=NPlot> Plot[Nobs]; //Plots (all observations)
      int<lower=1,upper=NSite> Site[Nobs]; //Plots (all observations)
      int<lower=1,upper=NRegion> Region[Nobs]; //Plots (all observations)
      int<lower=1,upper=2> tea_type_site[NSite]; //Tea type (1=Green, 2=Rooibos)
      int<lower=1,upper=2> tea_type_region[NRegion]; //Tea type (1=Green, 2=Rooibos)
      
      int<lower=0,upper=1> multobs_lobs[Nobs]; //Are sites nested in region (all obs)
      int<lower=0,upper=1> multobs_lplot[NPlot]; //Are sites nested in region (all obs)
      int<lower=0,upper=1> multsites_lobs[Nobs]; //Are sites nested in region (all obs)
      int<lower=0,upper=1> multsites_lplot[NPlot]; //Are sites nested in region (no. plots)
      int<lower=0,upper=1> multsites_lsite[NSite]; //Are sites nested in region (no. plots)
      int<lower=0,upper=1> multsites_lregion[NRegion]; //Are sites nested in region (no. plots)
      int<lower=0,upper=1> multplots_lobs[Nobs]; //Are plots nested in site  (all obs)
      int<lower=0,upper=1> multplots_lplot[NPlot]; //Are plots nested in site  (no plots)
      
      int<lower=0,upper=2> obs_envlevel[Nobs];
      int<lower=0,upper=2> site_envlevel[NSite];
      int<lower=0,upper=2> region_envlevel[NRegion];
      
      vector[Nobs] traitobs; //Mass Loss
      vector[NSite] temp_mean_site; //Temperature (unique regions)
      vector[NSite] temp_sd_site; //Temperature SD (unique regions)
      vector[NRegion] temp_mean_region; //Temperature (unique regions)
      vector[NRegion] temp_sd_region; //Temperature SD (unique regions)
      vector[NSite] SiteDays; //
      vector[NSite] SiteDays_sd; //
      vector[NRegion] RegionDays; //
      vector[NRegion] RegionDays_sd; //
      
      vector[Nxhat] xhat1; //Predictor variables
      vector[Nxhat] xhat3; //Predictor variables
      
      }
      
      parameters {
      real<lower=-3,upper=3> as[NSite];  // Region effect
      real<lower=-5,upper=5> ap[NPlot];
      real<lower=-5,upper=5> aMeanRegion[NRegion];
      real<lower=-2,upper=2> gamma0[NTea];  // intercept of relationship between mass loss and temp change 
      real<lower=-2,upper=2> gamma1[NTea];  // slope of temperature - loss relationship
      real<lower=-2,upper=2> gamma2[NTea];  // slope of CHELSA_summer_temp - loss relationship
      real<lower=-2,upper=2> gamma3[NTea];  // temperature - CHELSA_summer_temp interaction
      real<lower=-2,upper=2> gamma4[NTea];  // temperature - CHELSA_summer_temp interaction
      real<lower=-2,upper=2> gamma5[NTea];  // polynomial relationship with CHELSA_summer_temp

      real<lower=0,upper=5> sigma_overall; //Error around loss- temp relationship
      real<lower=0,upper=5> sigma_plot;
      real<lower=0,upper=5> sigma_site;
      real<lower=0,upper=5> sigma_region;
      real<lower=0,upper=5> sigma_resid;
      
      vector[NSite] temp_pred_site;
      vector[NSite] days_pred_site;
      vector[NRegion] temp_pred_region;
      vector[NRegion] days_pred_region;
      
      }
      
      transformed parameters {
      
      vector[Nobs] mu;   
      vector[Nobs] app;
      vector[Nobs] ass;
      vector[Nobs] arr;
      
      for (i in 1:Nobs){
      
      if((multobs_lobs[i]==1 && multplots_lobs[i]==1))
      app[i] = ap[Plot[i]];
      // set plot effects to 0 for plots that don't have multiple obs or are the only plot within a site
      else app[i] = 0;
      
      if(multsites_lobs[i] == 1)
      ass[i] = as[Site[i]];
      else ass[i] = 0;
      
      arr[i] = aMeanRegion[Region[i]];
      
      mu[i] = app[i] + ass[i] + arr[i];;
      
      }
      
      //print("ap=",ap[1:10],"as=",as[1:10],"aMeanSite=",aMeanSite[1:8],"mu=",mu[1:10])
      
      }
      
      model {
      
      for (i in 1:Nobs){
      traitobs[i] ~ normal(mu[i], sigma_resid);
      }
      
      //Set up plot and site random effects
      
      for (i in 1:NPlot){
      if(multobs_lplot[i]==1 && multplots_lplot[i]==1)
      ap[i] ~ normal(0, sigma_plot);
      }
      
      for (i in 1:NSite){
      if(multsites_lsite[i]==1)
      as[i] ~ normal(0, sigma_site);
      }
      
      //Bring in environmental data means and SD per region
      
      for (i in 1:NRegion){
      temp_pred_region[i] ~ normal(temp_mean_region[i], temp_sd_region[i]); //temp_mean_region and temp_sd are given as data
      days_pred_region[i] ~ normal(RegionDays[i], RegionDays_sd[i]); //temp_mean_region and temp_sd are given as data
      }
      
      for (i in 1:NSite){
      temp_pred_site[i] ~ normal(temp_mean_site[i], temp_sd_site[i]); //temp_mean_region and temp_sd are given as data
      days_pred_site[i] ~ normal(SiteDays[i], SiteDays_sd[i]); //temp_mean_region and temp_sd are given as data
      }
      
      //Relationship between mass loss at the region level and temperature and CHELSA_summer_temp, per tea type
      
      
      for (i in 1:NRegion){
      aMeanRegion[i] ~ normal(gamma0[tea_type_region[i]] + gamma1[tea_type_region[i]]*temp_pred_region[i] + gamma4[tea_type_region[i]]*days_pred_region[i] + gamma5[tea_type_region[i]]*square(temp_pred_region[i]) , sigma_overall); 
      }
      
      
      } //Close model
      
      generated quantities{
      
      matrix[Nxhat,NTea] preds; //matrix of predictions
      real<lower=-5,upper=5> teaDiff;
      
      for (i in 1:Nxhat){
      for (j in 1:NTea){
      preds[i,j] = (gamma0[j] + gamma1[j]*xhat1[i] + gamma4[j]*xhat3[i] + gamma5[j]*square(xhat1[i])); //predictions 
      }
      }
      
      teaDiff <- gamma0[1]-gamma0[2]; //AB: if you want to know whether the intercepts of the tea types are significantly different, can also do this with the slopes or a prediction at a particular xhat if you want
      
      }
      
      
