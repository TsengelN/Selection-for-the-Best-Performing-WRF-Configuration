################################################################################
# This Code compares different WRF sensitivity runs using AMET Spatial Surface statistics and 
# plot spatial plot for a Winner Config.

# Developed by: Tsengel Nergui, Ph.D., Atmospheric Modeler, LADCO
# Update history:


# Updated for speeding up the loops:07/26/2019
# ref: https://www.r-bloggers.com/strategies-to-speedup-r-code/


# Updated for T-testing for 4DU components: 06/10/2019
# PART 3: Convert UTC to Local Standard Time (LST) at each site;
#         Apply Student's T-testing for difference of model performance metrics (MPM)
#         for Sunrise/Sunset/Daytime/Nighttime at each site

# Initial version: 05/17/2019
# PART 1: Preping site-specific statistics
# PART 2: FINAL X-Y plots for the Best Config (Lowest MAE) Selection

################################################################################


rm(list=ls())

# Libraries ---------------------------------------------------------------

library(stats)
library(plyr)  # dplyr must be called before dplyr
library(dplyr) # data manipulation
library(ggplot2) # plotting
library(ggrepel) # better label handling
library(data.table) # faster handling a large files

# setting wrk dir---
getwd()
setwd("C:/Users/nargu/My R workplace/MET_evaluation")


# -----------------------------------------------------------
# PART 1: Preping site-specific statistics
# -----------------------------------------------------------

domain <- "d01"  
variable <- "mixr2m" #"mixr2m" #"wnddir10m" #"wnddir10m" #"temp2m"
period <- "20160610-20160619"

if (domain == "d01") { 
  sens1 <- paste("USEPA2016_APLX_NAM_gda_",domain,sep = "");sens1
  insens1 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens1,"/spatial_surface_ddhh/",sens1,".spatial.",variable,".stats.",period,".csv",sep = "");insens1
  
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  insens2 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens2,"/spatial_surface_ddhh/",sens2,".spatial.",variable,".stats.",period,".csv",sep = "");insens2
  
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
  insens3 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens3,"/spatial_surface_ddhh/",sens3,".spatial.",variable,".stats.",period,".csv",sep = "");insens3
  

} else if (domain == "d02") { 
sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
insens1 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens1,"/spatial_surface_ddhh/",sens1,".spatial.",variable,".stats.",period,".csv",sep = "");insens1

sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
insens2 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens2,"/spatial_surface_ddhh/",sens2,".spatial.",variable,".stats.",period,".csv",sep = "");insens2

sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
insens3 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens3,"/spatial_surface_ddhh/",sens3,".spatial.",variable,".stats.",period,".csv",sep = "");insens3


} else if (domain == "d03") { 
  sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
  insens1 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens1,"/spatial_surface_ddhh/",sens1,".spatial.",variable,".stats.",period,".csv",sep = "");insens1
  
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  insens2 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens2,"/spatial_surface_ddhh/",sens2,".spatial.",variable,".stats.",period,".csv",sep = "");insens2
  
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
  insens3 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens3,"/spatial_surface_ddhh/",sens3,".spatial.",variable,".stats.",period,".csv",sep = "");insens3

}



# reading site-specific statisticts files and adding header ---
domain
rm(dat.sens1,dat.sens2,dat.sens3)
dat.sens1 <- read.csv(file=insens1,skip=1, header = FALSE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                     colClasses = c("character",rep ("numeric",2),"character","integer",rep ("numeric",16)))
colnames(dat.sens1) <- c ("station_dd_hh","lat","lon","state","count","corr","ac","var","sdev","rmse","mae","bias","mfbias","mnbias","mngerr","nmbias","nmerr","max","min",
                         "mod","obs")  # last two column headers are edu guesses because no header was written out from AMET. NT

# droping NA columns before removing NA rows ---
dat.sens1 <- subset(dat.sens1, select = -c(corr, ac))

# NA rows been removed ---
dat.sens1[complete.cases(dat.sens1),] -> dat.sens11  # writing complete cases to new df
head(dat.sens11)
summary(dat.sens11)

dat.sens2 <- read.csv(file=insens2,skip=1, header = FALSE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                      colClasses = c("character",rep ("numeric",2),"character","integer",rep ("numeric",16)))
colnames(dat.sens2) <- c ("station_dd_hh","lat","lon","state","count","corr","ac","var","sdev","rmse","mae","bias","mfbias","mnbias","mngerr","nmbias","nmerr","max","min",
                          "mod","obs")  # last two column headers are edu guesses because no header was written out from AMET. NT

# droping NA columns before removing NA rows ---
dat.sens2 <- subset(dat.sens2, select = -c(corr, ac))

# NA rows been removed ---
dat.sens2[complete.cases(dat.sens2),] -> dat.sens22  # writing complete cases to new df
summary(dat.sens22)

dat.sens3 <- read.csv(file=insens3,skip=1, header = FALSE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                      colClasses = c("character",rep ("numeric",2),"character","integer",rep ("numeric",16)))
colnames(dat.sens3) <- c ("station_dd_hh","lat","lon","state","count","corr","ac","var","sdev","rmse","mae","bias","mfbias","mnbias","mngerr","nmbias","nmerr","max","min",
                          "mod","obs")  # last two column headers are edu guesses because no header was written out from AMET. NT

# droping NA columns before removing NA rows ---
dat.sens3 <- subset(dat.sens3, select = -c(corr, ac))
# NA rows been removed ---
dat.sens3[complete.cases(dat.sens3),] -> dat.sens33  # writing complete cases to new df
summary(dat.sens33)



# merging CASEs data into one df ---
library(dplyr)
length(unique(dat.sens22$station_dd_hh))
# intersect gives overlapping variables with duplication is removed
length(intersect(dat.sens11$station_dd_hh,dat.sens22$station_dd_hh))

# set difference, duplicates removed (10)
setdiff(dat.sens11$station_dd_hh,dat.sens22$station_dd_hh)
# union gives all unique combinations with duplicatation is removed
length(union(dat.sens11$station_dd_hh,dat.sens22$station_dd_hh))


# merge 2+ dframes by a connecting variable, keep only common rows ---
MyMerge <- function(x, y){
  df <- merge(x, y, by= "station_dd_hh", all.x= FALSE, all.y= FALSE, suffixes = c(".1", ".2",".3"))
  return(df)
}

rm(tmp_3cases0,tmp_3cases)
# had to combine max of 3 dfs at a time ---
tmp_3cases0 <- Reduce(MyMerge, list(dat.sens11,dat.sens22,dat.sens33))
colnames(tmp_3cases0)
keeps <- c("station_dd_hh","lat.1","lon.1","state.1","obs","mod.1","mod.2","mod",
           "rmse.1", "mae.1", "bias.1","mfbias.1","mnbias.1","mngerr.1","nmbias.1","nmerr.1",
           "rmse.2", "mae.2", "bias.2","mfbias.2","mnbias.2","mngerr.2","nmbias.2","nmerr.2",
           "rmse",   "mae",   "bias",  "mfbias",  "mnbias",  "mngerr",  "nmbias", "nmerr" )

tmp_3cases <- tmp_3cases0[,keeps]
colnames(tmp_3cases) <- c("station_dd_hh","lat","lon","state","obs","mod1","mod2","mod3",
                       "rmse.1", "mae.1", "bias.1","mfbias.1","mnbias.1","mngerr.1","nmbias.1","nmerr.1",
                       "rmse.2", "mae.2", "bias.2","mfbias.2","mnbias.2","mngerr.2","nmbias.2","nmerr.2",
                       "rmse.3", "mae.3", "bias.3","mfbias.3","mnbias.3","mngerr.3","nmbias.3","nmerr.3" )


head(tmp_3cases)


#seperating columns ---
library(tidyr)
tmp_3cases <- tmp_3cases %>%
  tidyr::separate(station_dd_hh, into = c("station_id", "date", "time"), sep = "_")

head(tmp_3cases)
tmp_3cases$date_time <- paste(tmp_3cases$date,tmp_3cases$time,sep = " ")

domain
#converting to POSIXct obj ---
library(lubridate)
## Note: parse_date_time parses an input vector into POSIXct date-time object, at which requires timezone infor for pase output.
## thus, I gave tz ="UTC", because AMET for MET collocates at UTC.
tmp_3cases$datetime_UTC <- parse_date_time(tmp_3cases$date_time, orders = "%Y-%m-%d %H", tz = "UTC")
dim(tmp_3cases)

glimpse(tmp_3cases)

# Converting GMT/UTC time to Local Standard Time (LST)---
#https://cran.r-project.org/web/packages/lutz/lutz.pdf
#https://www.timeanddate.com/worldclock/usa/chicago   for testing

# TEST for a site ---
# library(lutz)
# library(sf)
# rm(lat,lon)
# lon =  -104.5
# lat = 39.5
# # time zone by timezone name
# rm(tz_name,tz_shift)
# tz_name <- tz_lookup_coords(lat, lon, method = "accurate", warn = TRUE);tz_name
# tz_shift <- tz_offset("2019-01-16", tz_name);tz_shift   # timezone shift for DST
# 
# # adding timzezone shift onto GMT to get Local DST (local Daytime Saving Time)---
# # NOTE: output would still be seen as in GMT or UTC, it is actually in local DST. NT 
# library(lubridate)
# x <- tmp_5cases[1,36];x
# x + hours(tz_shift ) # add tz_shift in hour

# looping through sites for changing GMT/UTC to DST ---
library(dplyr)
glimpse(tmp_3cases)
dim(tmp_3cases)[1]
dim(tmp_3cases)
tmp_3cases$tz <- NA
tmp_3cases$datetime_LST <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")

# finding column index by name ---
lat.idx <- which(colnames(tmp_3cases)=="lat");lat.idx
lon.idx <- which(colnames(tmp_3cases)=="lon");lon.idx
dtUTC.idx <- which(colnames(tmp_3cases)=="datetime_UTC");dtUTC.idx
tz.idx <- which(colnames(tmp_3cases)=="tz");tz.idx
dtLST.idx <- which(colnames(tmp_3cases) == "datetime_LST");dtLST.idx


# required libraries ---
library(lutz)
#https://cran.rstudio.com/src/contrib/sf_0.7-7.tar.gz
#install.packages("sf")
library(sf)
library(lubridate)
domain

i =  1
for(i in i:dim(tmp_3cases)[1]) {
  print(paste("Reading line ", i, "out of ", dim(tmp_3cases)[1] ))
  
  lat =  tmp_3cases[i,lat.idx] # latitude
  lon =  tmp_3cases[i,lon.idx] # longitute
  #lat;lon
  # time zone by timezone name
  tz_name <- lutz::tz_lookup_coords(lat, lon, method = "accurate", warn = FALSE );tz_name
  ## DST: tz_shift <- lutz::tz_offset(tmp_3cases[i,36], tz_name)$utc_offset_h;tz_shift   # timezone shift for DST
  tz_shift <- lutz::tz_offset("2016-01-01 00:00:00", tz_name)$utc_offset_h;tz_shift   # timezone shift for LST
  tmp_3cases[i,tz.idx] <- as.integer(tz_shift)
  tmp_3cases[i,dtLST.idx] <- tmp_3cases[i,dtUTC.idx] + hours(as.integer(tz_shift))
  
  # Empty all tmp variables
  rm(lat,lon,tz_name,tz_shift)
} # end of i-loop in data length

i

tail(tmp_3cases) 
colnames(tmp_3cases)
# dropping some column(s) before writing out ---
rm(dat0)
dat0 <- subset(tmp_3cases,select = -c(date_time))
head(dat0)

domain
variable
period
dim(dat0)

outdir = getwd();outdir
outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/site_ts_sens1-3_",variable,"_",domain,"_",period,".csv",sep = ""));outfile
write.table (dat0,outfile,row.names = F,col.names = T,sep = ",", quote = FALSE)

# Reading dat0 as an input ---
domain <- "d01"  
variable <- "mixr2m" #"mixr2m" #"wnddir10m" #"temp2m"
period <- "20160610-20160619"
indir = getwd();indir
infile <- (paste(indir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/site_ts_sens1-3_",variable,"_",domain,"_",period,".csv",sep = ""));infile
rm(dat0)
dat0 <- read.csv(file=infile,skip=0, header = TRUE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                 colClasses = c(rep("character",2),"integer",rep ("numeric",2),"character",
                                rep ("numeric",28),"character","integer","character"))

glimpse(dat0)
library(dplyr)
tail(dat0)
# geting date and time as a seperate columns ---
library(lubridate)
head(dat0)
dat0$date_LST <- date(dat0$datetime_LST)
dat0$month_LST <- month(dat0$datetime_LST)
dat0$day_LST <- day(dat0$datetime_LST)
dat0$hour_LST <- hour(dat0$datetime_LST)
str(dat0)
head(dat0)
sort(unique(dat0$hour_LST))


# site.loc.dateLST list ---
head(dat0)
glimpse(dat0)

dat0$connector <- paste(dat0$station_id,dat0$lat,dat0$lon,dat0$date_LST,sep = "_")
site.loc.dateLST <- unique(dat0$connector)
head(site.loc.dateLST)

# create empty df for site and date specific sunset/sunrise info ---
rm(site.date.sun)
site.date.sun <- c()

require(lubridate)
i =1
for(i in 1:length(site.loc.dateLST)) {
  idx <- which(dat0$connector == site.loc.dateLST[i]);idx
  
# -----------------------------------------------------------
# PART 4: Calculating approx. Sunrise and Sunset in LST using a combination of 
# a) Brad Pierce's IDL code for Solar Zenith Angle
# b) https://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html
#    NOAA_Solar_Calculations_day.xls
# The calculations of the NOAA Sunrise/Sunset and Solar Position Calculators
# are based on equations from Astronomical Algorithms, by Jean Meeus.
# -----------------------------------------------------------
  dat0[idx[1],]
  col.idx <- which(colnames(dat0) %in% c("station_id","lat","lon","tz","date_LST"))
  site     <- dat0[idx[1],col.idx[1]] # station_id
  lat      <- dat0[idx[1],col.idx[2]] # latitude
  lon      <- dat0[idx[1],col.idx[3]] # longitude
  tzone    <- dat0[idx[1],col.idx[4]] # time_zone
  date_LST <- dat0[idx[1],col.idx[5]] # date_LST
  
  # julian Day ---
  doy <- lubridate::yday(date_LST);doy   # Julian day of LST
  
  # convertors between  radian <-> degree ---
  r2d =180./pi 
  d2r=pi/180.

  # calculate Equation of Time value ---
  BETA = (2.*pi*(doy-81.0))/365.
  EQTIME = 9.87*sin(2*BETA) - 7.53*cos(BETA) - 1.5*sin(BETA)
  # calculate Declination angle ---;  ##$T$2 in excel file
  DECLN = 23.45 * sin(2.*pi * ((284+doy)/365));DECLN

  ## from "NOAA_Solar_Calculations_day.xls"
  #Hour Angle Sunrise (deg) ---; ##$W$2  =DEGREES(ACOS(COS(RADIANS(90.833))/(COS(RADIANS($B$3))*COS(RADIANS(T2)))-TAN(RADIANS($B$3))*TAN(RADIANS(T2))))
  HA_sunrise <- r2d*(acos(cos(d2r*90.833)/(cos(d2r*lat)*cos(d2r*DECLN))-tan(d2r*lat)*tan(d2r*DECLN)))
  # Solar Noon (time fraction in LST) ---;  ## $X$2  =(720-4*$B$4-V2+$B$5*60)/1440
  sol_noon <- (720 - 4*lon - EQTIME + tzone*60)/1440
  # Sunrise and Sunset times (timefraction in LST) ---; ## $Y$2  =X2-W2*4/1440 ## $Z$2  =X2+W2*4/1440
  sunrise <- as.integer(round(24*(sol_noon-HA_sunrise*4/1440)));sunrise
  sunset <- as.integer(round(24*(sol_noon+HA_sunrise*4/1440)));sunset

  # Define morning and evening transition periods, and day and night periods---
  #----------------------------------------------------------------------------
  # hr_sunrise ---
  hr_sunrise <- c(sunrise-2,sunrise-1,sunrise,sunrise+1,sunrise+2);hr_sunrise
  rm(idx1,idx2,idx3,idx4)
  idx1 <- which(hr_sunrise == -1)
  idx2 <- which(hr_sunrise == -2)
  idx3 <- which(hr_sunrise == 24)
  idx4 <- which(hr_sunrise == 25)
  if(length(idx1) == 1) {hr_sunrise[idx1] <- 24 + hr_sunrise[idx1]}
  if(length(idx2) == 1) {hr_sunrise[idx2] <- 24 + hr_sunrise[idx2]}
  if(length(idx3) == 1) {hr_sunrise[idx3] <- hr_sunrise[idx3] - 24}
  if(length(idx4) == 1) {hr_sunrise[idx4] <- hr_sunrise[idx4] - 24}
  
  # hr_sunset ---
  hr_sunset <- c(sunset-2,sunset-1,sunset,sunset+1,sunset+2);hr_sunset
  rm(idx1,idx2,idx3,idx4)
  idx1 <- which(hr_sunset == -1)
  idx2 <- which(hr_sunset == -2)
  idx3 <- which(hr_sunset == 24)
  idx4 <- which(hr_sunset == 25)
  if(length(idx1) == 1) {hr_sunset[idx1] <- 24 + hr_sunset[idx1]}
  if(length(idx2) == 1) {hr_sunset[idx2] <- 24 + hr_sunset[idx2]}
  if(length(idx3) == 1) {hr_sunset[idx3] <- hr_sunset[idx3]-24 }
  if(length(idx4) == 1) {hr_sunset[idx4] <- hr_sunset[idx4]-24 }
  
  # hr_daytime is seq. between the last element of sunrise and the first element of sunset ---
  hr_daytime <- seq(hr_sunrise[length(hr_sunrise)]+1,hr_sunset[1]-1,1);hr_daytime
  
  # hr_nighttime is whatever is not sunrise/daytime/sunset ---
  hour24 <- seq(0,23,1);hour24
  idx.n <- which(! hour24 %in% c(hr_sunrise,hr_daytime,hr_sunset))  
  hr_nighttime <- hour24[idx.n];hr_nighttime
  rm(idx.n)
   
  # hr_sunrise
  # hr_daytime
  # hr_sunset
  # hr_nighttime
   
  # creating site-and-date specific 4 periods and its hour list ---
  #----------------------------------------------------------------
  tmp_file <- data.frame(station_id = site, lat = lat, lon = lon, tz = tzone, date_LST = date_LST,
                   period=c("sunrise","daytime","sunset","nighttime"))
  tmp_file$hours <- list(hr_sunrise,hr_daytime,hr_sunset, hr_nighttime)
  
  site.date.sun <- rbind(site.date.sun, tmp_file)
  n_lines <- dim(tmp_file)[1]
  print(paste("Segregating DU periods for", i, "/", length(site.loc.dateLST)," site.", site,":", n_lines, "lines were processed.",sep = " " ))
  # Empty the tmp buffer
  rm(tmp_file,idx, doy,BETA,EQTIME,DECLN,HA_sunrise,sol_noon, sunrise,sunset,hr_sunrise, hr_daytime, hr_sunset, hr_nighttime)
  
} #loop end of sitename list

i
head(site.date.sun)

# accessing to numeric list ---
site.date.sun[1,7]  # or 
site.date.sun$hours[1]
str(site.date.sun$hours[1])
head(site.date.sun)
# #to convert list column into a character vector before writing out ---
# site.date.sun$hours <- vapply(site.date.sun$hours, paste, collapse = ", ", character(1L))
# 
# outdir = getwd();outdir
# outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/site_location_dateLST_sunrise_sunset_",domain,"_",period,".csv",sep = ""));outfile
# write.table (site.date.sun,outfile,row.names = F,col.names = T,sep = ",", quote = FALSE)


# BIG LOOP by du_idx for duirnal periods for Sig Testing ----
#------------------------------------------------------------
site.date.sun$connector <- paste(site.date.sun$station_id,site.date.sun$lat,site.date.sun$lon,site.date.sun$date_LST,sep = "_")
head(dat0)
head(site.date.sun)

rm(dat0.sun)
dat0.sun <- merge.data.frame(x = dat0, y = site.date.sun, by= "connector", all.x= TRUE, all.y= FALSE, suffixes = c("", ".sun"))
head(dat0.sun)

unique(dat0.sun$period)

# BIG LOOP for 4 duirnal periods ---
du_period <- unique(dat0.sun$period);du_period
#du_idx = 2

for (du_idx in c(1, 2, 3, 4) ) {
sel_period <- du_period[du_idx] ;sel_period  
# creating df specific to selected duirnal band and sitenamelist ---
rm(dat1)
dat1 <- dat0.sun %>% dplyr::filter(period == du_period[du_idx] & hour_LST %in% hours[[du_idx]]) 
colnames(dat1)

# STEP 1: Test Statistics for selected period
head(dat1)
rm(sitenamelist)
sitenamelist <- unique(dat1$station_id);length(sitenamelist)
str(sitenamelist)
sel_period
domain
variable
if (domain == "d01" ) { 
  if ( variable == "temp2m" )  {  
    if (sel_period == "sunrise")  { sites_excluded <- c("KFFZ","KMJX","KNBT","MMDO","MMPE","NR01") }
    if (sel_period == "daytime")  { sites_excluded <- c("CWAJ","NR02") }
    if (sel_period == "sunset")   { sites_excluded <- c("KFFZ","KNUC","MMMV","MMNL") }
    if (sel_period == "nighttime"){ sites_excluded <- c("KABH","KDYA","MBPV","MMAN","MMCS","MMDO") }
  } # temp2m 
  if ( variable == "mixr2m" )  {  
    if (sel_period == "sunrise")  { sites_excluded <- c("KFFZ","KMJX","KNBT","MMDO","MMPE","NR01") }
    if (sel_period == "daytime")  { sites_excluded <- c("CWAJ","NR02","KABH") }
    if (sel_period == "sunset")   { sites_excluded <- c("KFFZ","KNUC","MMMV","MMNL","MMAN","MMPG") }
    if (sel_period == "nighttime"){ sites_excluded <- c("KABH","KDYA","MBPV","MMAN","MMCS","MMDO","MMML") }
  } # mixr2m
  if ( variable %in% c("wnddir10m", "wndspd10m") ) {
    if (sel_period == "sunrise")  { sites_excluded <- c("K5R8","KBKB","KCDD","KHSA","KI35","KISM","KMMI","KMWT","KSBS","KVDF","MMAN","MMDO","MMIO","MMMV","MMPE","NR01") }
    if (sel_period == "daytime")  { sites_excluded <- c("CWAJ","KDMH","NR02") }
    if (sel_period == "sunset")   { sites_excluded <- c("KCQB","KNUC","MMMV") }
    if (sel_period == "nighttime"){ sites_excluded <- c("K0VG","KACP","KADF","KAQR","KBGF","KI35","KJFX","KJVW","KM30","KM91","KMEH","KOLV","KRYT","KSBD","KVBS","MBPV","MMAN","MMDO","MMHO","NA") }
  } # winddir10m & windspd10m 
  
} # d01


if (domain == "d02" ) {
  if ( variable == "temp2m" ) {
    if (sel_period == "daytime")  { sites_excluded <- c("CWAJ","NR02") } # only 1 data entry for sel_period of "daytime"}
   } # temp2m

  if ( variable %in% c("wnddir10m", "wndspd10m") ) {
    if (sel_period == "sunrise")  { sites_excluded <- c("KCDD","KI35","KMMI") }
    if (sel_period == "daytime")  { sites_excluded <- c("CWAJ","NR02") }
    if (sel_period == "sunset")   { sites_excluded <- c("KCQB") }
    if (sel_period == "nighttime"){ sites_excluded <- c("K0VG","KBGF","KI35","KM30","KM91") }
  } # winddir10m  & winspd10m  
} # d02 

sites_excluded
# excluding sites from original dataset
## QA-1 for for excluding very short time series from analysis
#---------------------------------------------------------------
length(sitenamelist)  
rm(index)
index <- which(!(sitenamelist  %in% sites_excluded)) # not equal to the excluding sites
sitenamelist[index]
sitenamelist <- sitenamelist[index]
length(sitenamelist) 
  

# STEP 1: writing out DU specific MPS (model Performance Statistics), later needed for PLOT 3: MAE and BIAS
# dropping NA columns before removing NA rows ---
rm(dat.out,dat.out.sum)
dat.out <- subset(dat1, select = -c(connector,state,date,time,tz, datetime_UTC,datetime_LST,date_LST,month_LST,day_LST,hour_LST,station_id.sun, date_LST.sun,lat.sun,lon.sun, tz.sun, period,hours))
head(dat.out)

dat.out.sum <- dat.out %>% filter(station_id %in% sitenamelist) %>%
  group_by(station_id) %>% 
  summarise_all(funs(mean,sd,count=n())) # gives average, sd, and count

outdir = getwd();outdir
sel_period
domain  
variable
period
rm(outfile)
outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MPS_LADCO_sens1-3_",variable,"_",domain,"_", period,".csv",sep = ""));outfile
write.table (dat.out.sum,outfile,row.names = F,col.names = T,sep = ",", quote = FALSE)


# creating empty df for Test statistics ---
rm(Stat_signif)
Stat_signif <- data.frame(site=character(),
                           lat=numeric(), 
                           lon=numeric(),
                           mod1_mae=numeric(), 
                           mod2_mae=numeric(),
                           mod3_mae=numeric(),
                           first_lowest = character(),
                           first_lowest_mae = numeric(),
                           second_lowest = character(),
                           second_lowest_mae = numeric(),
                           mod12_tval=numeric(), 
                           mod13_tval=numeric(),
                           mod23_tval=numeric(),
                           M2M_Ts = character(),
                           M2M_Ts_value = numeric(),
                           T_critical = numeric(),
                           Ts2Tc = numeric(),
                           best_mod = character(),
                           best_mea = numeric(),
                           stringsAsFactors = FALSE)


i = 1
for (i in i:length(sitenamelist)) {

  sel.site <- sitenamelist[i]
  print(paste(du_idx,sel_period,": Running Stat Testing for", i, "/", length(sitenamelist)," Site:", sel.site, sep = " " ))
  idx <- which(dat1$station_id == sel.site);idx
  site.dat0 <- dat1 %>% dplyr::filter(dat1$station_id == sel.site ) %>% dplyr::select(mae.1,mae.2,mae.3) 
  site.dat <- site.dat0[complete.cases(site.dat0),] 
  dat1[idx[1],]
  Stat_signif[i,1] <- sel.site
  Stat_signif[i,2] <- dat1[idx[1],5] #lat
  Stat_signif[i,3] <- dat1[idx[1],6] #lon
  Stat_signif[i,4] <- mean(site.dat[,1],na.rm=TRUE)  # average mae for entire simulation period at a site
  Stat_signif[i,5] <- mean(site.dat[,2],na.rm=TRUE)
  Stat_signif[i,6] <- mean(site.dat[,3],na.rm=TRUE) 
  temp1 <- Stat_signif[i,4:6]
  nth1 <- which.min(apply(temp1, MARGIN = 2, min));nth1  # MARGIN = 2 is for getting column name
  # higher the p-value (higher p-val supports H0) indicates lesser difference between MOD and OBS, which means MOD dif a better job
  name1 <- names(nth1)
  Stat_signif[i,7] <- unlist(strsplit(name1, "[_]"))[1]
  Stat_signif[i,8] <- Stat_signif[i, 3 + nth1]
  # temporarily assinging 999 for getting the second min
  temp2 <- temp1
  temp2[,nth1] <- 999.9
  nth2 <- which.min(apply(temp2, MARGIN = 2, min))
  name2 <- names(nth2)
  Stat_signif[i,9] <- unlist(strsplit(name2, "[_]"))[1]
  Stat_signif[i,10] <- Stat_signif[i, 3 + nth2]
  
  name1
  name2
  dd <- c(as.integer(substr(name1, 4,4)),as.integer(substr(name2, 4,4)));dd
  suffix <- paste0(min(dd),max(dd))  # ordered in low-to-high
  tvalm2m <- paste("mod",suffix,"_tval",sep = "");tvalm2m
  # student T-test T-statistics on difference of 2 arrays
  # **      Null Hypothesis: mean(AC1-AC2)=0, AC1-AC2 follows normal distribution.
  # **      plot the 5% conf interval of difference of means : F*SD/sqrt(N-1),
  # **      Tc=1.96 for infinite samples, Tc=2.0 for nsz=60, Tc=2.042 for nsz=30, Tc=2.228 for nsz=10
  #    if(nsz>=80);            'define Tc1=1.960, Tc2=2.576, Tc3=3.291 for 95.0%, 99.0%, 99.9% confidence intervals '  ;endif
  #    if(nsz>=40 & nsz <80);  'define Tc1=2.000, Tc2=2.660, Tc3=3.460 '  ;endif
  #    if(nsz>=20 & nsz <40);  'define Tc1=2.042, Tc2=2.750, Tc3=3.646 '  ;endif
  #   if(nsz<20);              'define Tc1=2.228, Tc2=3.169, Tc3=4.586 '  ;endif
  
  #sd(a11-a22)
  #hist(a11-a22)
  #t.test(a11-a22,alternative="two.sided",mu = 0.0,var.equal=FALSE,conf.level = 0.95)
  #     0.58628*1.690253/sqrt(8)
  site.dat
  tmp12.pval <- t.test(site.dat[,1]-site.dat[,2],mu = 0.0,alternative="two.sided",var.equal=FALSE,conf.level = 0.95)$p.value; tmp12.pval
  tmp13.pval <- t.test(site.dat[,1]-site.dat[,3],mu = 0.0,alternative="two.sided",var.equal=FALSE,conf.level = 0.95)$p.value; tmp13.pval
  tmp23.pval <- t.test(site.dat[,2]-site.dat[,3],mu = 0.0,alternative="two.sided",var.equal=FALSE,conf.level = 0.95)$p.value; tmp23.pval
  
  if (tmp12.pval <= 0.05) {
    Stat_signif[i,11] <- abs(t.test(site.dat[,1]-site.dat[,2],mu = 0.0,alternative="two.sided",var.equal=FALSE,conf.level = 0.95)$statistic)
  }  else {Stat_signif[i,11] <- NA } #  T-statistics ; assinging NA for "insignificant difference 
  if (tmp13.pval <= 0.05) {
    Stat_signif[i,12] <- abs(t.test(site.dat[,1]-site.dat[,3],mu = 0.0,alternative="two.sided",var.equal=FALSE,conf.level = 0.95)$statistic)
  } else { Stat_signif[i,12] <- NA }
  if (tmp23.pval <= 0.05) {
    Stat_signif[i,13] <- abs(t.test(site.dat[,2]-site.dat[,3],mu = 0.0,alternative="two.sided",var.equal=FALSE,conf.level = 0.95)$statistic)
  } else { Stat_signif[i,13] <- NA }

  Stat_signif[i,14] <- tvalm2m
  if (tvalm2m == "mod12_tval" ) { Stat_signif[i,15] <-  Stat_signif[i,11]}
  if (tvalm2m == "mod13_tval" ) { Stat_signif[i,15] <-  Stat_signif[i,12]}
  if (tvalm2m == "mod23_tval" ) { Stat_signif[i,15] <-  Stat_signif[i,13]}

  # to get criteria value ---
  #library(stats2)
  alpha1 = 0.95
  T.c <- qt((1-alpha1)/2,df = dim(site.dat)[1]-1,lower.tail=FALSE);T.c
  Stat_signif[i,16] <- T.c
  # following condition checks if M2M_Ts_value was statistically significant different than Tc at 95% confidence interval 
  if (is.na(Stat_signif[i,15]) == FALSE ) {     
    Stat_signif[i,17] <- Stat_signif[i,15]/T.c   # ratio of Ts to Tc
    Stat_signif[i,18] <- paste(Stat_signif[i,7],"*",sep = "")  # * indicates significant at a =0.95
    Stat_signif[i,19] <- Stat_signif[i,8]   
  } else {
    Stat_signif[i,17] <- NA 
    Stat_signif[i,18] <- paste("mod",suffix,sep = "")
    Stat_signif[i,19] <- 0.5*(Stat_signif[i,8] + Stat_signif[i,10])  }
 
  rm(sel.site, idx, site.dat0,site.dat,nth1,nth2,name1,name2,temp1,temp2,T.c,dd,suffix,list=ls(pattern = "*.pval"))
  
  } # end of sitelist loop

i
sel.site

head(Stat_signif)
unique(Stat_signif$best_mod)

# STEP 2: writing out df with statistically significant Best config ---
outdir = getwd();outdir
sel_period
domain  
variable
period
rm(outfile)
outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MAE_LADCO_sens1-3_Best_Config_Signif95_",variable,"_",domain,"_",period,".csv",sep = ""));outfile
write.table (Stat_signif,outfile,row.names = F,col.names = T,sep = ",", quote = FALSE)


} # go back to BIG LOOP for next duirnal loop index ---


#-------------------------------------------------------------
# PART 2: FINAL X-Y plots for the Best Config (Lowest MAE*) Selection    ---
#-------------------------------------------------------------
# these are packages you will need, but probably already have.
# Don't bother installing if you already have them
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
# some standard map packages.
#install.packages(c("maps", "mapdata"))

#get world data ---
library(maps)
library(mapdata)
library(mapproj)
library(ggplot2)
library(dplyr)

library(mapdata)
states <- map_data("state")
head(states)
unique(states$region)
# The Midwest and Northeast US region---
LADCO <- c("Minnesota", "wisconsin", "Iowa", "Missouri","Ohio","Illinois","Indiana",
           "Michigan","Kentucky", "West Virginia","Virginia","Maryland","Deleware","Pennsylvania",
           "New York", "New Jersey","Connecticut","Rhode Island","New Hampshire","Massachusetts","Maine")  # graph purpose

Michigan <- c("wisconsin", "Michigan")  # graph purpose


# reading input file ---
indir <- getwd()
indir
# du_idx loop for PLOTTING ---
du_period <- c("sunrise","daytime","sunset","nighttime")
domain <- "d01"  
variable <- "mixr2m" #"mixr2m" #"temp2m" #"wnddir10m" 
period <- "20160610-20160619"

du_idx = 4
# looping through du_idx ---
for (du_idx in c(1, 2, 3, 4) ) {
sel_period <- du_period[du_idx] ;sel_period

# reading Stat_signif as Explore_Factors ---
rm(Explore_Factors)
infile <- (paste(indir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MAE_LADCO_sens1-3_Best_Config_Signif95_",variable,"_",domain,"_",period,".csv",sep = ""));infile
Explore_Factors <- read.csv(file=infile,skip=0, header = TRUE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                            colClasses = c("character",rep ("numeric",5),rep(c("character","numeric"),2),
                                           rep ("numeric",3),"character",rep ("numeric",3),"character","numeric"))


# #### COOKIE CUT sites in d03 from d02 ---
# head(Explore_Factors)
# sites.d03 <- unique(Explore_Factors$site);sites.d03
# 
# domain <- "d02"  
# variable <- "temp2m" #"temp2m" #"wnddir10m" 
# period <- "20160610-20160619"
# 
# # reading Stat_signif as Explore_Factors ---
# rm(Explore_Factors,Ecplore_Factors0)
# infile <- (paste(indir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MAE_LADCO_sens1-3_Best_Config_Signif95_",variable,"_",domain,"_",period,".csv",sep = ""));infile
# Explore_Factors0 <- read.csv(file=infile,skip=0, header = TRUE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
#                             colClasses = c("character",rep ("numeric",5),rep(c("character","numeric"),2),
#                                            rep ("numeric",3),"character",rep ("numeric",3),"character","numeric"))
# 
# head(Explore_Factors0)
# Explore_Factors <- Explore_Factors0 %>% 
#   dplyr::filter(Explore_Factors0$site %in% sites.d03)  
# 
# 
# #### END of COOKIE CUT sites in d03 from d02 ---

if (domain == "d01") { 
  sens1 <- paste("USEPA2016_APLX_NAM_gda_",domain,sep = "");sens1
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3

} else if (domain == "d02") { 
  sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3

} else if (domain == "d03") { 
  sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3

}

# tallies ---
library(dplyr)
count(Explore_Factors, 'station_id')
head(Explore_Factors)

rm(tally.b)
tally.b <- Explore_Factors %>% 
  dplyr::select(best_mod,best_mea) %>% 
  group_by(best_mod) %>% 
  summarise_all(funs(mean,sd,count=n())) # gives average, sd, and count
tally.b

# Ordering  by targeted values ---
target_order <- c("mod1*", "mod2*", "mod3*","mod12","mod13","mod23")
tally.b <- tally.b[match(target_order, tally.b$best_mod),]

tally.b

unique(Explore_Factors$best_mod)


# legend labeling ---
library(stringr)
domain
# all combinations ---
combn <- c(10,20,30,12,13,23)
rm(cases)
cases <- c(sens1,sens2,sens3)
cases
# assiging the legend labels ---
rm(sens10.a,sens20.a,sens30.a,sens12.a,sens13.a,sens23.a)
rm(sens10.p,sens20.p,sens30.p,sens12.p,sens13.p,sens23.p)
if (domain == "d01") { 
  for (i in 1:length(combn)) {
  #i = 1
  tally.b[i,2:4] <- sprintf(c("%5.1f","%5.1f","%6.f"), tally.b[i,2:4])  # paste0("%" ..) make it right aligned
  mod.no1 <- as.integer(substr(combn[i], 1,1));mod.no1
  mod.no2 <- as.integer(substr(combn[i], 2,2));mod.no2
  sens.name <- paste0("sens", combn[i], ".a")
  if (mod.no2 == 0) { 
    assign(sens.name, sprintf("%-40s", substr(cases[mod.no1], 1,nchar(cases[mod.no1])-4)))  
    # creating plot legend labels ---
    sens.Pname <- paste0("sens", combn[i], ".p")
    assign(sens.Pname, paste(get(sens.name), tally.b[i,2],tally.b[i,3],tally.b[i,4],"*",sep = " "))
  } else  {   
    assign(sens.name, sprintf("%-40s", paste(substr(cases[mod.no1], 11,nchar(cases[mod.no1])-4), substr(cases[mod.no2], 11,nchar(cases[mod.no2])-4), sep = " || ")))
    sens.Pname <- paste0("sens", combn[i], ".p")
    assign(sens.Pname, paste(get(sens.name), tally.b[i,2],tally.b[i,3],tally.b[i,4],sep = " "))   }
  
  rm(mod.no1,mod.no2,sens.name,sens.Pname)
  } # i-loop

} else {
  for (i in 1:length(combn)) {
    tally.b[i,2:4] <- sprintf(c("%5.1f","%5.1f","%6.f"), tally.b[i,2:4])  # paste0("%" ..) make it right aligned
    mod.no1 <- as.integer(substr(combn[i], 1,1));mod.no1
    mod.no2 <- as.integer(substr(combn[i], 2,2));mod.no2
    sens.name <- paste0("sens", combn[i], ".a")
    if (mod.no2 == 0) { 
      if (sens.name == "sens10.a" ) {
      assign(sens.name, sprintf("%-41s", substr(cases[mod.no1], 1,nchar(cases[mod.no1])-4)))  
      } else {
      assign(sens.name, sprintf("%-44s", substr(cases[mod.no1], 1,nchar(cases[mod.no1])-4)))  
      }
      
      # creating plot legend labels ---
      sens.Pname <- paste0("sens", combn[i], ".p")
      assign(sens.Pname, paste(get(sens.name), tally.b[i,2],tally.b[i,3],tally.b[i,4],"*",sep = " "))
    } else  {  
      if (sens.name == "sens23.a" ) {
        assign(sens.name, sprintf("%-43s", paste(substr(cases[mod.no1], 11,nchar(cases[mod.no1])-4), substr(cases[mod.no2], 11,nchar(cases[mod.no2])-4), sep = " || ")))
      } else {
        assign(sens.name, sprintf("%-39s", paste(substr(cases[mod.no1], 11,nchar(cases[mod.no1])-4), substr(cases[mod.no2], 11,nchar(cases[mod.no2])-4), sep = " || ")))
      }
      sens.Pname <- paste0("sens", combn[i], ".p")
      assign(sens.Pname, paste(get(sens.name), tally.b[i,2],tally.b[i,3],tally.b[i,4],sep = " "))   }
    
    rm(mod.no1,mod.no2,sens.name,sens.Pname)
    
  } # i-loop
} #d01 check


list=ls(pattern = "sens");list
sens30.p
sens12.a
sens12.p


#For plotting -------------
library(ggplot2)
dev.size("cm")

#  #resizing function
resize.win <- function(Width=6, Height=6) {
  # works for windows
  #dev.off()
  #dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height) 
}


#constrain to the Midwest and Northeast region for plotting ---
if (domain == "d01") {
  # CONUS ---
  domain.p <- "12 km"
  pt.size = 2
  yoff <- 0.2
  paperH <- 18.5
  lonmin = -125.0
  lonmax = -67.0
  latmin = 25.0
  latmax = 55.0
  resize.win(32.70250, 24.28875)
  par(mfrow = c(1,1))
  xminD = -79.5
  xmaxD = -64.5
  yminD = 23
  ymaxD = 32
  } else if (domain == "d02") {
# Northeast ---
domain.p <- "4 km" 
pt.size = 3
yoff <- 0.2 
paperH <- 20
lonmin = -100.0
lonmax = -70.0 #67.0
latmin = 34.0
latmax = 50.0 
resize.win(14.92250, 11.08604)
par(mfrow = c(1,1)) 
xminD = -77.5 
xmaxD = -69 
yminD = 33    
ymaxD = 38    
} else if (domain == "d03") {
# Lake Michigan ---
domain.p <- "1.33 km"
pt.size = 3.5
yoff <- 0.12
paperH <- 23
lonmin = -93.0
lonmax = -81.0
latmin = 40.0
latmax = 48.0
resize.win(13.94354, 11.58875)
par(mfrow = c(1,1))
xminD = -84.3
xmaxD = -80.5
yminD = 39.5
ymaxD = 42    }


# Plotting dot plot ---
head(Explore_Factors)
dim(Explore_Factors)
sel_period
title.p <- paste(toupper(sel_period), " Best Config (Lowest MAE*) for ", variable, " at the 95% significant level for ", domain.p, " grid",sep = "");title.p
# setting levels  ---
rm(target_order,target_order_label)
target_order <- c("mod1*","mod2*","mod3*","mod12","mod13","mod23")
target_order_label <- c(sens10.p,sens20.p,sens30.p,sens12.p,sens13.p,sens23.p )
target_order_label
Explore_Factors$best_mod = factor(Explore_Factors$best_mod, levels = target_order)

# assign variable specific colors ---
var_col <- c("mod1*" ="red","mod2*" = "blue","mod3*"="green",
             "mod1" ="red", "mod2" = "blue", "mod3"="green",
             "mod12" ="magenta","mod13" = "yellow","mod23"="cyan" )



# PLOT 1: DIURNAL BEST CONFIG ---
rm(p1,boxp,p3)

p1 <- ggplot(data=states) +
  geom_path(data=states,aes(x=long,y=lat,group=group),colour="black",lty = 1, lwd = 1) +  
  borders("state", regions = Michigan, colour="black", lty = 1, lwd = 1) + theme_bw() +
  # borders("county", colour="blue", alpha=0.5) +
  
  # for values at site locations
  #geom_text(data = Explore_Factors,aes(x=lon+0.8, y =lat-0.1,label = station_id),check_overlap = TRUE, size = 2.7 ) +
  ##geom_point(data = Explore_Factors,aes(x=lon, y =lat, fill = factor(first_lowest)), size = pt.size, shape= 21, alpha =1.0) +
  
  #geom_point(data = subset (Explore_Factors,best_mod %in% c("mod1*","mod2*","mod3*")),aes(x=lon, y =lat, fill = factor(best_mod)), size = pt.size, shape= 21, alpha =1.0) +
  geom_point(data = Explore_Factors,aes(x=lon, y =lat, fill = factor(best_mod)), size = pt.size, shape= 21, alpha =1.0) +
  
  scale_fill_manual(bquote(paste("WRF CONFIG CASE:                 MAE"["mean, sd "]*"       n")),
                     values = var_col,
                     breaks = target_order,
                     labels = target_order_label,
                     guide = guide_legend(direction = "vertical",
                                          title.position = "top",
                                          label.position = "right",
                                          label.hjust = 0.0,label.vjust = 0.5,
                                          nrow = 6, bycol = FALSE,
                                          override.aes = list(size = pt.size)) )+

  #geom_point(data = Explore_Factors,aes(x=lon, y =lat, fill = factor(second_lowest)), size = pt.size, shape= 21, alpha =1.0,show.legend = FALSE) +
  #scale_fill_manual(values = var_col) +

  scale_x_continuous(limits = c(lonmin, lonmax)) +
  scale_y_continuous(limits = c(latmin, latmax)) +
  
  labs(x = bquote(paste("Longitude, "^{o}*"W")), y =bquote(paste("Latitude, "^{o}*"N"))) + ggtitle(title.p) +
  theme(plot.title = element_text(size = 12,face = "bold")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 12, face = "bold")) +
  theme(strip.text.x = element_text(size = 0.5)) +
  theme(strip.text.y = element_text(size = 0.5)) +
  theme(axis.title.y=element_text(margin=margin(r = 12))) +
  theme(axis.title.x=element_text(margin=margin(t = 16))) +
  theme(panel.background = element_rect(colour = "black", size = 0.5)) +
  theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black")) +
  #putting the legend outside the plot area ---
  # theme(legend.position = "right") +
  # theme(legend.position = "right",legend.justification = "top") +
  
  #putting the legend inside the plot area ---
  # domain 3 ---
  # theme(legend.position = c(0.75,0.87),legend.key.size= unit(0.6, "cm")) +
  # theme(legend.title=element_text(size = 10,face="bold"),legend.text=element_text( size = 10)) 
  # domain 2 ---
  theme(legend.position = c(0.8,0.87),legend.key.size= unit(0.5, "cm")) +
  theme(legend.title=element_text(size = 10,face="bold"),legend.text=element_text( size = 8)) 


# PLOT2: Embedded Box-plot for 3 cases ---
library(EnvStats)
head(Explore_Factors)

rm(tally.mod1,tally.mod2,tally.mod3 )

tally.mod1 <- Explore_Factors %>% 
  dplyr::select(mod1_mae) %>% 
  summarise_all(funs(mean,sd,count=n())) # gives average, sd, and count

tally.mod2 <- Explore_Factors %>% 
  dplyr::select(mod2_mae) %>% 
  summarise_all(funs(mean,sd,count=n())) # gives average, sd, and count

tally.mod3 <- Explore_Factors %>% 
  dplyr::select(mod3_mae) %>% 
  summarise_all(funs(mean,sd,count=n())) # gives average, sd, and count

# formatting ---
tally.mod1[1,1:3] <- sprintf(c("%5.1f","%5.1f","%6.f"), c(tally.mod1[1,1:3]))  
tally.mod2[1,1:3] <- sprintf(c("%5.1f","%5.1f","%6.f"), c(tally.mod2[1,1:3]))  
tally.mod3[1,1:3] <- sprintf(c("%5.1f","%5.1f","%6.f"), c(tally.mod3[1,1:3]))  


head(Explore_Factors)
library(reshape2)
rm(Explore_Factors.melt)
Explore_Factors.melt <- reshape2::melt(Explore_Factors, id.vars = "site",
                          measure.vars = c("mod1_mae", "mod2_mae", "mod3_mae"),
                          variable.name = "CONFIG",na.rm = TRUE)
head(Explore_Factors.melt)
# setting levels  ---
unique(Explore_Factors.melt$CONFIG)
Explore_Factors.melt$CONFIG = factor(Explore_Factors.melt$CONFIG, levels = c("mod1_mae","mod2_mae","mod3_mae"))

# boxplot ---
user.col   <- c("mod1_mae" ="red","mod2_mae"="blue","mod3_mae" = "green")

# evoke graphic device for boxplot---
variable
if (variable == "temp2m") {
   unit <- "K"
   user.lim <- 5
   user.lim.intv <- 1 
} else if (variable == "wnddir10m") {
   unit <- "deg"
   user.lim <- 100
   user.lim.intv <- 10 
} else if (variable == "wndspd10m") {
  unit <- "m/s"
  user.lim <- 3
  user.lim.intv <- 1 
} else if (variable == "mixr2m") {
   unit <- "g/kg"
   user.lim <- 3.0
   user.lim.intv <- 0.5 }


boxp <- ggplot(Explore_Factors.melt, aes(x=CONFIG, y=value)) +
  geom_jitter(aes(group =CONFIG, color=CONFIG),size = 0.5,width = 0.1,show.legend = FALSE)+
  scale_color_manual(values = user.col)+
  geom_boxplot(aes(fill=CONFIG),na.rm = TRUE,alpha = 0.5,outlier.shape = NA,show.legend = FALSE)+
  scale_fill_manual(values= user.col) +
  labs(x="",y=paste("MAE,", unit,sep = " "))+
  theme_bw() + 
  theme(plot.title = element_text(size = 10,face = "bold")) +
  theme(axis.title = element_text(size = 8)) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 0.5)) +
  theme(strip.text.y = element_text(size = 0.5)) +
  theme(axis.title.y=element_text(margin=margin(r = 6))) +
  theme(axis.title.x=element_text(margin=margin(t = 6))) +
  theme(panel.background = element_rect(colour = "black", size = 0.5)) +
  theme(legend.background = element_rect(size=0.6, linetype="solid", 
                                         colour ="black")) +
  # putting the legend inside the plot area
  #theme(legend.position = c(0.9,0.87),legend.key.size= unit(0.6, "cm")) +
  #theme(legend.title=element_text(size = 12,face="bold"),legend.text=element_text(size=10)) +
  scale_y_continuous(limits= c(0,user.lim),breaks = seq(0,user.lim,user.lim.intv))  +
  scale_x_discrete(labels=c("mod1_mae" = paste0("APLX_NAM","\n\n","avg =",tally.mod1[1,1],"\n","sd =",tally.mod1[1,2],"\n","n =",tally.mod1[1,3]),
                            "mod2_mae" = paste0("YNT_GFS","\n\n","avg =",tally.mod2[1,1],"\n","sd =",tally.mod2[1,2],"\n","n =",tally.mod2[1,3]),
                            "mod3_mae" = paste0("YNT_NAM","\n\n","avg =",tally.mod3[1,1],"\n","sd =",tally.mod3[1,2],"\n","n =",tally.mod3[1,3])) )


# Plot inside a ggplot ---
library(ggplot2)
# Final ploting p1 and boxplot to graphic device ---
p1 + annotation_custom(ggplotGrob(boxp), xmin = xminD, xmax = xmaxD, 
                       ymin = yminD, ymax = ymaxD)

# saving the plot as *.png ---
outdir = getwd();outdir
sel_period
domain  
variable
period

outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MAE_LADCO_sens1-3_Best_",variable,"_",domain,"_",period,".png",sep = ""));outfile
# for cookie cut only ---
#outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MAE_LADCO_sens1-3_Best_",variable,"_d04cut_",period,".png",sep = ""));outfile

ggsave(outfile,plot = last_plot(), 
       height = paperH, width = 25, units = "cm")
# closing the graphic dev ---
dev.off()

} # go back to next loop for du_idx ---



#-------------------------------------------------------------------------------------------------
# Conc1: LADCO2016_YNT_NAM_gsda (sens3) seemed the Best config based on total number of sites in the domain.  
# So, plot MAE Magnitude and BIAS at sites for the LADCO2016_YNT_NAM_gsda Config.
#-------------------------------------------------------------------------------------------------


# PLOT 3: MAE MAGNITUDE by dot size and BIAS by dot color ---
domain <- "d01"  
variable <- "mixr2m" #"mixr2m" #"wndspd10m" #"wnddir10m" #"temp2m"
period <- "20160610-20160619"
du_period <- c("sunrise","daytime","sunset","nighttime")
# du_idx = 3
# sel_period <- du_period[du_idx] ;sel_period

# looping through du_idx ---
for (du_idx in c(1, 2, 3, 4) ) {
sel_period <- du_period[du_idx] ;sel_period

outdir = getwd();outdir
rm(infile)
infile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MPS_LADCO_sens1-3_",variable,"_",domain,"_", period,".csv",sep = ""));outfile
rm(dat.best)
dat.best <- read.csv(file=infile,skip=0, header = TRUE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                     colClasses = c("character",rep ("numeric",90)))

dim(dat.best)
head(dat.best)

# include a fake data to have consistent color gradient for data missing in a bin ---

colnames(dat.best)
summary(dat.best)

# for plotting purpose ---
if (domain == "d01") { 
  sens1 <- paste("USEPA2016_APLX_NAM_gda_",domain,sep = "");sens1
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
  
} else if (domain == "d02") { 
  sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
  
} else if (domain == "d03") { 
  sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
  
}

# for MAE and BIAS plotting purpose ---
rm(sens.array)
sens.array <- data.frame(X = c(1,2,3), case = c(sens1,sens2,sens3),stringsAsFactors = FALSE)

# subsetting data for the BEST config (sens3 or 3rd mod is the best) ---
# loop-X ---
X <- 1
for (X in c(1 , 2, 3) ) {
sens.array[X,]
sens.array[X,2]
rm(mae.X,bias.X)
mae.X <- paste0("mae.",X,"_mean");mae.X
bias.X <- paste0("bias.",X,"_mean");bias.X
rm(sel.col.idx)
sel.col.idx <- which(colnames(dat.best) %in% c("station_id", "lat_mean","lon_mean", mae.X, bias.X))
sel.col.idx
dat.best[1,sel.col.idx]


#op1 selecting columns ---
rm(dat.best1)
dat.best1 <- subset(dat.best, select = sel.col.idx)
head(dat.best1)
#op2 selecting columns ---
#dat.best1 <- subset(dat.best, select = c("station_id", "lat_mean","lon_mean", mae.X, bias.X))

sel.col.idx

variable
if (variable == "temp2m") {
  unit <- "K"
  idx <- which(dat.best1[,4] > 5.0);idx # > 4th column is MAE; 5 K error is useless
  dat.best1[idx,]
  dat.best1[idx,] <- NA
} else if (variable == "wnddir10m") {
  unit <- "deg"  
  idx <- which(dat.best1[,4] > 70.0);idx # > 60 deg error is useless
  dat.best1[idx,]
  dat.best1[idx,] <- NA 
} else if (variable == "wndspd10m" ) {
  unit <- "m/s"  
  idx <- which(dat.best1[,4] > 5.0);idx # > 5.0 m/s error is useless
  dat.best1[idx,]
  dat.best1[idx,] <- NA 
} else if (variable == "mixr2m") {
  unit <- "g/kg"  
  idx <- which(dat.best1[,4] > 3.0);idx # > 3.0 g/kg error is useless
  dat.best1[idx,]
  dat.best1[idx,] <- NA }


# removing NA rows ---
rm(dat.best11)
dat.best11 <- dat.best1[complete.cases(dat.best1),]  # writing complete cases to new df
head(dat.best11)
summary(dat.best11)


# setting dot size bins ---
rm(max.val,size.breaks,size.breaks, labels,fakebreaks,fakelength,minB,maxB,intB,labels.p)
dat.best11[1,]
max.val <- ceiling(max(dat.best11[,4],na.rm = T));max.val

if (variable %in% c("temp2m","wndspd10m")) {
  size.breaks <- c(seq(0, 3.5, by =0.5),max.val);size.breaks;length(size.breaks)
  labels      <- c("0.0 - 0.5",   "0.6 - 1.0",      "1.1 - 1.5",  "1.6 - 2.0",   "2.1 - 2.5",  "2.6 - 3.0",  "3.1 - 3.5",paste0("3.6 - ",sprintf("%2.1f",max.val)));length(labels)
  # inserting a fake data into df in order to control dot size for universal use --- 
  fakebreaks <- seq(0.25, max.val, by =0.5);fakebreaks
  fakelength <- length(fakebreaks);fakelength
  rm(fake)
  fake <-  dat.best11[1:fakelength,];fake
  fake[,1] <- "ZZZZZ"
  fake[,2] <- 60.0
  fake[,3] <- -60.0
  fake[,4] <- fakebreaks 
  fake[,5] <- 0
  fake[1,5] <- 4.0 
  fake[fakelength,5] <- -4.0 
  dat.best11 <- rbind(dat.best11,fake) 
  maxB <- 4.0
  minB <- -4.0
  intB <- 1.0
  labels.p <- sprintf(c(rep("%2.1f",length(seq(minB,maxB,by = intB)))),seq(minB,maxB,by = intB))
  barh.var <-  9

} else if (variable == "wnddir10m") {
  size.breaks <- c(seq(0, 50, by =10),max.val);size.breaks;length(size.breaks)
  labels      <- c("  0 - 10", "11 - 20",  "21 - 30",  "31 - 40",   "41 - 50",  paste0("51 - ",sprintf("%2.0f",max.val)));length(labels)
  # inserting a fake data into df in order to control dot size for universal use --- 
  fakebreaks <- seq(5, 40, by =10);fakebreaks
  fakelength <- length(fakebreaks);fakelength
  rm(fake)
  fake <-  dat.best11[1:fakelength,];fake
  fake[,1] <- "ZZZZZ"
  fake[,2] <- 60.0
  fake[,3] <- -60.0
  fake[,4] <- fakebreaks 
  fake[,5] <- 0
  fake[1,5] <- 40.0 
  fake[fakelength,5] <- -40.0 
  dat.best11 <- rbind(dat.best11,fake) 
  maxB <- 50.0
  minB <- -50.0
  intB <- 10.0
  labels.p <- sprintf(c(rep("%3.0f",length(seq(minB,maxB,by = intB)))),seq(minB,maxB,by = intB))
  barh.var <-  10
  
} else if (variable == "mixr2m") {
  size.breaks <- c(seq(0, 2.5, by =0.5),max.val);size.breaks;length(size.breaks)
  labels      <- c("0.0 - 0.5", "0.6 - 1.0",  "1.1 - 1.5",  "1.5 - 2.0",   "2.1 - 2.5",  paste0("2.6 - ",sprintf("%2.1f",max.val)));length(labels)
  # inserting a fake data into df in order to control dot size for universal use --- 
  fakebreaks <- seq(0.25, 3.0, by =0.5);fakebreaks
  fakelength <- length(fakebreaks);fakelength
  rm(fake)
  fake <-  dat.best11[1:fakelength,];fake
  fake[,1] <- "ZZZZZ"
  fake[,2] <- 60.0
  fake[,3] <- -60.0
  fake[,4] <- fakebreaks 
  fake[,5] <- NA
  fake[1,5] <- 3.0 
  fake[fakelength,5] <- -3.0 
  dat.best11 <- rbind(dat.best11,fake) 
  maxB <- 3.0
  minB <- -3.0
  intB <- 0.5
  labels.p <- sprintf(c(rep("%3.1f",length(seq(minB,maxB,by = intB)))),seq(minB,maxB,by = intB))
  barh.var <-  10
}  

labels
labels.p
tail(dat.best11)

# creating  sizebin ---
size.breaks
dat.best11$sizebin <- cut(dat.best11[,4],breaks = size.breaks, labels = NULL,include.lowest = TRUE, right = TRUE)  # labels = TRUE will give you bin intervals for plot labeling
unique(dat.best11$sizebin)
# setting level factor ---
#dat.best11$sizebin = factor(dat.best11$sizebin, levels = size.breaks)
summary(dat.best11)

# PLOT3: MAE Magnitute ---Dot plot for MAE and BIAS at sites ---
length(labels)
# constrain to the Midwest and Northeast region for plotting ---
# domain <- "d02"
if (domain == "d01") {
  # Northeast ---
  domain.p <- "12 km"
  pt.size = 2
  yoff <- 0.2
  paperH <- 18.5
  lonmin = -125.0
  lonmax = -67.0
  latmin = 25.0
  latmax = 55.0
  resize.win(32.70250, 24.28875)
  par(mfrow = c(1,1))
  xminD = -79.5
  xmaxD = -64.5
  yminD = 23
  ymaxD = 32
} else if (domain == "d02") {
  # Northeast ---
  domain.p <- "4 km" 
  pt.size = 3
  yoff <- 0.2 
  paperH <- 20
  lonmin = -100.0
  lonmax = -70.0 #67.0
  latmin = 34.0
  latmax = 50.0 
  resize.win(14.92250, 11.08604)
  par(mfrow = c(1,1)) 
  xminD = -77.5 
  xmaxD = -69 
  yminD = 33    
  ymaxD = 38    
} else if (domain == "d03") {
  # Lake Michigan ---
  domain.p <- "1.33 km"
  pt.size = 3.5
  yoff <- 0.12
  paperH <- 23
  lonmin = -93.0
  lonmax = -81.0
  latmin = 40.0
  latmax = 48.0
  resize.win(13.94354, 11.58875)
  dev.cur()
  par(mfrow = c(1,1))
  xminD = -84.3
  xmaxD = -80.5
  yminD = 39.5
  ymaxD = 42    }



sens.array[X,]
title.p <- paste(toupper(sel_period), sens.array[X,2], "Config for",domain.p,"domain:",variable, sep = " ");title.p
require(ggplot2) # is needed for getting desired breaks in BIAS legend using scale_fill_gradient2() function

head(dat.best11)
rm(p3)
p3 <- ggplot(data=states) +
  geom_path(data=states,aes(x=long,y=lat,group=group),colour="black",lty = 1, lwd = 1)+  
  borders("state", regions = Michigan, colour="black", lty = 1, lwd = 1) + theme_bw()+
  # borders("county", colour="blue", alpha=0.5) +
  
  #geom_point(data = dat.best,aes(x=lon_mean, y =lat_mean, fill = bias.3_mean), size = 4, color = "black", shape= 21,alpha = 1.0,show.legend = TRUE) +
  
  geom_point(data = dat.best11,aes(x=lon_mean, y =lat_mean, fill = dat.best11[,5], size = factor(sizebin)), color = "black", shape= 21,alpha = 1.0,show.legend = TRUE) +
  scale_size_manual(paste("MAE,", unit,sep = " "),values = seq(2,length(labels),by = 0.5),labels = labels,
                                      guide = guide_legend(direction = "vertical",
                                                           title.position = "top",
                                                           label.position = "right",
                                                           order = 1,
                                                           label.hjust = 0.0,label.vjust = 0.4)) +
  scale_fill_gradient2(paste("BIAS,", unit,sep = " "),limits=c(minB,maxB),breaks = seq(minB,maxB,by = intB),labels = labels.p,
                       guide = "colourbar", low='blue',mid='white',high='red', midpoint = 0 ) +
                       
  scale_x_continuous(limits = c(lonmin, lonmax)) +
  scale_y_continuous(limits = c(latmin, latmax)) +
  
  labs(x = bquote(paste("Longitude, "^{o}*"W")), y =bquote(paste("Latitude, "^{o}*"N"))) + ggtitle(title.p)+
  theme(plot.title = element_text(size = 16,face = "bold")) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 14, face = "bold")) +
  theme(strip.text.x = element_text(size = 0.5)) +
  theme(strip.text.y = element_text(size = 0.5)) +
  theme(axis.title.y=element_text(margin=margin(r = 12))) +
  theme(axis.title.x=element_text(margin=margin(t = 16))) +
  theme(panel.background = element_rect(colour = "black", size = 0.5)) +
  theme(legend.background = element_rect(size=0.7, linetype="solid", 
                                         colour ="gray")) +
  # putting the legend inside the plot area
  theme(legend.position = c(0.92,0.67),legend.key.size= unit(0.5, "cm")) +
  theme(legend.title=element_text(size = 12,face="bold"),legend.text=element_text(size=9)) 
  p3 + guides (fill = guide_colourbar(barwidth = 1.7, barheight = barh.var, draw.ulim = FALSE, draw.llim = FALSE,order = 2,label.hjust = 1)) 


# saving the plot3 as *.png ---
outdir = getwd();outdir
domain  
variable
period

# removing white space from both ends; trimws(sens30.a,"r"); trimws(sens30.a,"l")
sens.array[X,2]

#outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MAE_BIAS_",sens.array[X,2], "_cut4d02_",variable,"_",period,".png",sep = ""));outfile
outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/LowestMAE_SigTesting/",sel_period,"_MAE_BIAS_",sens.array[X,2], "_",variable,"_",period,".png",sep = ""));outfile
ggsave(outfile,plot = last_plot(), 
       height = paperH, width = 25, units = "cm")
# closing the graphic dev ---
dev.off()

 } # for X loop, for case selection ---

} # for next loop of du_idx  -----

##END of CODE --------------------

