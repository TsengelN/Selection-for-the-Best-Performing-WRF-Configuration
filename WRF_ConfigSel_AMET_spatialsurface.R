################################################################################
# This Code compares different WRF senstitivtiy runs using AMET Spatial Surface statistics and 
# plot spatial plot for a Winner Config.

# By: Tsengel Nergui, Atmospheric Modeler, LADCO
# Update history:
# initial version: 05/17/2019

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
variable <- "wnddir10m" #"wnddir10m" #"temp2m"
period <- "20160610-20160619"

if (domain == "d01") { 
  sens1 <- paste("USEPA2016_APLX_NAM_gda_",domain,sep = "");sens1
  insens1 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens1,"/spatial_surface_LADCOzoom/",sens1,".spatial.",variable,".stats.",period,".csv",sep = "");insens1
  
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  insens2 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens2,"/spatial_surface_LADCOzoom/",sens2,".spatial.",variable,".stats.",period,".csv",sep = "");insens2
  
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
  insens3 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens3,"/spatial_surface_LADCOzoom/",sens3,".spatial.",variable,".stats.",period,".csv",sep = "");insens3
  
} else if (domain == "d02") { 
sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
insens1 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens1,"/spatial_surface_LADCOzoom/",sens1,".spatial.",variable,".stats.",period,".csv",sep = "");insens1

sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
insens2 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens2,"/spatial_surface_LADCOzoom/",sens2,".spatial.",variable,".stats.",period,".csv",sep = "");insens2

sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
insens3 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens3,"/spatial_surface_LADCOzoom/",sens3,".spatial.",variable,".stats.",period,".csv",sep = "");insens3

} else if (domain == "d03") { 
  sens1 <- paste("LADCO2016_APLX_NAM_gda_nd_",domain,sep = "");sens1
  insens1 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens1,"/spatial_surface_ext_d03/",sens1,".spatial.",variable,".stats.",period,".csv",sep = "");insens1
  
  sens2 <- paste("LADCO2016_YNT_GFS_gsda_",domain,sep = "");sens2
  insens2 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens2,"/spatial_surface_ext_d03/",sens2,".spatial.",variable,".stats.",period,".csv",sep = "");insens2
  
  sens3 <- paste("LADCO2016_YNT_NAM_gsda_",domain,sep = "");sens3
  insens3 <- paste("C:/Users/nargu/My R workplace/MET_evaluation/AWS/Jun10-19_2016/",sens3,"/spatial_surface_ext_d03/",sens3,".spatial.",variable,".stats.",period,".csv",sep = "");insens3

}

rm(filenames)
filenames <- c(insens1,insens2,insens3);filenames

# reading site-specific statisticts files and adding header ---

rm(dat.sens1,dat.sens2,dat.sens3)
dat.sens1 <- read.csv(file=insens1,skip=1, header = FALSE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                     colClasses = c("character",rep ("numeric",2),"integer",rep ("numeric",16)))
colnames(dat.sens1) <- c ("station_id","lat","lon","count","corr","ac","var","sdev","rmse","mae","bias","mfbias","mnbias","mngerr","nmbias","nmerr","max","min",
                         "mod_mean","obs_mean")  # last two column headers are edu guesses becasue no header was written out from AMET. NT
# NA rows been removed ---
dat.sens1[complete.cases(dat.sens1),] -> dat.sens11  # writing complete cases to new df
head(dat.sens1)
summary(dat.sens11)

dat.sens2 <- read.csv(file=insens2,skip=1, header = FALSE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                      colClasses = c("character",rep ("numeric",2),"integer",rep ("numeric",16)))
colnames(dat.sens2) <- c ("station_id","lat","lon","count","corr","ac","var","sdev","rmse","mae","bias","mfbias","mnbias","mngerr","nmbias","nmerr","max","min",
                          "mod_mean","obs_mean")  # last two column headers are edu guesses becasue no header was written out from AMET. NT
# NA rows been removed ---
dat.sens2[complete.cases(dat.sens2),] -> dat.sens22  # writing complete cases to new df
summary(dat.sens22)

dat.sens3 <- read.csv(file=insens3,skip=1, header = FALSE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                      colClasses = c("character",rep ("numeric",2),"integer",rep ("numeric",16)))
colnames(dat.sens3) <- c ("station_id","lat","lon","count","corr","ac","var","sdev","rmse","mae","bias","mfbias","mnbias","mngerr","nmbias","nmerr","max","min",
                          "mod_mean","obs_mean")  # last two column headers are edu guesses becasue no header was written out from AMET. NT

# NA rows been removed ---
dat.sens3[complete.cases(dat.sens3),] -> dat.sens33  # writing complete cases to new df
summary(dat.sens33)


# creating sitelist ---
tmp_all <- rbind(dat.sens11,dat.sens22,dat.sens33)
tmp_all$site_loc <- paste(tmp_all$station_id,tmp_all$lat,tmp_all$lon, sep = "_")
siteloclist <- as.data.frame(unique(tmp_all$site_loc))
idx <- which(tmp_all$site_loc == siteloclist);idx
rm(site_dat, sitelist)
site_dat <- tmp_all[idx,1:3]
head(site_dat)
sitelist <- unique(tmp_all$station_id);length(sitelist)

# Explore_Factors ---
library(fitdistrplus)
rm(Explore_Factors)
Explore_Factors <- data.frame(station_id=character(),
                              lat=numeric(), 
                              lon=numeric(),
                              mae_sens1=numeric(), 
                              mae_sens2=numeric(),
                              mae_sens3=numeric(),
                              lowest_mae1 = character(),
                              lowest_mae_col1 = numeric(),
                              lowest_mae2 = character(),
                              lowest_mae_col2 = numeric(),
                              mae2_mae1 = numeric(),
                              best_mae = character(),
                              stringsAsFactors = FALSE)


dim(sitelist)

if (variable == "temp2m") { cutoff <- 0.1 }
if (variable == "wnddir10m") { cutoff <- 2.0 }



for (i in 1:length(sitelist)) {
  sel.site <- sitelist[i]
  idx <- which(site_dat$station_id == sel.site);idx
  
  tmp1 <- dat.sens11 %>% dplyr::filter(dat.sens11$station_id == sel.site) %>% dplyr::select(station_id, mae)
  tmp2 <- dat.sens22 %>% dplyr::filter(dat.sens22$station_id == sel.site) %>% dplyr::select(station_id, mae)
  tmp3 <- dat.sens33 %>% dplyr::filter(dat.sens33$station_id == sel.site) %>% dplyr::select(station_id, mae)
  #print(paste(tmp1[1,1],tmp1[1,2],tmp2[1,2],tmp3[1,2],sep= " "))
  
  Explore_Factors[i,1] <- sel.site
  Explore_Factors[i,2] <- site_dat[idx,2]  #lat
  Explore_Factors[i,3] <- site_dat[idx,3]  #lon
  
  if (! dim(tmp1)[1] == 0) { Explore_Factors[i,4] <- tmp1[1,2] }
  else                     { Explore_Factors[i,4] <- NA } #if-condition
  if (! dim(tmp2)[1] == 0) { Explore_Factors[i,5] <- tmp2[1,2] }
  else                     { Explore_Factors[i,5] <- NA } #if-condition
  if (! dim(tmp3)[1] == 0) { Explore_Factors[i,6] <- tmp3[1,2] }
  else                     { Explore_Factors[i,6] <- NA } #if-condition
  
  temp1 <- Explore_Factors[i,4:6]
  nth1 <- which.min(apply(temp1, MARGIN = 2, min));nth1  # MARGIN = 2 is for getting column name
  name1 <- names(nth1)
  Explore_Factors[i,7] <- unlist(strsplit(name1, "[_]"))[2]
  Explore_Factors[i,8] <- Explore_Factors[i, 3 +nth1]
  # temporary assinging 999 for getting the second min
  temp2 <- temp1
  temp2[,nth1] <- 999.9
  nth2 <- which.min(apply(temp2, MARGIN = 2, min))
  name2 <- names(nth2)
  Explore_Factors[i,9] <- unlist(strsplit(name2, "[_]"))[2]
  #Explore_Factors[i,10] <- 3+nth2
  Explore_Factors[i,10] <- Explore_Factors[i, 3 +nth2]
  Explore_Factors[i,11] <- Explore_Factors[i, 10] - Explore_Factors[i, 8]
  if (Explore_Factors[i,11] >= cutoff) { Explore_Factors[i,12] <- Explore_Factors[i,7]  }
  else                     { 
    #Explore_Factors[i,12] <- paste(Explore_Factors[i,7],"/",Explore_Factors[i,9],sep="")  
    Explore_Factors[i,12] <- NA      } #if-condition
  
  rm(sel.site, idx, tmp1,tmp2,tmp3, nth1,nth2,name1,name2,temp1,temp2)
  
} # end of sitelist loop

head(Explore_Factors)


outdir = getwd();outdir
domain  
variable
period

outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/MAE_LADCO_sens1-3_Best_",cutoff,"diff_",variable,"_",period,"_",domain,".csv",sep = ""));outfile
write.table (Explore_Factors,outfile,row.names = F,col.names = T,sep = ",", quote = FALSE)

domain

#-------------------------------------------------------------
# PART 2: FINAL X-Y plots for the Best Config (Lowest MAE) Selection    ---
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
#indir <- "C:/Users/nargu/My R workplace/MET_evaluation"
domain <- "d03"  
variable <- "temp2m" #"temp2m" #"wnddir10m" 
period <- "20160610-20160619"
if (variable == "temp2m") { cutoff <- 0.1 }
if (variable == "wnddir10m") { cutoff <- 2.0 }


rm(Explore_Factors)
infile <- (paste(indir,"/summary_LADCO_sens_AWS/MAE_LADCO_sens1-3_Best_",cutoff,"diff_",variable,"_",period,"_",domain,".csv",sep = ""));infile

Explore_Factors <- read.csv(file=infile,skip=0, header = TRUE, quote = "\"", sep =",", na.strings = "NA", stringsAsFactors = FALSE,
                            colClasses = c("character",rep ("numeric",5),"character","numeric","character","numeric","character"))

head(Explore_Factors)

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

rm(tally.1,tall.b)
tally.1 <- Explore_Factors %>% 
  dplyr::select(lowest_mae1,lowest_mae_col1) %>% 
  group_by(lowest_mae1) %>% 
  summarise_all(funs(mean,sd,count=n())) # gives average, sd, and count
tally.1

tally.b <- Explore_Factors %>%
  group_by(best_mae) %>%
  summarise(count=n()) 
tally.b
  
tally.1[1,3]
# legend labeling ---
library(stringr)
maxlen.c <- max(nchar(sens1),nchar(sens2), nchar(sens3))+2;maxlen.c

# cutting off the domain info from sens variable
substr(sens1, 1,nchar(sens1)-4)

# sens1.a <- sprintf(paste0("%-",maxlen.c,"s"), substr(sens1, 1,nchar(sens1)-4)) # paste0("%-" ..) make it left justified
# sens2.a <- sprintf(paste0("%-",maxlen.c,"s"), substr(sens2, 1,nchar(sens2)-4))
# sens3.a <- sprintf(paste0("%-",maxlen.c,"s"), substr(sens3, 1,nchar(sens3)-4))

if (domain != "d01") { 
sens1.a <- sprintf("%-29s", substr(sens1, 1,nchar(sens1)-4))
sens2.a <- sprintf("%-32s", substr(sens2, 1,nchar(sens2)-4))
sens3.a <- sprintf("%-32s", substr(sens3, 1,nchar(sens3)-4))
} else {
  sens1.a <- sprintf("%-32s", substr(sens1, 1,nchar(sens1)-4))
  sens2.a <- sprintf("%-32s", substr(sens2, 1,nchar(sens2)-4))
  sens3.a <- sprintf("%-32s", substr(sens3, 1,nchar(sens3)-4))
}

tally.1[1,2] <- sprintf("%5.1f", tally.1[1,2])  # paste0("%" ..) make ir right aligned
tally.1[2,2] <- sprintf("%5.1f", tally.1[2,2])
tally.1[3,2] <- sprintf("%5.1f", tally.1[3,2])

tally.1[1,3] <- sprintf("%5.1f", tally.1[1,3])  # paste0("%" ..) make ir right aligned
tally.1[2,3] <- sprintf("%5.1f", tally.1[2,3])
tally.1[3,3] <- sprintf("%5.1f", tally.1[3,3])

tally.1[1,4] <- sprintf("%6.f", tally.1[1,4])  # paste0("%" ..) make ir right aligned
tally.1[2,4] <- sprintf("%6.f", tally.1[2,4])
tally.1[3,4] <- sprintf("%6.f", tally.1[3,4])
tally.1

tally.b[1,2] <- sprintf("%6.f", tally.b[1,2])  
tally.b[2,2] <- sprintf("%6.f", tally.b[2,2])
tally.b[3,2] <- sprintf("%6.f", tally.b[3,2])
tally.b

rm(sens1.p,sens2.p,sens3.p)
sens1.p <- paste(sens1.a, tally.1[1,2],tally.1[1,3],tally.1[1,4],tally.b[1,2],sep = " ");sens1.p
sens2.p <- paste(sens2.a, tally.1[2,2],tally.1[2,3],tally.1[2,4],tally.b[2,2],sep = " ");sens2.p
sens3.p <- paste(sens3.a, tally.1[3,2],tally.1[3,3],tally.1[3,4],tally.b[3,2],sep = " ");sens3.p


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


# constrain to the Midwest and Northeast region for plotting ---
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
par(mfrow = c(1,1)) } 



# Plotting ---

head(Explore_Factors)

title.p <- paste("The Best Config (Lowest MAE) for ", variable, " at ", domain.p, " grid", sep = "");title.p
ggplot(data=states) +
  geom_path(data=states,aes(x=long,y=lat,group=group),colour="black",lty = 1, lwd = 1)+  
  borders("state", regions = Michigan, colour="black", lty = 1, lwd = 1) + theme_bw()+
  # borders("county", colour="blue", alpha=0.5) +
  
  # for values at site locations
  #geom_text(data = Explore_Factors,aes(x=lon+0.8, y =lat-0.1,label = station_id),check_overlap = TRUE, size = 2.7 ) +
  geom_point(data = Explore_Factors,aes(x=lon, y =lat, fill = factor(lowest_mae1)), size = pt.size, shape= 21, alpha =1.0) +
  #geom_point(data = Explore_Factors,aes(x=Explore_Factors$lon, y =Explore_Factors$lat, fill = factor(Explore_Factors$best_mae)), size = 3, shape= 21, alpha =1.0) +
  
  scale_fill_manual(bquote(paste("WRF CONFIG CASE:                      MAE"["mean, sd"]*"   n"[1]*"    n"[2]*"")), values = c("red","blue","green"),
                    breaks = c("sens1","sens2","sens3"),
                    labels = c(sens1.p,sens2.p,sens3.p),
                    guide = guide_legend(direction = "vertical",
                                         title.position = "top",
                                         label.position = "right",
                                         label.hjust = 0.0,label.vjust = 0.4,
                                         nrow = 3, byrow = TRUE,
                                         override.aes = list(size = pt.size)) )+
  
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
  theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black")) +
  # putting the legend inside the plot area
  theme(legend.position = c(0.77,0.9),legend.key.size= unit(.6, "cm")) +
  theme(legend.title=element_text(size = 10,face="bold"),legend.text=element_text(size=9)) 


# saving the plot as *.png ---
outdir = getwd();outdir
domain  
variable
period

outfile <- (paste(outdir,"/summary_LADCO_sens_AWS/MAE_LADCO_sens1-3_Best_",variable,"_",period,"_",domain,".png",sep = ""));outfile
ggsave(outfile,plot = last_plot(), 
       height = paperH, width = 25, units = "cm")
# closing the graphic dev ---
dev.off()




