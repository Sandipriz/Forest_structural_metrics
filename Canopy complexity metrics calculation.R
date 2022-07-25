

############### Packages ####################################################
library(neonUtilities)
library(lidR)
library(gstat)
library(rgdal)
library(rgeos)
library(data.table)
library(ggplot2)
library(tidyr)



########## Shapefile data
# load shapefile of Bartlett plot polygons
stands<-readOGR("MEL_spatial/30x30_subplots","Bartlett_intensive_sites_30x30")
stands<-spTransform(stands,CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs+ellps=WGS84 +towgs84=0,0,0")) # To convert it to WGS8
stands@data

plot(stands)

# centroids are the 'plot centers'. This script works with point data.
centroids <- as.data.frame(getSpPPolygonsLabptSlots(stands))
rownames(centroids)<-stands$unique_plo
centroids$Stand<-as.factor(stands$stand)
centroids$Plot<-as.factor(stands$plot)

# this line downloads the Aop data
easting<-centroids$V1
northing<-centroids$V2

# load Neon tiles
#byTileAOP("DP1.30003.001", site="BART", year="2019", check.size = F,buffer = 200, easting=easting, northing=northing, savepath="neon_data")


# read in LAz files

# 2014
f14<-list.files(path="neon_data\\DP1.30003.001\\neon-aop-products\\2014\\FullSite\\D01\\2014_BART_1\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)
f14
# 2016
f16<-list.files(path="neon_data\\DP1.30003.001\\neon-aop-products\\2016\\FullSite\\D01\\2016_BART_2\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)
# 2017
f17<-list.files(path="neon_data\\DP1.30003.001\\neon-aop-products\\2017\\FullSite\\D01\\2017_BART_3\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)
# 2018
f18 <- list.files(path="neon_data\\DP1.30003.001\\neon-aop-products\\2018\\FullSite\\D01\\2018_BART_4\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)
# 2019
f19 <- list.files(path="neon_data\\DP1.30003.001\\neon-aop-products\\2019\\FullSite\\D01\\2019_BART_5\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)
f19
###########

#3 Put in the year
laz<-readLAS(f14)

######################################################################
  # the Loop
######################################################################
plot.metrics<-list()
plot.metrics


# For 2016 and 50
for(i in c(1:36)){    # the loop only goes for the first 4 rows because the first 4 use the C1laz. rows 5-8 need C2laz. 9-12 need C3laz.
  center<-centroids[i,]
  
  use.laz<-laz # this is where I set the loop to use the laz file 

  chm <- grid_canopy(laz_data, res = 1, dsmtin()) 
  chm@data@values
  
  
  plot.area<-15
  
  # You shouldn't have to change anything below.
  # select the xy location of the plot we are calculating canopy metrics for
  x<-as.numeric(center[1])
  y<-as.numeric(center[2])
  
  #Cut out a 200 x 200 m buffer by adding 100 m to easting and northing coordinates (x,y).
  data.cut <- lasclipRectangle( use.laz , xleft = (as.numeric(x - 100)), ybottom = (as.numeric(y - 100)),xright = (as.numeric(x + 100)), ytop = (as.numeric(y + 100)))
  #Correct for ground height using a kriging function to interpolate elevation from ground points in the .laz file.
  #If the function will not run, then you may need to checkfor outliers by adjusting the 'drop_z_' arguments when reading in the .laz files.
  dtm <- grid_terrain(data.cut, 1, kriging(k = 10L))
  data.200m<- lasnormalize(data.cut, dtm)
  #plot(data.200m)
  data.30m <- lasclipRectangle(data.200m, xleft = (x - plot.area), ybottom = (y - plot.area), xright = (x + plot.area), ytop = (y + plot.area))
  data.30m@data$Z[data.30m@data$Z <= 4] <- NA  
  data.30m@data$Z[data.30m@data$Z >= 35] <- NA  
  laz_data<-data.30m
#  plot(laz_data)
  structural_diversity_metrics <- function(laz_data) {
    chm <- grid_canopy(laz_data, res = 1, dsmtin()) 
    mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
    max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
    rumple <- rumple_index(chm) 
    top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    cover.fraction <- 1 - deepgap.fraction 
    vert.sd <- cloud_metrics(laz_data, sd(Z, na.rm = TRUE)) 
    sd.1m2 <- grid_metrics(laz_data, sd(Z), 1) 
    sd.sd <- sd(sd.1m2[,3], na.rm = TRUE) 
    Zs <- laz_data@data$Z
    Zs <- Zs[!is.na(Zs)]
    entro <- entropy(Zs, by = 1) 
    gap_frac <- gap_fraction_profile(Zs, dz = 1, z0=3)
    GFP.AOP <- mean(gap_frac$gf) 
    LADen<-LAD(Zs, dz = 1, k=0.5, z0=3) 
    VAI.AOP <- sum(LADen$lad, na.rm=TRUE) 
    VCI.AOP <- VCI(Zs, by = 1, zmax=35) 
    out.plot <- data.frame(
      matrix(c(x, y, mean.max.canopy.ht,max.canopy.ht, 
               rumple,deepgaps, deepgap.fraction, 
               cover.fraction, top.rugosity, vert.sd, 
               sd.sd, entro, GFP.AOP, VAI.AOP,VCI.AOP),
             ncol = 15)) 
    colnames(out.plot) <- 
      c("easting", "northing", "mean.max.canopy.ht.aop",
        "max.canopy.ht.aop", "rumple.aop", "deepgaps.aop",
        "deepgap.fraction.aop", "cover.fraction.aop",
        "top.rugosity.aop","vert.sd.aop","sd.sd.aop", 
        "entropy.aop", "GFP.AOP.aop",
        "VAI.AOP.aop", "VCI.AOP.aop") 
    print(out.plot)
  }
  
  plot.metrics[[i]]<-structural_diversity_metrics(laz_data)
  
}  # END LOOP

# post-loop processing
plot.metrics<-as.data.frame(rbindlist(plot.metrics))
plot.metrics$staplo<-rownames(centroids)
pm<-plot.metrics

head(pm) # make sure staplo is there

#plot_row_5.16<-pm
#plot_row_20.36<-pm
##mm<-rbind(plot_row_5.16, plot_row_20.36)


mm<-pm
mm$en<-paste(mm$easting, mm$northing)

centroids$en<-paste(centroids$V1, centroids$V2)
centroids$staplo<-paste(centroids$Stand, centroids$Plot)
# match info in. 
mm$staplo<-centroids$staplo[match(mm$en, centroids$en)]
mm$Stand<-centroids$Stand[match(mm$en, centroids$en)]
mm$Plot<-centroids$Plot[match(mm$en, centroids$en)]
###########################################################

# once you have all 9 stands in pm, you can proceed below.
############################################################################################
table(mm$staplo)

pm<-mm
pm$staplo<-gsub("-", " ", pm$staplo)

table(pm$staplo)
pm$Treatment<-sapply(pm[ ,"staplo"],switch,
                      "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                      "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                      "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                      "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                      "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                      "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P",
                      "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                      "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP",
                      "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N")
#centroids$staplo<-paste(centroids$Stand,centroids$Plot)

# if staplo isn't working
#pm$staplo<-gsub("-", " ", pm$staplo)
#pm$Stand<-centroids$Stand[match(pm$staplo,centroids$staplo)]

#N*P Anova
pm$Treatment<-factor(pm$Treatment, levels=c("Control","N","P","NP"))
pm$Ntrmt <- factor(  ifelse(pm$Treatment == "N" | pm$Treatment == "NP", "N", "NoN"))
pm$Ptrmt <- factor(  ifelse(pm$Treatment %in% c("P", "NP"), "P", "NoP"))

# Add ages
pm$Age[pm$Stand=="C1"]<-"~30 years old"
pm$Age[pm$Stand=="C2"]<-"~30 years old"
pm$Age[pm$Stand=="C3"]<-"~30 years old"
pm$Age[pm$Stand=="C4"]<-"~60 years old"
pm$Age[pm$Stand=="C5"]<-"~60 years old"
pm$Age[pm$Stand=="C6"]<-"~60 years old" 
pm$Age[pm$Stand=="C7"]<-"~100 years old"
pm$Age[pm$Stand=="C8"]<-"~100 years old"
pm$Age[pm$Stand=="C9"]<-"~100 years old"

# graph to see it
g<-gather(pm, "metric","value",3:15)


ggplot(g, aes(x=Stand, y=value, fill=Treatment))+ geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()+facet_wrap(~metric, scales="free_y")

library(lme4)
library(lmerTest)
### Functions for statistical anova
aov<- function(y, Stand, Ntrmt, Ptrmt, Age){
  slope1<-anova(lmer(y ~ Ntrmt*Ptrmt+Age+(1|Stand)))
  return(slope1)}

metric<-list()
for(i in c(3:15)){ 
  y = pm[,i]
  Age=pm$Age
  Stand= pm$Stand
  Ntrmt=pm$Ntrmt
  Ptrmt=pm$Ptrmt
metric[[i-2]] <- aov(y, Stand, Ntrmt, Ptrmt,Age)}

met<- as.data.frame(rbindlist(metric))
met$Source<-rep(c("N","P","Age","N*P"))
met$resp.var<-rep(names(pm)[c(3:15)], each=4)
names(met)
met<-met[ ,c(8,7,6,2,3,4,5)]
met<-na.omit(met)

met[met$`Pr(>F)`<0.05,]

##############################################

pm$year<-"2014"
pm$area<-"30x30m"
write.csv(pm, file="data/mel_complexity_2014_30.csv")




