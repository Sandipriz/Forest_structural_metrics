##preliminary R code to retrive NEON plot locations


library(neonUtilities)
library(devtools)
library(geoNEON)


tree <-
  loadByProduct(
    site = "BART",
    dpID = "DP1.10098.001",
    package = "basic",
    check.size = FALSE
  )

names(tree) # these are the data sub-files
tree

head(tree$vst_perplotperyear)

plots<-tree$vst_perplotperyear

table(plots$nlcdClass)

table(plots$eventID, plots$plotID )

lp<-as.data.frame(table(plots$plotID, plots$plotType ))


lp$nlcd<-plots$nlcdClass[match(lp$Var1, plots$plotID)]
lp$easting<-plots$easting[match(lp$Var1, plots$plotID)]
lp$northing<-plots$northing[match(lp$Var1, plots$plotID)]

head(lp)
write.csv(lp, file="Bartlett plots.csv")

# you can get locations of veg plots conveniently using getLocTOS
m <- getLocTOS(data = tree$vst_mappingandtagging,
               dataProd = "vst_mappingandtagging")
m
# calculate the mean easting and northing for the trees. this gets plot centers (to use for lidar)
locMean <-
  aggregate(
    list(
      adjNorthing = m$adjNorthing ,
      adjEasting = m$adjEasting
    ),
    by = list(plotID = m$plotID),
    FUN = mean,
    na.rm = T
  )


head(locMean)
