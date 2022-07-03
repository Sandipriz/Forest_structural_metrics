##preliminary R code to retrive NEON plot locations


library(neonUtilities)
library(devtools)
devtools::install_github("NEONScience/NEON-geolocation/geoNEON")  # work with NEON spatial data


library(geoNEON)


tree <-
  loadByProduct(
    site = "BART",
    dpID = "DP1.10098.001",
    package = "basic",
    check.size = FALSE
  )

names(tree)
tree

# you can get locations of veg plots conveniently using getLocTOS
m <- getLocTOS(data = tree$vst_mappingandtagging,
               dataProd = "vst_mappingandtagging")

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