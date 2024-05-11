library(mrremind)
library(mrcommons)


# load libraries
devtools::load_all("dev/mredgebuildings")
#library(mredgebuildings)

setConfig(outputfolder = "/p/tmp/hagento/output",
          mappingfolder = "/p/tmp/hagento/dev/mredgebuildings/inst",
          ignorecache = c("ISIMIPbuildings"))

#raster::rasterOptions(tmpdir = "/p/tmp/hagento/.Rtmp")

# List with the names of the files containing the ISO-to-region mapping to which the data should be aggregated. 
# Each list element is a named vector with:
# - the first element "regionmapping": region mapping used for the aggregation of the input data
# - the second element "extramappings_historic": mapping with regions to which only the historic data are aggregated in addition to the normal region mapping

# regionmapping <- "regionmappingISO-EDGE_EUR_ETP.csv"

# Current input data revision (<mainrevision>.<subrevision>) ####
revision <- "0.1"   # should be a number with two decimal places for production
cachetype <- "def"
dev <- ""
puc <- identical(dev, '')
renv <- FALSE

sessionInfo()


# HISTORICAL

f <- "MPI-ESM1-2-HR_historical.csv"

# EDGE mapping
# retrieveData(model = "HDDCDD", regionmapping = regionmapping, rev = revision,
#              dev = dev, cachetype = cachetype, puc = puc, renv = renv)

dev <- paste0("_", gsub(".csv", "", f))
  
# ISO
retrieveData(model = "hddcdd", file = f, rev = revision,
             dev = dev, cachetype = cachetype, puc = puc, renv = renv)


