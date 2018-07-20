#script to classify MapBiomas maps to new classification (from an Excel file) 
#write produced maps to file 

rm(list=ls())

library(raster)

cls <- c("PastureA", "PastureB", "PastureC")

yrls <- seq(2000,2015,1)  

for(i in seq_along(cls)){

  classification <- read_excel("MapBiomas_CRAFTY_classifications.xlsx", sheet = cls[i], range="B2:C21", col_names=F) 
  
  #loop over years  
  for(j in seq_along(yrls)){
    
    map <- raster(paste0("ASCII/brazillc_",yrls[j],"_5km_int.txt"))
    map <- reclassify(map, rcl=as.matrix(classification))
    
    writeRaster(map, paste0("ASCII/brazillc_",yrls[j],"_",cls[i],".asc"))
  }
}
