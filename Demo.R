
#Packages
library(dplyr)
library(raster)
########################################################################################################################################################################################################################################################################################################################################################################################

##Data
dataINDIVIDUAL           <- readRDS("SubSample.rds")
colnames(dataINDIVIDUAL) <- c("ID", "Energy", "Volume")
                       
##ID/Plan
rB    = raster("net_number.asc")
dataB = rasterToPoints(rB); colnames(dataB) = c("XBat","YBat","NetNumber")


QRA_3Dmatrix = array(0,dim = c(nrow(dataB),19,2))  

for (i in 1:nrow(dataINDIVIDUAL)) {
         row    <- as.numeric(dataINDIVIDUAL[i,1])                    # row corresponds to IDcell 
         column <- floor(as.numeric(dataINDIVIDUAL[i,3]))             # column corresponds to the volume class
         if (column == 20) {column = 19}
         QRA_3Dmatrix[row,column,1] <- QRA_3Dmatrix[row,column,1] + 1 # Nr passage, initially zero, then +1 
         QRA_3Dmatrix[row,column,2] <- QRA_3Dmatrix[row,column,2] + as.numeric(1 - (1.358/(1 + exp((((dataINDIVIDUAL[i,2]*10^3) - 129000)/120300)))))
         print(i)
}


