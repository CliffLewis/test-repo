pollutantmean <- function(directory, pollutant, id = 1:332) {
      
      vals = numeric(0)
      for (i in id) {
            if (i<10)
                  tmpData <- read.csv(paste(directory,"/00",i,".csv",sep=""))
            else if (i<100)
                  tmpData <- read.csv(paste(directory,"/0",i,".csv",sep=""))
            else
                  tmpData <- read.csv(paste(directory,"/",i,".csv",sep=""))
            
            useCol <- names(tmpData)==pollutant
            badIdx <- is.na(tmpData[,useCol])
            vals   <- c(vals,tmpData[!badIdx,useCol])
      }
      
      valMean <- mean(vals)
      print(valMean,digits=4) 
      
}