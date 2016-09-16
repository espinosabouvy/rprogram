pollutantmean <- function(direc ="specdata", pollutant, id = 1:10){
     print(paste0("Mean of ", pollutant, " by monitor "))
     archivos <- list.files(direc,full.names = TRUE)
     for (i in id) {
          contamina <-read.csv(archivos[i])[pollutant]
          sina <- mean(contamina[!is.na(contamina)])
          #contamina <- subset.data.frame(tabla,select = pollutant, na.rm = FALSE)
          print(paste(i, ":", sina))        
     }

}