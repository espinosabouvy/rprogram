rankall <- function(outcome, num ="best"){
     #function for changing sorting
     
     if (is.numeric(num)){
          if (num >=0){orden <- FALSE } else {orden <- TRUE}
     } else {
          if (as.character(num) == "best"){orden <- FALSE} else if (as.character(num) == "worst") {orden <- TRUE}
          else stop(("invalid num"), call. = FALSE)
     }
     
     
     #verfiy outcome and assign column name acoordingly
     if (outcome != "heart attack" && outcome!="heart failure" && outcome !="pneumonia"){
          stop("invalid outcome", call. = FALSE)
     } 
     if (outcome == "heart attack") {disease <- 11}
     if (outcome == "heart failure"){disease <- 17}
     if (outcome == "pneumonia") {disease <- 23}
     
     #reading and prep file
     datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[c(2,7,disease)]
     colnames(datos) <- c("Hospital", "State", "Rate")
     datos[,3]<- suppressWarnings(as.numeric(datos[,3]))
     #order
     datos <- datos[order(datos[,1]),]
     datos <- datos[order(datos[,3], decreasing = TRUE),]
     datos <- datos[order(datos[,2]),]
     
     #assign ranking+
     ranking <- NULL
     rankk <- 1
     edo <- datos[1,2]
     for (i in 1:nrow(datos)){
          #when state change
          if (datos[i,2] != edo){
               rankk<- 1
               edo <- datos[i,2]
          }
          #creating vector
          if(is.na(datos[i,3])){
               ranking[i]= NA
          } else {
               ranking[i] <- rankk
               rankk = rankk+1
          }
     }
     datos["Rank"] <- ranking
     head(datos,50)
     
     if(num=="best"){
          best <- datos[datos$Rank == ranking,]
          best <- best[!is.na(best$Rank),c(1,2)]
          print(best)
     }
     
}