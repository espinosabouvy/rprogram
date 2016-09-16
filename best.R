best <- function(state, outcome){
     #verfiy outcome
     msg <- paste0("Error in best(",state,", ",outcome,") : invalid ")
     if (outcome != "heart attack" && outcome!="heart failure" && outcome !="pneumonia"){
          print(paste(msg,"outcome")) } 
     else {
          if (outcome == "heart attack") {disease <- 13}
          if (outcome == "heart failure"){disease <- 19}
          if (outcome == "pneumonia") {disease <- 25}
          
          #reading and prep file
          datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[c(2,7,disease)]
          colnames(datos) <- c("Hospital", "State", "Disease")
          datos[,3]<- suppressWarnings(as.numeric(datos[,3]))
          solost <- datos[datos[,2]==state,]
          #validate state
          cuantos <- nrow(solost)
          if (cuantos == 0) {
               print(paste(msg,"state"))
          } 
          else {
               best <- solost[which.min(solost$Disease),1]
               print(best)
          }
     }
     
}