rankhospital <- function(state, outcome, num ="best"){
     
     #function for changing sorting
     
     if (is.numeric(num)){or <- 1}
     else {
          if (as.character(num) == "best"){
               or <- 1
               num <- 1
          } else if (as.character(num) == "worst") {
               or <- -1
               num <- 1
               }
          else {stop(("invalid num"), call. = FALSE)}
     }
     
     #verfiy outcome and assign column name acoordingly
     if (outcome != "heart attack" && outcome!="heart failure" && outcome !="pneumonia"){
          stop("invalid outcome", call. = FALSE)
     } 
     
     disease <- c("heart attack" = 11, "heart failure" = 17, "pneumonia"=23)
     
     
     #reading and prep file
     datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available"
                       , stringsAsFactors = FALSE)[c(2,7,disease[outcome])]
     colnames(datos) <- c("hospital", "state", "rate")
     solost <- datos[datos$State == state,]
     solost <- solost[!is.na(solost$Rate),]
     
     #validate state
     cuantos <- nrow(solost)
     if (cuantos == 0) {stop("invalid state", call. = FALSE)} 
     
     #ordering data
     solost <- solost[order(or*as.numeric(solost$rate), solost$hospital),]

     
     #showing results
     result <- solost[num,1]
     print(result)
}