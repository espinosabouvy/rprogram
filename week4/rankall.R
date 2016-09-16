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
     
     #used before checking the forum
     #if (outcome == "heart attack") {disease <- 11}
     #if (outcome == "heart failure"){disease <- 17}
     #if (outcome == "pneumonia") {disease <- 23}
     #much better
     disease <- c("heart attack" = 11, "heart failure" = 17, "pneumonia"=23)
     
     
     #reading and prep file
     datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available"
                       , stringsAsFactors = FALSE)[c(2,7,disease[outcome])]
     colnames(datos) <- c("hospital", "state", "rate")
     datos <- datos[!is.na(datos$rate),]
     
     #order
     if(is.numeric(num)){
          rank <- num
          datos <- datos[order(datos$state, as.numeric(datos$rate), datos$hospital),]
     } else {
          if (num == "best") {
               rank <- 1
               datos <- datos[order(datos$state, as.numeric(datos$rate), datos$hospital),]
          } else {
               if (num == "worst"){
                    rank <- 1
                    datos <- datos[order(datos$state, -as.numeric(datos$rate), datos$hospital),]     
               } else {stop("outcome invalid",call. = FALSE)}
          }
     }

     #split in state groups, and extracting row(rank) of each group, create data frame
     result <- NULL
     best <- split(datos, datos$state)
     tabla <- sapply(best, function(x){x[[1]][rank]})
     result <- data.frame("hospital" = tabla,"state" = names(tabla))
     #print(result)
}