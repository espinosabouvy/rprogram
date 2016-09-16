rankhospital <- function(state, outcome, num ="best"){
     
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
     solost <- datos[datos[,2]==state,]
     
     #validate state
     cuantos <- nrow(solost)
     if (cuantos == 0) {stop("invalid state", call. = FALSE)} 
     
     #ordering data
     solost <- solost[order(solost$Rate, solost$Hospital, decreasing = orden),]
     
     #showing results
     if (is.numeric(num)){
          if (num > cuantos){
               ## show NA, user asking for bigger rank than existing
               stop("NA", call. = FALSE)
          } else {
               # show in acendent order the number of rows required
               if (num>0){pr <- c(1:cuantos)} else {pr <- c(cuantos:1)}
               print(pr)
               solost["Rank"] <- pr
               print(head(solost, abs(num)))
          }
          
     } else {
          solost <- solost[order(solost$Rate, solost$Hospital, decreasing = orden),]
          print(solost[1,1])
     }
     
}