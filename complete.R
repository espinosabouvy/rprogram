complete <- function(direc, id = 1:332){
     archivos <- list.files(direc,full.names = TRUE)
     for (i in id) {
          casos <- sum(complete.cases(read.csv(archivos[i])))
          #falta crear el frame con los datos
     }
}