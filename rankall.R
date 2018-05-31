rankall <- function(outcome, num = "best") {
        ## Read outcome data
        datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #outcome validos
        voc <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that outcome is valid
        if(is.na(match(outcome, voc))) stop("invalid outcome")
        
        library(dplyr)
        library(tidyr)
        
        #genera vector donde se gurdan nombres hospitales
        state = levels(as.factor(datos$State))
        hospital <- character(length(state))
        
        #como las columnas de interes estan separas por 6 y en mismo orden 
        #vector out, obtengo indice de interes
        i <- 5+match(outcome, voc)*6
        
        ## For each state, find the hospital of the given rank
        for (j in seq_along(state)) {
                
                
                ## Return hospital name 
                sdatos <- filter(datos, State == state[j])
        
                #Convierte a numero la mortalidad
                suppressWarnings(sdatos[, i] <- as.numeric(sdatos[, i]))
        
                #ordena por mortalidad y luego por nombre
                sdatos <- arrange(sdatos, sdatos[,i], sdatos[,2])
        
                #elimina los valores NA
                sdatos <- drop_na(sdatos, i)
        
                ## 30-day death rate
                #entrega primer valor
                if(num == "best"){ hospital[j] = sdatos[1,2] }
        
                #entrega ultimo valor
                else if(num == "worst"){ hospital[j] = tail(sdatos[,2], 1)  }
                
                #entrega valor de num
                else{
                        num = as.integer(num)
                        hospital[j] = sdatos[num,2]
                }
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name 
        
        ranking = data.frame(hospital, state)
}