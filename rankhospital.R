rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #outcome validos
        voc <- c("heart attack", "heart failure", "pneumonia")

        
        ## Check that state and outcome are valid
        # if(is.na(match(state, state.abb))) stop("invalid state") #solo consideraba 50 estado, faltabn 4
        
        if(is.na(match(state, levels(as.factor(datos$State))))) stop("invalid state")
        if(is.na(match(outcome, voc))) stop("invalid outcome")

        
        library(dplyr)
        library(tidyr)
        ## Return hospital name in that state with the given rank
        sdatos <- filter(datos, State == state)
        
        #ha 11
        #hf 17
        #np 23  
        #como las columnas de interes estan separas por 6 y en mismo orden 
        #vector out, obtengo indice de interes
        i <- 5+match(outcome, voc)*6
        
        #para ordenar no es necesario que sean numericos.
        #sdatos[, i] <- as.numeric(sdatos[, i]) 
        
        #Convierte a numero la mortalidad
        suppressWarnings(sdatos[, i] <- as.numeric(sdatos[, i]))
        
        #ordena por mortalidad y luego por nombre
        sdatos <- arrange(sdatos, sdatos[,i], sdatos[,2])
        
        #elimina los valores NA
        sdatos <- drop_na(sdatos, i)
        
        ## 30-day death rate
        #entrega primer valor
        if(num == "best"){
                return(sdatos[1,2])
        }
        
        #entrega ultimo valor
        else if(num == "worst"){
                return(tail(sdatos[,2], 1))
        }
        
        else{
                num = as.integer(num)
                return(sdatos[num,2])
        }

        
        
}