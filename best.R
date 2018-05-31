best <- function(state, outcome){
        ## Read outcome data
        datos <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #outcome validos
        voc <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        #if(is.na(match(state, state.abb))) stop("invalid state") #solo considera 50 estados faltaban 4
        if(is.na(match(state, levels(as.factor(datos$State))))) stop("invalid state")
        if(is.na(match(outcome, voc))) stop("invalid outcome")
        
        #cargar libreria para manipular DF
        library(dplyr)

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        
        #filtra obteniendo solo datos para state requerido
        sdatos <- filter(datos, State == state)
        
        #ha 11
        #hf 17
        #np 23  
        #como las columnas de interes estan separas por 6 y en mismo orden 
        #vector out, obtengo indice de interes
        i <- 5+match(outcome, voc)*6
        
        #para ordenar no es necesario que sean numericos.
        #sdatos[, i] <- as.numeric(sdatos[, i]) 
        
        #ordena por mortalidad y luego por nombre
        sdatos <- arrange(sdatos, sdatos[,i], sdatos[,2])
        
        sdatos[1,2]

        
}