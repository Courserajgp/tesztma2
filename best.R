rankhospital <- function(state, outcome, num){
        
## Read outcome data
        
        setwd("~/Coursera R/Programming in R/programming assignment 3/rprog-data-ProgAssignment3-data")
        outcometable <- read.csv("outcome-of-care-measures.csv")
        z <- c(1,"hospitalname",3,4,5,6,"statename",8,9,10,"HA",12,13,14,15,16,"HF",18,19,20,21,22,"PN",24,
               25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
               46)
        colnames(outcometable) <- z
        
## Check that state and outcome is valid
        
        states <- c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", 
                    "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY",
                    "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT",
                    "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK",
                    "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA",
                    "VI", "VT", "WA", "WI", "WV", "WY")
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        validstate <- FALSE; validoutcome <- FALSE
        
        for (i in 1:56){if (state == states[i]){validstate <- TRUE; stateseq <- i}}
        if(validstate == FALSE){stop(c("invalid state"))}
        
        for (j in 1:3){if (outcome == outcomes[j]){validoutcome <- TRUE; outcomeseq <- j}}
        if(validoutcome == FALSE){stop(c("invalid outcome"))}
        
## identify and subset all hospitals in the state 
        l1 <- nrow(outcometable)
        l2 <- rep(FALSE, times = l1) ### l2 = logical vector, TRUE for hospitals in the state
        for (k in 1:l1){if(outcometable[k,7]==state){l2[k] <- TRUE}}
        statehospitals <- subset(outcometable, subset = l2)
        order.abc <- order(statehospitals$hospitalname)
        index1 <- with(statehospitals, order(hospitalname, decreasing=FALSE))
        statehospitals.abc <- statehospitals[index1,]


## Rank hospitals according to 30-day death rate and hospital name
        
        statehospitals.abc[,11] <- as.numeric(statehospitals.abc[,11])
        statehospitals.abc[,17] <- as.numeric(statehospitals.abc[,17])
        statehospitals.abc[,23] <- as.numeric(statehospitals.abc[,23])
        
        order.HA <- order(statehospitals.abc$HA, decreasing=FALSE, na.last=NA)
        order.HF <- order(statehospitals.abc$HF, decreasing=FALSE, na.last=NA)
        order.PN <- order(statehospitals.abc$PN, decreasing=FALSE, na.last=NA)
        order.outcome <- seq(nrow(statehospitals.abc))
        order.abc <- order(statehospitals.abc$hospitalname)
        outc <- NULL
        ranked <- NULL
        index <- NULL
        
        if(outcome == "heart attack"){
                order.outcome <- order.HA
                index <- with(statehospitals.abc, order(HA, hospitalname, decreasing=FALSE, na.last=NA))
                #ranked <- statehospitals[order.HA,]
        }
        if(outcome == "heart failure"){
                order.outcome <- order.HF
                index <- with(statehospitals.abc, order(HF, hospitalname, decreasing=FALSE, na.last=NA))
                #ranked <- statehospitals[order.HF,]
        }
        if(outcome == "pneumonia"){
                order.outcome <- order.PN
                index <- with(statehospitals.abc, order(PN, decreasing=FALSE, na.last = NA))
                #ranked <- statehospitals[order.PN,]
        }
        
        ranked <- statehospitals.abc[index,]
        result <- as.vector(ranked[1,2])
        
## Return hospital name in that state with lowest 30-day death rate
       print(result)
}