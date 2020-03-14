## In order to run this script, you must run 'setwd()' function to specify where to find the data files
## i.e. :
## setwd('D:\\LEARNING\\R\\MyProjectFolder')

rankeachhospital <- function(state, outcome, num) { 
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings = "Not Available" )
    dat <- dat[, c(2, 7, 11, 17, 23)]
    
    ## generates the subset of data with provided State and validated outcome
    state<-toupper(state)
    dat_sub<-dat[which(dat$State==state),]
    
    ## checks if state is the correct one by 
    if(length(row.names(dat_sub))==0) { stop("Invalid State")}
    
    ## checks that provided outcome is valid
    colN<-0
    outcome<-tolower(outcome)
    if (outcome == "heart attack") { colN<-3 }      ## Col 11: Hospital 30-Day Death (Mortality) Rates from Heart Attack
    else if(outcome=="heart failure"){ colN<-4 }    ## Col 17: Hospital 30-Day Death (Mortality) Rates from Heart Failure
    else if (outcome == "pneumonia"){ colN<-5 }     ## Col 23: Hospital 30-Day Death (Mortality) Rates from Pneumonia
    else{stop("invalid outcome")}
    
    ## Conversion needed to do a proper sorting (otherwise it will take all numbers as strings)
    dat_sub[, colN] <- as.numeric(dat_sub[, colN])
    
    sortedData<-dat_sub[order(dat_sub[,colN], dat_sub[,1]), c(1,colN)]
    names(sortedData)[1]<-("Hospital")
    rank<-1
    
    if(num=="best"){ rank<-1 }
    else if (num=="worst"){ rank<-nrow(na.omit(sortedData)) }
    else { rank<-num }
    return(as.list(sortedData[rank,"Hospital"], state))
}
readStates<- function()
{
    require(dplyr)
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings = "Not Available" )
    states<- distinct(dat, State)
    states<- states[,"State"]
    states<-states[order(states)]
    return(states)
}
rankall <- function(outcome, num = "best") { 

    states<-as.list(readStates())
    joinedResults <- matrix(ncol=2, nrow=0)
    colnames(joinedResults)<- c("hospital", "state")
    for(i in states)
    {
        currentState<- rankeachhospital(i, outcome, num)
        joinedResults<-rbind(joinedResults,c(currentState, i) )
    }
    
    joinedResults
}

rankall2 <- function(outcome, num = "best") { 
    
    states<-as.list(readStates())
    joinedResults <- matrix(ncol=2, nrow=0)
    colnames(joinedResults)<- c("hospital", "state")
    for(i in states)
    {
        currentState<- rankeachhospital(i, outcome, num)
        joinedResults<-rbind(joinedResults,c(currentState, i) )
    }
    
    joinedResults
} 

