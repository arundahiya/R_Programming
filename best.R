## This function takes two arguments: the 2-character abbreviated name of a 
## state and an outcome name. The function reads the outcome-of-care-measures.csv
## file and returns a character vector with the name of the hospital that has 
## the best (i.e. lowest) 30-day mortality for the specified outcome in that state.
best <- function(state, outcome)
{
    ## Read outcome data and store into "data"
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
    ## This concatenates the 3 possible outcomes into a vector that is called 
    ##"possible_outcomes"
    possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## This loop checks that the state and outcome are valid in terms of 
    ##whether or not they exist. If the state doesn't exist, the function
    ##returns "invalid state". If the outcome doesn't exist, the function
    ##returns "invalid outcome".
    if(!is.element(state, state.abb))
    {
        stop("invalid state")
    }
    if(!is.element(outcome, possible_outcomes))
    {
        stop("invalid outcome")
    }
    
    ## This formats the input outcome name in to column name in the data and stores it as 
    ##outcome_formatted.
    outcome_formatted = sapply(strsplit(simpleCap(outcome), " "),paste,collapse=".")
    colname <- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome_formatted, sep=".")
    
    ## This extracts the hospital and outcome columns from data for the state passed in
    dat <- subset(data, State==state, select= c("Hospital.Name", colname))
    
    ## This removes all invalid occurrences
    dat <- dat[dat[,colname] != "Not Available",]
    
    ## This sorts by hospital name
    sorted_name <- dat[order(dat["Hospital.Name"]),]
    
    ## Make outcome values numeric, and sort by outcome. This makes the minimum
    ## come to the top, and if there is a tie, the hospitals are already sorted 
    ## by name from the previous step
    sorted_name[,colname] <- sapply(sorted_name[,colname],as.numeric)
    sorted_min <- sorted_name[order(sorted_name[colname]),]
    
    ## Return hospital name in that state with lowest 30-day death rate
    print(sorted_min[1,"Hospital.Name"])

}

## This function takes a string and capitalizes the first letter of each word.
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}