##This function called "rankall" that takes two arguments: an outcome name (outcome) and a hospital ranking
##(num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
##containing the hospital in each state that has the ranking specified in num.
rankall <- function(outcome, num = "best")
{
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
    possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(!is.element(outcome, possible_outcomes))
    {
        stop("invalid outcome")
    }
    
    ## Format input outcome name in to column name in data
    outcome_formatted = sapply(strsplit(simpleCap(outcome), " "),paste,collapse=".")
    colname <- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome_formatted, sep=".")
    
    ## Extract state, hospital and outcome columns from data, remove all invalid rows
    dat <- subset(data, data[,colname] != "Not Available", select= c("State", "Hospital.Name", colname))
    
    ranked <- data.frame()
    
    dat[,colname] <- sapply(dat[,colname],as.numeric)
    statedata <- by(dat, dat$State, function(df) { d <- df[with(df, order(df[colname],df$Hospital.Name)),]
                                                   if (num == "best") {num <- 1 }
                                                   if (num == "worst") {num <- nrow(d) }
                                                   data.frame(hospital=d[num,"Hospital.Name"],state=df$State[1])
    } )
    
    ranked <- do.call(rbind, statedata)
}

## This function takes a string and capitalizes the first letter of each word.
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}