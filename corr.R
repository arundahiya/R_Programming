##This creates a function "corr" that takes a directory of data files 
##and a threshold for complete cases and calculates the correlation
##between pollutants for monitoring purposes.
corr <- function(directory, threshold = 0) {
    ##This stores the directory of data into "dat"
    dat <- complete(directory)
    ##This sorts the data into alphabetical order, then returns a vector of
    ##files in the directory in to "files"
    files <- list.files(directory, full.names=TRUE)
    ##This creates an empty vector called "correlate"
    correlate <- vector()
    ##This loop goes through the data and returns values over the threshold. 
    ##If the value is over the threshold, it correlates the sulfate and 
    ##nitrate values using the "complete.obs" data set. 
    for(i in 1:nrow(dat))
    {
        if(dat[i,"nobs"] > threshold)
        {
            file = read.csv(files[i])
            file_sul <- file$sulfate            
            file_nit <- file$nitrate            
            correlate <- c(correlate, cor(file_sul,file_nit,use="complete.obs"))
        }
    }
    ##If the value is below the threshold, it returns 0.
    if (length(correlate) == 0)
    {
        correlate <- numeric(0)
    }
    
    correlate
}

