##This creates the function "pollutantmean", which takes the arguments directory, pollutant, and ID 
## and returns the mean of the pollutant levels
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ##This creates a list of all of the paths to access the pollutant files in my directory. 
    files<-list.files(directory,full.names=TRUE)
    ##This initializes an empty data frame
    dat <-data.frame()
    
    ##This for loop accesses specific files from my dataset and binds them into my data frame.
    for (i in id){
        filedata <-read.csv(files[i])
        dat<- rbind(dat, filedata)
    }
    ##This creates a subset of the specific pollutant column from my dataframe.
    mydata<-dat[,pollutant]
    ##This removes the NAs from all the values to make it usable and takes the mean of the remaining values.
    mean(mydata,na.rm=TRUE)
    
}