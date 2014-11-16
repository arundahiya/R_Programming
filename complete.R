##This creates the function "complete", which takes the variables "directory" and "id" and finds only complete rows for these. 
complete <- function(directory,id = 1:332) {
 
    ##Create a list of files in my directory
    files<-list.files(directory,full.names=TRUE)

    ##This initializes an empty data frame
    dat <-data.frame()
    
    ##Create a for loop to know which files to get complete case from  
    for (i in id){
        filedata <-read.csv(files[i])
    
        ##Count all rows without NAs
        combined<- sum(complete.cases(filedata))
        dat<- rbind(dat, c(i, combined))
    }
    ##This names the returned colums "id" and "nobs" respectively in my data frame
    colnames(dat) <- c("id","nobs")
    ##Return data frame with how many complete cases there are
     dat 
}