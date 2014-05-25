corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
                
        filesdir <- directory
        filelist <- list.files(filesdir)
        filecount <- (length(filelist))
                       
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        bar <- threshold
               
        m <- 1
        corrData <- vector(mode="numeric", length=0)
                        
        while (m <= filecount) {
                pollutiondata <- data.frame()
                pathname <- paste(filesdir, "/", filelist[m], sep ="")
                pollutiondata <- rbind(pollutiondata, read.csv(pathname))
                pollutiondata <- na.omit(pollutiondata)
                pollutioncount <- nrow(pollutiondata)
                if(pollutioncount >= bar){
                        corrData<-c(corrData, cor(pollutiondata$sulfate, pollutiondata$nitrate))
                        }
                m <- m + 1
                }
        
        ## Return a numeric vector of correlations
        round(corrData, 5)
}