complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        filesdir <- directory
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        monitorids <- id
        monitorcount <- (length(monitorids))
                
        m <- 1
        pollutiondata <- data.frame() 
        results <- data.frame(id = numeric(0), nobs = numeric(0))
        i <- 1
                
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        while (m <= monitorcount) {
                filename <- paste(formatC(monitorids[m], width=3, flag = "0"), ".csv", sep = "")
                pathname <- paste(filesdir, "/", filename, sep ="")
                pollutiondata <- rbind(pollutiondata, read.csv(pathname))
                pollutiondata <- na.omit(pollutiondata)
                results[i, 1 ] <- monitorids[m]
                results[i, 2 ] <- nrow(pollutiondata) 
                
                pollutiondata <- data.frame() 
                m <- m + 1
                i <- i + 1
        }
        
        print(results)
}