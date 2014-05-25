pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
                
        filesdir <- directory
                
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        poltype <- pollutant
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used, so the data in the corresponding files must be read & 
        ## stored.
        
        monitorids <- id
        monitorcount <- (length(monitorids))
                                
        m <- 1
        pollutiondata <- data.frame()  
         
        while (m <= monitorcount) {
                filename <- paste(formatC(monitorids[m], width=3, flag = "0"), ".csv", sep = "")
                pathname <- paste(filesdir, "/", filename, sep ="")
                pollutiondata <- rbind(pollutiondata, read.csv(pathname))
                m <- m + 1
        }
        
        onetype <- data.frame()
        if (poltype == "sulfate") {
                onetype <- pollutiondata[,2]
        }else if (poltype == "nitrate") {
                onetype <- pollutiondata[,3]
        }else{
                print("***********")
                print("Invalid pollutant passed to function. Please re-enter.")
                print("***********")
        }        
                
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        round(mean(onetype, na.rm = TRUE), 3)
}