pollutantmean <- function(directory, pollutant, id = 1:332) {
        files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
        dat <- data.frame()
        for (i in id){
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        means<-numeric(length(id))
        id_subset<-dat[which(dat[,"ID"]%in%id),]
        pollutant_column<-subset(dat,select=c(pollutant))
        mean(pollutant_column[,pollutant], na.rm= TRUE)
}
