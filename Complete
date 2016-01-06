complete<-function(directory,id = 1:332){
        nobs<-vector()
        files<- list.files(directory, full.names = TRUE)
        for (i in 1:332){
                nobs[i]<-sum(complete.cases(read.csv(files[i])))
        }
        
        dataf<-data.frame(id,nobs[id])
        names(dataf)<-c('id','nobs')
        dataf
}
