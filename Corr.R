corr<-function(directory,threshold=0){
        dat<-data.frame()
        files<-list.files(directory,full.names = TRUE)
        for (i in 1:332){
                if(sum(complete.cases(read.csv(files[i])))>threshold)
                        dat<-rbind(dat,read.csv(files[i]))
        }
        
        corr_vector<-vector()
        result<-as.numeric(vector())
        unique_ids<-unique(dat$ID)
        for (i in unique_ids){
                dat_subset<-dat[which(dat[,"ID"]==i),]
                good<-complete.cases(dat_subset)
                goodDF<-dat_subset[good,]
                corr_vector[i]<-cor(goodDF$sulfate,goodDF$nitrate)
        }
        good<-complete.cases(corr_vector)
        result<-corr_vector[good]
        as.numeric(result)
}
