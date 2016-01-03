rankhospital<-function(state,outcome,num="best"){
data<-read.csv('/Users/tobiamartens/Desktop/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',colClasses = "character",na.strings = "Not Available")

names(data)[11]<-"heart_attack"
names(data)[17]<-"heart_failure"
names(data)[23]<-"pneumonia"

if (state%in%data[,7]){
        candidates<-subset(data,State==state)
} else {
        stop("invalid state")  
} 

if (outcome %in% c("heart attack","pneumonia","heart failure")) {
        candidates<-candidates
} else {
        stop ("invalid outcome")
}
candidates$heart_failure<-as.numeric(candidates$heart_failure)
candidates$heart_attack<-as.numeric(candidates$heart_attack)
candidates$pneumonia<-as.numeric(candidates$pneumonia)

if (outcome == 'heart failure'){
        candidates<- candidates[order(candidates$heart_failure,candidates$Hospital.Name,na.last = NA),]
        for (i in seq_along(candidates$heart_failure)){
                candidates$Rank[i]<-i
        }
} else if (outcome == 'heart attack') {
        candidates <- candidates[order(candidates$heart_attack,candidates$Hospital.Name,na.last = NA),]
        for (i in seq_along(candidates$heart_attack)) {
                candidates$Rank[i]<-i
        }
} else if (outcome == 'pneumonia') {
        candidates <- candidates[order(candidates$pneumonia,candidates$Hospital.Name,na.last = NA),]
        for (i in seq_along(candidates$pneumonia)) {
                candidates$Rank[i]<-i
        }
}
if (num == 'best'){
        candidates<-candidates[min(candidates$Rank),]
        candidates$Hospital.Name
} else if (num == 'worst'){
        candidates<-(candidates[max(candidates$Rank),])
        candidates$Hospital.Name
} else if (num %in% candidates$Rank){
        candidates<-candidates[candidates$Rank==num,]
        candidates$Hospital.Name
} else if (num > length(candidates$Rank)){
        print(NA)
}}

