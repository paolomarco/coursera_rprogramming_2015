best<-function(state,outcome) {
data<-read.csv('/Users/tobiamartens/Desktop/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',colClasses = "character",na.strings = "Not Available")

names(data)[11]<-"heart_attack"
names(data)[17]<-"heart_failure"
names(data)[23]<-"pneumonia"

if (state%in%data[,7]){
        candidates<-subset(data,State==state)
} else {
      stop("invalid state")  
}
if (outcome=="heart attack"){
        candidates<-candidates[complete.cases(candidates[,11]),]
        candidates['heart_attack']<-as.numeric(candidates$heart_attack)
        candidates<-candidates[candidates$heart_attack==
                                       min(candidates$heart_attack,na.rm = TRUE),]
        lowest<-sort(candidates$Hospital.Name)
        lowest[1]
} else if (outcome=="heart failure"){
        candidates<-candidates[complete.cases(candidates[,17]),]
        candidates['heart_failure']<-as.numeric(candidates$heart_failure)
        candidates<-candidates[candidates$heart_failure==
                                       min(candidates$heart_failure,na.rm = TRUE),]
        lowest<-sort(candidates$Hospital.Name)
        lowest[1]
} else if (outcome=="pneumonia") {
        candidates<-candidates[complete.cases(candidates[,23]),]
        candidates['pneumonia']<-as.numeric(candidates$pneumonia)
        candidates<-candidates[candidates$pneumonia==
                                       min(candidates$pneumonia,na.rm = TRUE),]
        lowest<-sort(candidates$Hospital.Name)
        lowest[1]
} else {
        stop("Invalid outcome")
}}
