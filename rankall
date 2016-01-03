rankall<-function(outcome,num='best'){
hospitals<-read.csv('/Users/tobiamartens/Desktop/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',stringsAsFactors = FALSE,na.strings = "Not Available")

outcomes<-c("heart failure"=17,"heart attack"=11,"pneumonia"=23)

hospitals<-hospitals[,c(2,7,outcomes[outcome])]

if (!(outcome %in% names(outcomes))){
        stop("invalid outcome")
}

names(hospitals)<-c('hospital','state','outcome')

hospitals<-hospitals[order(hospitals$state,hospitals$outcome,hospitals$hospital,na.last = NA),]

split_data<-split(hospitals,hospitals$state)

pickRank<-function(list_of_Dfs,index=num){
        if (index == 'best'){
                list_of_Dfs$hospital[1]
        } else if (index == 'worst') {
                list_of_Dfs$hospital[nrow(list_of_Dfs)]
        } else list_of_Dfs$hospital[index]
}

hospital<-lapply(split_data,pickRank)
state<-names(lapply(split_data,pickRank))
as.data.frame(cbind(hospital,state))
}



