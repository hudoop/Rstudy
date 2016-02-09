best<-function(state,outcome){
        mydata<-read.csv("outcome-of-care-measures.csv",na.strings = "Not Available",stringsAsFactors = FALSE)
        outcomes<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        df<-mydata[,c(2,7,outcomes[outcome])]
        names(df)<-c("hospital","state","outcome")
        if(!(state%in%df$state)){
                stop("invalid state")
        }
        else if(!(outcome%in%names(outcomes))){
                stop("invaild outcome")
        }
        else if((state%in%df$state)&(outcome%in%names(outcomes))){
                df1<-na.omit(df)
                df2<-df1[which(df1$state==state),]
                df2<-df2[which.min(df2$outcome),]
                return(df2[,1])
        }
}