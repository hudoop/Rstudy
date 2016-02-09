rankhospital<-function(state,outcome,num="best"){
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
                df1<-df[which(df$state==state),]
                df1<-na.omit(df1)
                df1<-df1[order(df1$state,df1$outcome,df1$hospital),]
                
                if(class(num)=="character"){
                        if(num=="best"){
                                return(df1$hospital[1])
                        }
                        else if(num=="worst"){
                                return(df1$hospital[nrow(df1)])
                        }
                }
                else{
                        return(df1$hospital[num])
                }
                
        }
        
}