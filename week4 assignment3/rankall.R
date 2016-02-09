rankall<-function(outcome,num="best"){
        mydata<-read.csv("outcome-of-care-measures.csv",na.strings = "Not Available",stringsAsFactors = FALSE)
        outcomes<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        df<-mydata[,c(2,7,outcomes[outcome])]
        names(df)<-c("hospital","state","outcome")
       
        if(!(outcome%in%names(outcomes))){
                stop("invaild outcome")
        }
        df<-df[order(df$state,df$outcome,df$hospital),]
        df<-na.omit(df)
        my_split<-split(df,df$state)
        
        #sapply(my_split,class)
        
        result<-function(data){
                if(class(num)=="character"){
                        if(num=="best"){
                                return(data$hospital[1])
                        }
                        else if(num=="worst"){
                                return(data$hospital[nrow(data)])
                        }
                }
                else{
                        return(data$hospital[num])
                }
        
        }
        mydata1<-lapply(my_split,result)
        hospital<-unlist(mydata1)
        state<-names(mydata1)

        a<-as.data.frame(cbind(hospital,state))
        
        

        
}


