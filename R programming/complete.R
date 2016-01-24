complete<-function(directory,id=1:322){
        myfile<-list.files(directory,full.names=TRUE)
        
        num<-c()
        for(i in id){
                dat<-data.frame()
                dat<-read.csv(myfile[i])
                num<-rbind(num,sum(complete.cases(dat)==TRUE))
                
               
        }
        
        mydata<-data.frame(id,nobs=num)
        mydata
        
}




