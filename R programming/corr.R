corr<-function(directory,threshold=0){
        myfile<-list.files(directory,full.names = TRUE)
        result<-c()
        for(i in 1:322){
                dat<-na.omit(read.csv(myfile[i]))
                if(nrow(dat)>threshold){
                        result<-append(result,cor(dat$sulfate,dat$nitrate))
                }
                else{0}
        }
        round(result,5)
        
}


