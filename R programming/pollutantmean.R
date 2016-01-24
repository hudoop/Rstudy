setwd("D:\\coursera\\R programming")
pollutantmean<-function(directory,pollutant,id=1:332){
        datafile<-list.files(directory,full.names = TRUE)
        dat<-data.frame()
        for(i in id){
                dat<-rbind(dat,read.csv(datafile[i]))
        }
       round(mean(dat[,pollutant],na.rm = TRUE),digits =3)
        
}




