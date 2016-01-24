setwd("D:\\coursera\\R programming")
source("pollutantmean.R")
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
source("complete.R")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

complete("specdata")
complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 54)
cc <- complete("specdata", 54)
print(cc$nobs)

source("complete.R")
set.seed(42)
cc <- complete("specdata", 332:1)
cc
use <- sample(332, 10)
print(cc[use, "nobs"])

source("complete.R")
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
class(cr)

source("complete.R")

1041+474+192+148+96
?cor
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata")
summary(cr)
log(-1)

