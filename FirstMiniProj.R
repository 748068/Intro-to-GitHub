## Problem 1
## Identifying the pollutant mean

pollutantmean <- function(directory, pollutant, id){
  
  mean <- vector()
  
  data <- 0
  
  File <- list()
  
  setwd("C:/Users/Win10/Desktop/")
  
  list <- list.files(directory)
  
  location <- paste("C:/Users/Win10/Desktop/",directory ,sep ="")
  
  setwd(location)
  
  
  for (i in id) {
    
    File[[i]] <- read.csv(list[i])
    
    variable <- which(colnames(File[[i]])== pollutant)
    
    
    data <- File[[i]][which(is.na(File[[i]][,variable])==FALSE), pollutant]
    
    mean <- c(mean, data)
    
    
  }
  print(mean(mean))
  
}

##sample code
pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064128

pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706047

pollutantmean("specdata", "nitrate", 23)
## [1] 1.280833

## Problem 2
## Identifying the number rows that has complete data in specified id

complete <- function(directory, id){
  
  nobs <- vector()
  
  data <- 0
  
  File <- list()
  
  setwd("C:/Users/Win10/Desktop/")
  
  list <- list.files(directory)
  
  location <- paste("C:/Users/Win10/Desktop/",directory ,sep ="")
  
  setwd(location)
  
  
  for (i in id) {
    
    File[[i]] <- read.csv(list[i])
    
    
    data <- nrow(na.omit(File[[i]]))
    
    nobs <- c(nobs, data)
    
  }
  
  data.frame(id, nobs)
}

complete("specdata", 1)
##   id nobs
## 1  1  117

complete("specdata", c(2,4,8,10,12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96

complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586

## Problem 3
## Identifying the correlation of nitrate and sulfate on variables greater than
## or equal the threshold

corr <- function(directory, threshold = 0){
  
  stored <- NULL
  
  data <- 0
  
  File <- list()
  
  setwd("C:/Users/Win10/Desktop/")
  
  list <- list.files(directory)
  
  location <- paste("C:/Users/Win10/Desktop/",directory ,sep ="")
  
  setwd(location)
  
  
  for (i in 1:332) {
    
    File[[i]] <- read.csv(list[i])
    
    File[[i]] <- na.omit(File[[i]])
    
    if(nrow(File[[i]]) >= threshold){
      
      data <- cor(File[[i]]$sulfate, File[[i]]$nitrate)
      stored <- c(stored, data)
    }
    
    
  }
  
  return(na.omit(stored))
}

cr <- corr("specdata", 150)
head(cr); summary(cr)

## -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.21057 -0.05147  0.09333  0.12401  0.26836  0.76313 

cr <- corr("specdata", 5000)
head(cr); summary(cr); length(cr)

## NULL
## Length  Class   Mode 
##      0   NULL   NULL 
## [1] 0

cr <- corr("specdata")
head(cr); summary(cr); length(cr)

## [1] -0.22255256 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667

##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.      
## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000        

## [1] 323

## Problem 4
## Modifying a histogram 

setwd("C:/Users/Win10/Desktop")

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

ls(outcome[11])

main <- ("Hospital 30-Day Death (Mortality) Rates from Heart Attack")

hist(outcome[, 11], main = main, xlab = "Deaths", col = "cyan2")
