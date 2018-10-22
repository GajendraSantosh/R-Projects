setwd('D:/Documents/R Projects/')
oData <- read.csv("T3CompanyLocationFM10.csv", stringsAsFactors=FALSE)
newTable = data.frame(ID = numeric(), 
                      Title = character(), 
                      LocationId = numeric(), 
                      AddressLine1 = character(), 
                      Region = character(),
                      Country = character(), 
                      City = character(), 
                      zip = numeric(),
                      dLocationId = character())
i=2
j=4
library(dplyr)
for(i in 1:nrow(data.frame(unique(oData$Id)))){
  test3 <- oData[oData$Id == data.frame(unique(oData$Id))[i,1], ][!duplicated(oData[oData$Id == data.frame(unique(oData$Id))[i,1], ][,c("Mid10","First10")]),]
  y <- c()
  
  for(j in 1:21){ 
    fAddress <- filter(oData, oData$Id == test3[j,"Id"], 
                       grepl(paste(c(test3[j,"First10"],test3[j,"Mid10"]), collapse = "|"), oData$AddressLine1), 
                       oData$Region == test3[j,"Region"], 
                       oData$Country == test3[j,"Country"],
                       !oData$LocationId %in% c(y))
    
    
    if (nrow(fAddress) != 0){
      if ((nrow(fAddress) == max(table(fAddress$First10)) | nrow(fAddress) == max(table(fAddress$Mid10)))
          && (length(unique(fAddress[!(is.na(fAddress$City) | fAddress$City ==""),7 ])) <= 3 |
              length(unique(fAddress[!(is.na(fAddress$zip) | fAddress$zip ==""),8 ])) <= 3)){
        if (nrow(fAddress)<=1){
          newrow = data.frame(ID = max(fAddress$Id), 
                              Title = max(fAddress$Title), 
                              LocationId = fAddress[1,"LocationId"], 
                              AddressLine1 = fAddress[1,"AddressLine1"], 
                              Region = max(fAddress$Region),
                              Country = max(fAddress$Country), 
                              City = max(fAddress$City), 
                              zip = max(fAddress$zip),
                              dLocationId = c(""))
          y <- c(y,fAddress$LocationId)
        }else{
          fAddress <- fAddress[order(nchar(fAddress$AddressLine1),decreasing = TRUE),]
          dLocationIdList <-  fAddress[-1,]
          newrow = data.frame(ID = max(fAddress$Id), 
                              Title = max(fAddress$Title), 
                              LocationId = fAddress[1,"LocationId"], 
                              AddressLine1 = fAddress[1,"AddressLine1"], 
                              Region = max(fAddress$Region),
                              Country = max(fAddress$Country), 
                              City = max(fAddress$City), 
                              zip = max(fAddress$zip),
                              dLocationId = paste(dLocationIdList$LocationId,collapse = "-"))
          y <- c(y,fAddress$LocationId)
        }
        newTable <- rbind(newTable, newrow)
      }
      
      if (length(unique(fAddress[!(is.na(fAddress$City) | fAddress$City ==""),7 ]))>
          length(unique(fAddress[!(is.na(fAddress$zip) | fAddress$zip ==""),8 ]))){
        if(length(which(fAddress$City==test3[i,"City"]))>=length(which(fAddress$zip==test3[i,"zip"]))){
          fAddress <- filter(oData, oData$Id == test3[j,"Id"], 
                             grepl(paste(c(test3[j,"First10"],test3[j,"Mid10"]), collapse = "|"), oData$AddressLine1), 
                             oData$Region == test3[j,"Region"], 
                             oData$Country == test3[j,"Country"], 
                             oData$City == test3[j,"City"], !oData$LocationId %in% c(y))
        }else{
          fAddress <- filter(oData, oData$Id == test3[j,"Id"], 
                             grepl(paste(c(test3[j,"First10"],test3[j,"Mid10"]), collapse = "|"), oData$AddressLine1), 
                             oData$Region == test3[j,"Region"], 
                             oData$Country == test3[j,"Country"], 
                             oData$zip == test3[j,"zip"], !oData$LocationId %in% c(y))
        }
        if (nrow(fAddress) != 0){
          if (nrow(fAddress)<=1){
            newrow = data.frame(ID = max(fAddress$Id), 
                                Title = max(fAddress$Title), 
                                LocationId = fAddress[1,"LocationId"], 
                                AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = max(fAddress$Region),
                                Country = max(fAddress$Country), 
                                City = max(fAddress$City), 
                                zip = max(fAddress$zip),
                                dLocationId = c(""))
            y <- c(y,fAddress$LocationId)
          }else{
            fAddress <- fAddress[order(nchar(fAddress$AddressLine1),decreasing = TRUE),]
            dLocationIdList <-  fAddress[-1,]
            newrow = data.frame(ID = max(fAddress$Id), 
                                Title = max(fAddress$Title), 
                                LocationId = fAddress[1,"LocationId"], 
                                AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = max(fAddress$Region),
                                Country = max(fAddress$Country), 
                                City = max(fAddress$City), 
                                zip = max(fAddress$zip),
                                dLocationId = paste(dLocationIdList$LocationId,collapse = "-"))  
            y <- c(y,fAddress$LocationId)
          }        
          newTable <- rbind(newTable, newrow)
        }  
      }
      
      if (length(unique(fAddress[!(is.na(fAddress$City) | fAddress$City ==""),7 ]))<
          length(unique(fAddress[!(is.na(fAddress$zip) | fAddress$zip ==""),8 ]))){
        if(length(which(fAddress$City==test3[i,"City"]))<=length(which(fAddress$zip==test3[i,"zip"]))){
          fAddress <- filter(oData, oData$Id == test3[j,"Id"], 
                             grepl(paste(c(test3[j,"First10"],test3[j,"Mid10"]), collapse = "|"), oData$AddressLine1), 
                             oData$Region == test3[j,"Region"], 
                             oData$Country == test3[j,"Country"], 
                             oData$zip == test3[j,"zip"], !oData$LocationId %in% c(y))
        }else{
          fAddress <- filter(oData, oData$Id == test3[j,"Id"], 
                             grepl(paste(c(test3[j,"First10"],test3[j,"Mid10"]), collapse = "|"), oData$AddressLine1), 
                             oData$Region == test3[j,"Region"], 
                             oData$Country == test3[j,"Country"], 
                             oData$City == test3[j,"City"], !oData$LocationId %in% c(y))
        }
        
        if (nrow(fAddress) != 0){
          if (nrow(fAddress)<=1){
            newrow = data.frame(ID = max(fAddress$Id), 
                                Title = max(fAddress$Title), 
                                LocationId = fAddress[1,"LocationId"], 
                                AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = max(fAddress$Region),
                                Country = max(fAddress$Country), 
                                City = max(fAddress$City), 
                                zip = max(fAddress$zip),
                                dLocationId = c(""))
            y <- c(y,fAddress$LocationId)
          }else{
            fAddress <- fAddress[order(nchar(fAddress$AddressLine1),decreasing = TRUE),]
            dLocationIdList <-  fAddress[-1,]
            newrow = data.frame(ID = max(fAddress$Id), 
                                Title = max(fAddress$Title), 
                                LocationId = fAddress[1,"LocationId"], 
                                AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = max(fAddress$Region),
                                Country = max(fAddress$Country), 
                                City = max(fAddress$City), 
                                zip = max(fAddress$zip),
                                dLocationId = paste(dLocationIdList$LocationId,collapse = "-"))  
            y <- c(y,fAddress$LocationId)
          }        
          newTable <- rbind(newTable, newrow)
        }  
      }
      
      if (length(unique(fAddress[!(is.na(fAddress$City) | fAddress$City ==""),7 ])) ==
          length(unique(fAddress[!(is.na(fAddress$zip) | fAddress$zip ==""),8 ]))){
        fAddress <- filter(oData, oData$Id == test3[j,"Id"], 
                           grepl(paste(c(test3[j,"First10"],test3[j,"Mid10"]), collapse = "|"), oData$AddressLine1), 
                           oData$Region == test3[j,"Region"], 
                           oData$Country == test3[j,"Country"], 
                           oData$zip == test3[j,"zip"], 
                           oData$First10 == test3[j,"First10"],
                           oData$Mid10 == test3[j,"Mid10"],
                           !oData$LocationId %in% c(y))
        
        if (nrow(fAddress) != 0){
          if (nrow(fAddress)<=1){
            newrow = data.frame(ID = max(fAddress$Id), 
                                Title = max(fAddress$Title), 
                                LocationId = fAddress[1,"LocationId"], 
                                AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = max(fAddress$Region),
                                Country = max(fAddress$Country), 
                                City = max(fAddress$City), 
                                zip = max(fAddress$zip),
                                dLocationId = c(""))
            y <- c(y,fAddress$LocationId)
          }else{
            fAddress <- fAddress[order(nchar(fAddress$AddressLine1),decreasing = TRUE),]
            dLocationIdList <-  fAddress[-1,]
            newrow = data.frame(ID = max(fAddress$Id), 
                                Title = max(fAddress$Title), 
                                LocationId = fAddress[1,"LocationId"], 
                                AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = max(fAddress$Region),
                                Country = max(fAddress$Country), 
                                City = max(fAddress$City), 
                                zip = max(fAddress$zip),
                                dLocationId = paste(dLocationIdList$LocationId,collapse = "-"))  
            y <- c(y,fAddress$LocationId)
          }        
          newTable <- rbind(newTable, newrow)
        }  
      }	  
    }
  }
}

write.csv(newTable,"T3CompanyLocationFM10_R20.csv")

