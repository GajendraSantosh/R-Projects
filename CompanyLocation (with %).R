setwd('D:/Documents/R Projects/')
oData <- read.csv("CompanyLocation D23-406.csv", stringsAsFactors=FALSE)
oData <- oData[!(is.na(oData$AddressLine1) | oData$AddressLine1 == ""),
               c("Id", "LocationId", "Title", "AddressLine1", "Region", "Country", "City", "zip", "dLocationId", "First7", "Mid7", "pMatch", "Status")]
oData[is.na(oData)]<-""
#oData[,c("dLocationId","pMatch", "Status")] <- c("")
#oData$pMatch <- as.numeric(as.character(oData$pMatch))
newTable = data.frame(Id = numeric(),                 LocationId = numeric(), 
                      Title = character(),            AddressLine1 = character(), 
                      Region = character(),           Country = character(), 
                      City = character(),             zip = numeric(),
                      dLocationId = character(),      First7 = character(),
                      Mid7 = character(),             pMatch = numeric(),
                      Status = character())

uniqueID <- data.frame(unique(oData$Id))
colnames(uniqueID)= c("Id")
cols <- c(1,2,9,12)
y <- c()
i=28
j=1
k=1
library(dplyr)
library(tokenizers)
#nrow(uniqueID) #uniqueID[i,"Id"]

for (i in 1:nrow(uniqueID)) {
  filterIdUnique <- oData[oData$Id == uniqueID[i,"Id"], ]
  test3 <- filterIdUnique[!duplicated(filterIdUnique[,c("First7", "Mid7")]),]
  
  for (j in 1:nrow(test3)) { 
    fAddress <- filter(oData, oData$Id == test3[j,"Id"], 
                       grepl(paste(c(test3[j,"First7"],test3[j,"Mid7"]), collapse = "|"), oData$AddressLine1), 
                       oData$Region == test3[j,"Region"], 
                       oData$Country == test3[j,"Country"],
                       !oData$LocationId %in% c(y))
    
    if(nrow(fAddress) != 0) {
      if (nrow(fAddress)<=1) {
        newrow = data.frame(Id = fAddress[1,"Id"],                  LocationId = fAddress[1,"LocationId"], 
                            Title = fAddress[1,"Title"],            AddressLine1 = fAddress[1,"AddressLine1"], 
                            Region = fAddress[1,"Region"],          Country = fAddress[1,"Country"], 
                            City = fAddress[1,"City"],              zip = fAddress[1,"zip"],
                            dLocationId = c(""),                    First7 = fAddress[1,"First7"],
                            Mid7 = fAddress[1,"Mid7"],              pMatch = c(""),
                            Status = c("Done"))
        y <- c(y,fAddress$LocationId)
        fAddress[cols] <- lapply(fAddress[cols], factor)
        newTable <- rbind(newTable, newrow, fAddress)
      }else{
        for (k in 1:nrow(fAddress)) {
          fWord <- unlist(tokenize_words(fAddress[1,"AddressLine1"]))
          sWord <- unlist(tokenize_words(fAddress[k,"AddressLine1"]))
          fAddress[k,"pMatch"] = as.integer(length(fWord[fWord %in% sWord])/length(sWord)*100)
        }
        fAddress <- filter(fAddress, pMatch >= 50 )
          if (nrow(fAddress)<=1) {
            newrow = data.frame(Id = fAddress[1,"Id"],                  Title = fAddress[1,"Title"], 
                                LocationId = fAddress[1,"LocationId"],  AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = fAddress[1,"Region"],          Country = fAddress[1,"Country"], 
                                City = fAddress[1,"City"],              zip = fAddress[1,"zip"],
                                dLocationId = c(""),                    First7 = fAddress[1,"First7"],
                                Mid7 = fAddress[1,"Mid7"],              pMatch = c(""),
                                Status = c("Done"))
            y <- c(y,fAddress$LocationId)
            fAddress[cols] <- lapply(fAddress[cols], factor)
            newTable <- rbind(newTable, newrow, fAddress)
          }else{
            fAddress <- fAddress[order(nchar(fAddress$AddressLine1),decreasing = TRUE),]
            dLocationIdList <-  unlist(fAddress[-1,2])
            newrow = data.frame(Id = fAddress[1,"Id"],                  LocationId = fAddress[1,"LocationId"], 
                                Title = fAddress[1,"Title"],            AddressLine1 = fAddress[1,"AddressLine1"], 
                                Region = max(fAddress$Region),          Country = max(fAddress$Country), 
                                City = max(fAddress$City),              zip = max(fAddress$zip),
                                dLocationId = paste(dLocationIdList,collapse = "-"),
                                First7 = fAddress[1,"First7"],          Mid7 = fAddress[1,"Mid7"],              
                                pMatch = fAddress[1,"pMatch"],          Status = c("Done"))
            y <- c(y,fAddress$LocationId)
            fAddress[cols] <- lapply(fAddress[cols], factor)
            newTable <- rbind(newTable, newrow, fAddress )
          }
      }
    }
  }
}

fAddress <- filter(oData,!oData$LocationId %in% c(y))
    
 class(fAddress$pMatch)    
 
 uniqueID[1:100]   
    
oData <- oData[1:100,]   
write.csv(newTable,"CompanyLocation(D23-406).csv")
