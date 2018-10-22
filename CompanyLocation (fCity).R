
#setwd('T:/WMI/WMI Projects & Tender/Construction Project/2018/Process/Language/Team Folder/Chideesh/R Projects')
setwd('C:/Documents and Settings/gsantosh/My Documents/R Projects')
oData <- read.csv("CompanyLocation.csv", stringsAsFactors=FALSE)
loopTable <- oData[ ,c("Id","Region", "Country", "City", "FistFive", "MidFive")]
newTable = data.frame(ID = numeric(), 
                      Title = character(), 
                      LocationId = numeric(), 
                      AddressLine1 = character(), 
                      Region = character(),
                      Country = character(), 
                      City = character(), 
                      zip = numeric(),
                      dLocationId = character())

uniqueloopTable = data.frame(Id = numeric(), Region = character(), Country = character(), City = character(), FistFive = character(), MidFive = character())
uniqueID <- unique(loopTable$Id)
uniqueID100 <- uniqueID[1:100]
colnames(oData)

library(dplyr)
for(i in 1:6){
filterIdUnique <- loopTable[loopTable$Id == 1000010, ]
test2 <- filterIdUnique[!duplicated(filterIdUnique[,'MidFive']),]
test3 <- test2[!duplicated(test2[,'FistFive']),]
First_Mid <- c(test3[i,5]|test3[i,6])
fAddress <- filter(oData, oData$Id == test3[i,1], grepl(paste(First_Mid, collapse = "|"), oData$AddressLine1, oData$Region == test3[i,2], oData$Country == test3[i,3], oData$City == test3[i,4]))

if (nrow(fAddress)>1){
newrow = data.frame(ID = max(fAddress$Id), 
                    Title = max(fAddress$Title), 
                    LocationId = max(fAddress$LocationId), 
                    AddressLine1 = max(fAddress$AddressLine1), 
                    Region = max(fAddress$AddressLine1),
                    Country = max(fAddress$Country), 
                    City = max(fAddress$City), 
                    zip = max(fAddress$zip),
                    dLocationId = character())
}else{
  dLocationIdList <-  fAddress[-which(grepl(min(fAddress$AddressLine1), fAddress$LocationId)),]
  
  newrow = data.frame(ID = max(fAddress$Id), 
                      Title = max(fAddress$Title), 
                      LocationId = max(fAddress$LocationId), 
                      AddressLine1 = max(fAddress$AddressLine1), 
                      Region = max(fAddress$AddressLine1),
                      Country = max(fAddress$Country), 
                      City = max(fAddress$City), 
                      zip = max(fAddress$zip),
                      dLocationId = paste(dLocationIdList$LocationId,collapse = ","))  
}

newTable <- rbind(newTable, newrow)
}


x <- loopTable[1:5,]

min(x$Country)
x[,3]
which(grepl(min(x$Country), x$Country))
list(x$FistFive)-3
y <- x[-which(grepl(min(x$Country), x$Country)),]

list(y$FistFive)
paste(y$FistFive,collapse = ",")

paste(list(y$FistFive),collapse = ",")

