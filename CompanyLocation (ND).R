setwd('D:/Documents/R Projects/')
oData <- read.csv("OrrginalCompLocationFM_5-10.csv", stringsAsFactors=FALSE)
newTable = data.frame(Id = numeric(),                 LocationId = numeric(), 
                      Title = character(),            AddressLine1 = character(), 
                      Region = character(),           Country = character(), 
                      City = character(),             zip = numeric(),
                      dLocationId = character(),      First7 = character(),
                      Mid7 = character(),             pMatch = numeric(),
                      Status = character())

Idcount <- data.frame(table(oData$Id))
library(dplyr)
countId <- data.frame(filter(Idcount, Idcount$Freq %in% a))
countId_1 <- countId$Var1
newTable_1 <- filter(oData, oData$Id %in% countId_1)

write.csv(newTable_1,"CompanyLocation D11-22.csv")
library(tokenizers)
a <- unlist(tokenize_words("11	12	13	14	15	16	17	18	19	20	21	22	27	32"))

