kdata <- read.csv("C:\\Users\\Shivam-pc\\Desktop\\spam.csv",header=TRUE)
str(kdata)
summary(kdata)
print(kdata)
head(kdata$ReportingDate,10)
kdata$ReportingDate <- strptime(kdata$ReportingDate, format="%d-%b-%y")

#create two new cols year and mon
kdata$ReportingYear <- as.numeric(format(kdata$ReportingDate, "%Y"))
kdata$ReportingMon <- as.numeric(format(kdata$ReportingDate, "%m"))
kdata$ReportingWeek <- as.numeric(format(kdata$ReportingDate, "%W"))
str(kdata)
attach(kdata)
aggdata <- aggregate(Unitsordered,by=list(Id,ReportingYear,ReportingWeek), FUN=sum, na.rm=TRUE)
detach(kdata)
str(aggdata)
head(aggdata, 20)
colnames(aggdata) <- c("Id","ReportingYear","ReportingWeek","Unitsordered")
head(aggdata,10)
tail(aggdata,10)
index <- seq(0,52,by=2)
colnames(aggdata) <- c("Id","ReportingYear","biweek","Unitsordered")
head(aggdata,10)
aggdata <- aggdata[order(aggdata$Id),]
head(aggdata,10)
attach(aggdata)
index <- seq(0,52,by=2)
kdata$biweek <- NA
for(i in index)
  {
  aggdata$biweek[aggdata$biweek == i] <- i 
  aggdata$biweek[aggdata$biweek == i+1] <- i 
}

detach(aggdata)
aggdata <- aggdata[order(aggdata$Id, aggdata$ReportingYear, aggdata$biweek),]
head(aggdata,10)
tail(aggdata,10)
str(aggdata)
aggdata$flag <- aggdata$Unitsordered >=1
mdata_selectedIDs <- aggregate(aggdata, by=list(Id),FUN = sum) 
mdata_selectedIDs <- aggregate(aggdata, by=list(aggdata$Id),FUN = sum) 
str(mdata_selectedIDs)
mdata_selectedIDs <- aggregate(aggdata$flag, by=list(aggdata$Id),FUN = sum)
str(mdata_selectedIDs)
colnames(mdata_selectedIDs) <- c("Id", "NonzeroRecords")
mdata_selectedIDsTop <- mdata_selectedIDs[mdata_selectedIDs$NonzeroRecords>=38,]
head(mdata_selectedIDsTop)
mdata_selectedIDsTop
mdata_selectedIDs <- aggregate(aggdata$flag, by=list(aggdata$Id),FUN = sum)
mdata_lenghtIDs <-  aggregate(aggdata$flag, by=list(aggdata$Id),FUN = length)
colnames(mdata_selectedIDs) <- c("Id", "NonzeroRecords")
colnames(mdata_lenghtIDs) <- c("Id", "AllRecords")
length(mdata_selectedIDsTop)
count(mdata_selectedIDsTop)
