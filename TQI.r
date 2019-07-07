install.packages("sqldf")
install.packages("plyr")
install.packages("formattable")
library(sqldf)
library(plyr)
library(formattable)
#Read the data dbnew.csv
db<-read.csv(file.choose(),header=T)
#To find the summary of the data
summary(db)
str(db)
n = as.integer(readline(prompt="Enter a milepost: "))
m = readline(prompt="Enter a parameter: ")
l=print(paste('a<-sqldf("SELECT', m ,'FROM db WHERE (milepost =',n,') AND (test_car= ', test') AND (track = SG) AND (prefix= XXB) )")'))

l2 = paste(paste(paste(paste(paste('a<-sqldf("SELECT', m, sep = " "), 'FROM db WHERE (milepost =\'', sep=" "), n, sep=""), '\')")', sep=''))
k=eval(parse(text=l2))
a<-sqldf("SELECT GAGE FROM db WHERE (CONTINUOUSMILEPOST = '660.63') AND (CAR= 'EC4')AND (TRACK = 'SG') AND (PREFIX='00K')")
print(a)
class(b<-unlist(a))
class(b)
d <-(as.character(b))
N2 <- gsub(",","",d)
c<-as.numeric(N2)
class(c)
c
sd(c, na.rm = TRUE)
mylist1<-list(c)
t<-lapply(mylist1,function(y)sqrt(1^2+(y*y)^2))
print(t)
class(t)
Lo=length(unlist(t))
print(t)
Ls = 0.00
for(i in t)
{
  Ls = Ls +i
  }
print (Ls)
#y <- formattable(Ls, digits = 3, format = "f")
Z<-sum(Ls,na.rm=TRUE)
Z
TQI =((Z)/57.86)-1
print(TQI)
#b<-as.numeric(unlist(a))
#print(b)
#sd(as.numeric(unlist(a)), na.rm = TRUE)
#mylist1<-list(a$GAGE)
#class(db$GAGE)
#b=sum(Ls)
#z=as.numeric(paste(e))
***********************************************************************************************************
# For the PROFILE62_RIGHT_M TRACK_SEGMENT_PREFIX_I (OTR)
a<-sqldf("SELECT gage FROM db WHERE (milepost = '34') AND (passenger_speed= '40' OR passenger_speed= '25'OR passenger_speed= '50')")
print(a)
mylist1<-list(a)
t<-lapply(mylist1,function(x)sqrt(1^2+x^2))
print(t)
Ls = 0.00
for(i in t)
{
  Ls = Ls +i
}
Lo =5280.00
print (Ls)

b=sum(Ls)
TQI =((sum(Ls)/Lo)-1)
print(TQI)
************************************************************************************************************
  # For the PROFILE62_RIGHT_M TRACK_SEGMENT_PREFIX_I (OTR)
a<-sqldf("SELECT gage FROM db WHERE (milepost = '37') AND (passenger_speed= '40' OR passenger_speed= '25'OR passenger_speed= '50')")
print(a)
mylist1<-list(a)
t<-lapply(mylist1,function(x)sqrt(1^2+x^2))
print(t)
Ls = 0.00
for(i in t)
{
  Ls = Ls +i
}
Lo =5280.00
print (Ls)
sum(Ls)
b=sum(Ls)
TQI =((sum(Ls)/Lo)-1)
print(TQI)
**************************************************************************************************************
a<-sqldf("SELECT gage FROM db WHERE (milepost = '36') AND (passenger_speed= '40' OR passenger_speed= '25'OR passenger_speed= '50')")
print(a)
mylist1<-list(a)
t<-lapply(mylist1,function(x)sqrt(1^2+x^2))
print(t)
Ls = 0.00
for(i in t)
{
  Ls = Ls +i
}
Lo =5280.00
print (Ls)
sum(Ls)
b=sum(Ls)
TQI =((sum(Ls)/Lo)-1)
print(TQI)
**************************************************************************************************************
a<-sqldf("SELECT gage FROM db WHERE (milepost = '36') AND (passenger_speed= '40' OR passenger_speed= '25'OR passenger_speed= '50')")
print(a)
mylist1<-list(a)
t<-lapply(mylist1,function(x)sqrt(1^2+x^2))
print(t)
Ls = 0.00
for(i in t)
{
  Ls = Ls +i
}
Lo =5280.00
print (Ls)
sum(Ls)
b=sum(Ls)
TQI =((sum(Ls)/Lo)-1)
print(TQI)
***************************************************************************************************************
a<-sqldf("SELECT gage FROM db WHERE (milepost = '35') AND (passenger_speed= '40' OR passenger_speed= '25'OR passenger_speed= '50')")
print(a)
mylist1<-list(a)
t<-lapply(mylist1,function(x)sqrt(1^2+x^2))
print(t)
Ls = 0.00
for(i in t)
{
  Ls = Ls +i
}
Lo =5280.00
print (Ls)
sum(Ls)
b=sum(Ls)
TQI =((sum(Ls)/Lo)-1)
print(TQI)
************************************************************************************************************
a<-sqldf("SELECT gage FROM db WHERE (milepost = '34') AND (passenger_speed= '40' OR passenger_speed= '25'OR passenger_speed= '50')")
print(a)
mylist1<-list(a)
t<-lapply(mylist1,function(x)sqrt(1^2+x^2))
print(t)
Ls = 0.00
for(i in t)
{
  Ls = Ls +i
}
Lo =5280.00
print (Ls)
sum(Ls)
b=sum(Ls)
TQI =((sum(Ls)/Lo)-1)
print(TQI)
***********************************************************************************************************

# mylist2<-list(0.73,0.46,0.36,0.28,0.03)
# k<-lapply(mylist2,function(x)sqrt(15^2+x^2))
# Ls = 0
# for(i in k)
# {
#   Ls = Ls +i
# }
# Lo =50.00
# print (Ls)
# TQI =((Ls/50.00)-1)
# print(TQI)
***************************************************************************************************
install.packages("sqldf")
library(sqldf)
sqldf("SELECT 
TRACK_SEGMENT_PREFIX_I, 
ENGINEER_MILEPOST_I, 
TRACK_TYPE_C, 
GEOGRMS_MEASUREMENT_D, 
GAGE_M, 
CROSSLEVEL_M, ALIGNMENT62_LEFT_M,
ALIGNMENT62_RIGHT_M,
PROFILE62_LEFT_M, 
PROFILE62_RIGHT_M, 
WARP31_M, 
WARP62_M, 
RAIL_CANT_LEFT_M, 
RAIL_CANT_RIGHT_M,
TEST_VEHICLE_C
FROM db Where TRACK_SEGMENT_PREFIX_I ='OTR' ")

