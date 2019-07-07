##Installing sqldf package##
install.packages("sqldf")

##Importing the library##
library(sqldf)

#Installing the dplyr package for summarizing the data into per year level
install.packages("dplyr")

###Importing the library  ##
library(dplyr)


##Getting the working Directory##
getwd()


##Setting the working directory##
setwd("H:/Shivam")
y<-list.files("H:/Shivam")


##Reading multiple files present in the working directory##
for (t in 1:length(y))


{

## Select one individual file##  
db<-read.csv(file.choose(),header=T)

##Selecting the multiple files in R code##
#db<-read.csv(y[t],header=T)

}

##To find the summary and structure of the data##
summary(db)
str(db)




# mp_case2 = unique(db4[,c("DIVISION", "SUBDIVISION", "PREFIX", "MILEPOST", "TRACK")])
# 
# for (i in 1:nrow(mp_case2))
#   
# 
# {
# 
# #temp = which(db$Diivision==mp_case2[i,c("Diivision")] & db$Track==mp_case2[i,c("Track")] & db$Prefix==mp_case2[i,c("Prefix")] & db$Milepost==mp_case2[i,c("Milepost")] & db$Subdivision==mp_case2[i,c("Subdivision")] )
# temp = which(db$DIVISION==mp_case2[i,c("DIVISION")] & db$TRACK==mp_case2[i,c("TRACK")] & db$PREFIX==mp_case2[i,c("PREFIX")] & db$MILEPOST==mp_case2[i,c("MILEPOST")] & db$SUBDIVISION==mp_case2[i,c("SUBDIVISION")] )  
#   
  
##Creating an Array with no of Unique Milepost##
arr1=unique(db$CONTINUOUSMILEPOST)

###arr1=unique(t3_geo_detail_swp_valid.milepost)
arr1

##Definining an Dataframe for storing the TQI vcalues##
arr2 = data.frame(CONTINUOUSMILEPOST=numeric(),GAGE=numeric(),CROSSLEVEL=numeric(),PROFILERIGHT=numeric(),PROFILELEFT=numeric()) 
arr2[length(arr1),5]=NA

#arr2 = list()


# arr2 = data.frame(V1=integer())
# arr2[1, 1] = NA
# arr2[1, 2] = NA
# arr2[1, 3] = NA
# arr2[1, 4] = NA
# arr2[1, 5] = NA
# arr2[1, 6] = NA
# col_name_arr2 = c("CONTINUOUSMILEPOST", "GAGE", "CROSSLEVEL", "PROFILERIGHT", "PROFILELEFT", "COMBINEDTQI")
# names(arr2) = col_name_arr2

#Defining the dataframe for Combined TQI##
COMBINEDTQI = data.frame() 
COMBINEDTQI[length(arr1),1] = NA

#Defining the four channels in a list
f<-list("GAGE","CROSSLEVEL","PROFILERIGHT","PROFILELEFT")
#f<-list("t3_geo_detail_swp_valid.gage","t3_geo_detail_swp_valid.cross_level","t3_geo_detail_swp_valid.left_profile","t3_geo_detail_swp_valid.right_profile")
  

#Outer for loop for each Milepost##  
for (i in 1:length(arr1))
  { 
  
  arr2[i,1] = arr1[i]

##Inner for loop for going through each channel for unique Milepost##
  for(l in 1:length(f))
    
        
    {
#Selecting the data from the file for each UNIQUE MILEPOST, TRACK SEGMENT PREFIX, CAR and TRACK##    
  tp= paste0(" SELECT ",f[l]," FROM db WHERE (CONTINUOUSMILEPOST = ",arr1[i]," ) AND (CAR= 'EC4')AND (TRACK = 'SG') AND (PREFIX='00K')")
  #tp= paste0("SELECT PROFILERIGHT FROM db WHERE (CONTINUOUSMILEPOST = ",arr1[i]," ) AND (CAR= 'EC4')AND (TRACK = 'SG') AND (PREFIX='00K')")
  ##tp= paste0(" SELECT ",f[l]," FROM db WHERE (t3_geo_detail_swp_valid.milepost = ",arr1[i]," ) AND (t3_geo_detail_swp_valid.test_car= 'TGC3')AND (t3_geo_detail_swp_valid.track = 'SG') AND (t3_geo_detail_swp_valid.prefix='BAK')")
  a=(sqldf(tp))



#sqldf("SELECT PROFILERIGHT FROM db WHERE (CONTINUOUSMILEPOST = 672.99 AND (CAR= 'EC4') AND (TRACK = 'SG') AND (PREFIX='00K'))")

#a<-sqldf("SELECT PROFILERIGHT FROM db WHERE (CONTINUOUSMILEPOST = 672.94 ) AND (CAR= 'EC4')AND (TRACK = 'SG') AND (PREFIX='00K')")
#print(a)

b<-unlist(a)
#print(b)
class(b)
d <-(as.character(b))
#print(d)
N2 <- gsub(",","",d)
#print(N2)
c<-as.numeric(N2,na.rm = TRUE)
#class(c)
#print(c)
k<-na.omit(c)


##Converting measurement values from Inches into feet## 
e<-(k/12)
#print(e)


##Taking Square of the Y Values##
newData<- sapply(e, function(x) x^2)
#print(newData)

##Taking Difference of Square of the Y Values##
DeltaofYSquare<- diff(newData, differences = 1 )
#print(DeltaofYSquare)
#class(DeltaofYSquare)

##Storing the DeltaofYSquare in a list##
DeltaofYSquare1<-list(DeltaofYSquare)

##Defining DeltaX per Feet value##  
DeltaX<-1

##Computing the Square of DeltaofYSquare##
t<-lapply(DeltaofYSquare1,function(DeltaofYSquare)sqrt((DeltaX)^2+(DeltaofYSquare)^2))
class(t)

z<-unlist(t)
class(z)
n<-sprintf('%.20f',z)
p <-(as.character(n))
N3 <- gsub(",","",p)
q<-as.numeric(N3,na.rm = TRUE)


Ls = 0.00

##Calculating Ls (Traced Length of Space Curve)##
for(j in q)
{
  Ls = Ls +j
}
#print (Ls)

##Taking sum of Ls##
sum(Ls)

##Defining Lo (Fixed length of Track Segment,Feet)##
Lo =length(e)


##Calculating TQI##
TQI =((sum(Ls)/Lo)-1)

arr2[i,(l+1)] = TQI


#print(TQI)
#Results = rbind(arr2, data.frame(CONTINUOUSMILEPOST,GAGE,CROSSLEVEL,PROFILERIGHT,PROFILELEFT))

  }
COMBINEDTQI[i,1]= ((0.2*arr2[i,(l-3)] ) +(0.2*arr2[i,(l-2)])+(0.025*arr2[i,(l-1)])+(0.025*arr2[i,(l)]))/(0.45)

}


##Binding the column of Combined TQI to the arr2##
arr2<-cbind(arr2,COMBINEDTQI)



# Load RODBC package
# install.packages("RODBC")
# library(RODBC)
# Check that connection is working (Optional)
# channel <- odbcConnect("TMSP", uid="TPRS_USER", pwd="TPRS_USER", believeNRows=FALSE)
# Tables <- sqlTables(channel, schema="TPRS_USER")

##Reading the File for the VTI##
VTI<-read.csv(file.choose(),header=T)
FORMATING_PER_YEAR_VTI<-format(EXCEPTION_D,'%Y')
CONVERTING_TO_NUMERIC_VTI<-as.numeric(FORMATING_PER_YEAR)
db<-cbind(VTI,CONVERTING_TO_NUMERIC)
gb = group_by(db,EXCEPTION_LOCATION_MP_I,CONVERTING_TO_NUMERIC)
result_VTI <- summarize(gb, count = n())
#result_VTI
db<-cbind(db,result_VTI)

##Reading the File for the GEOMETRIC_EXCEPTION##
GEOMETRIC_EXCEPTION<-read.csv(file.choose(),header=T)
FORMATING_PER_YEAR_GE<-format(EXCEPTION_D,'%Y')
CONVERTING_TO_NUMERIC_GE<-as.numeric(FORMATING_PER_YEAR_GE)
db<-cbind(GEOMETRIC_EXCEPTION,CONVERTING_TO_NUMERIC_GE)
gb = group_by(db,EXCEPTION_LOCATION_MP_I,CONVERTING_TO_NUMERIC_GE)
result_GEOMETRIC_EXCEPTION <- summarize(gb, count = n())
#result_GEOMETRIC_EXCEPTION
db<-cbind(db,result_GEOMETRIC_EXCEPTION)

##Binding the column together##
arr2<-cbind(arr2,db)

##Writing the data into the .csv file##
write.csv(arr2,"db100.csv")


## Writing the results into  different .csv files##
# write.csv(arr2,paste(paste("db", t), ".csv"))


# write.csv(arr2,"db100.csv")


--------------------------------------------------------------------------------

