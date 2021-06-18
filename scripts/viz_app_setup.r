
#Define custom scripts functions
#------------------------------
#Function that simplifies loading .RData objects
assignLoad <- function(filename){
  load(filename)
  get(ls()[ls() != "filename"])
}





#Items from Data Dictionary
######################################
#Load User Type sheet---
Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "User_Type_Code")
Eco_User_Type. <- Data_Dictionary..$User_Type
names(Eco_User_Type.) <- Data_Dictionary..$Eco_User_Type_Desc
Eco_User_Type. <- Eco_User_Type.[!(is.na(names(Eco_User_Type.)))] 
#Define additional user types codes since Eco Counter does not correctly define users when bike/ped collected together---
User_Type. <- Data_Dictionary..$User_Type
names(User_Type.) <- Data_Dictionary..$User_Type_Desc
User_Type. <- User_Type.[!(is.na(names(User_Type.)))] 
#Load Direction Code sheet---
Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "Direction")
#Create a direction description vector
Direction_Desc. <- Data_Dictionary..$Direction_Code
names(Direction_Desc.) <- Data_Dictionary..$Direction_Desc
#Load Collection Type Code sheet---
Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "Collection_Type_Code")
#Collection Type Codes---
Collection_Type. <- Data_Dictionary..$Collection_Type
names(Collection_Type.) <- Data_Dictionary..$Collection_Type_Desc
#Load Facility Type Code sheet---
Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "Facility_Type_Code")
#Create facility type desc vector
Facility_Type. <- Data_Dictionary..$Facility_Type
names(Facility_Type.) <-Data_Dictionary..$Facility_Type_Desc
#Load Facility Type Code sheet---
Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "Device_Type")
#Create device type desc vector
Device_Type. <- Data_Dictionary..$Device_Type
names(Device_Type.) <- Data_Dictionary..$Device_Type_Desc
#Load Error Code sheet---
Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "Error_Codes")
#Create device type desc vector
Error_Code. <- Data_Dictionary..$Error_Code
names(Error_Code.) <- Data_Dictionary..$Error_Name


#Load Data
#--------------------

#Step 3 Data ---------------------------------
All_Files. <- file.info(dir(paste(getwd(),"/Counts Data/Step 3 - Counts with Errors",sep=""), full.names=TRUE))$ctime
names(All_Files.) <- dir(paste(getwd(),"/Counts Data/Step 3 - Counts with Errors",sep=""), full.names=TRUE)
All_Files.<- All_Files.[!grepl(".csv", names(All_Files.))]

#Sub_Location_Id
##########
#Daily Sub Location---
File <- All_Files.[grep("Daily_Sub_Location_Id", names(All_Files.))]
File <- names(File[File%in%max(File)])
Load_Daily_Sub_Location_Id.. <- assignLoad(File)
#Hourly---
#File <- All_Files.[grep("Hourly_Sub_Location_Id", names(All_Files.))]
#File <- names(File[File%in%max(File)])
#Load_Hourly_Sub_Location_Id.. <- assignLoad(File)
#Select and rename data
Daily_Sub_Location_Id.. <- Load_Daily_Sub_Location_Id..[ Load_Daily_Sub_Location_Id..$Direction%in%"Total" & Load_Daily_Sub_Location_Id..$Obs_Hours%in%24,] 
#Create a location Id summary table
#############################
Daily_Location_Id.. <-   Daily_Sub_Location_Id.. %>% group_by(Device_Name, User_Type_Desc,Date) %>% summarise(Counts = sum(Counts,na.rm=T))
#Add columns
Daily_Location_Id..$Month <- months(Daily_Location_Id..$Date)
Daily_Location_Id..$Year <- year(Daily_Location_Id..$Date)
Daily_Location_Id..$Weekday <- weekdays(Daily_Location_Id..$Date)
Daily_Location_Id.. <- Daily_Location_Id.. %>%  mutate(Is_Weekday = ifelse(Weekday%in%c("Saturday","Sunday"),"Weekend","Weekday"))
#Error code - if any code is not 0 assign a new error falg
Error_Codes..  <- Daily_Sub_Location_Id.. %>% group_by(Device_Name,User_Type_Desc,Date) %>%
  summarise(Error_Code = case_when(
    all(Error_Code == 0) ~ 0,
    any(Error_Code == 1) ~ 1,
    any(Error_Code == 2) ~ 2,
    any(Error_Code == 3) ~ 3,
    any(Error_Code == 4) ~ 4,
    any(Error_Code == 5) ~ 5))
#Join error codes to dailky counts
Daily_Location_Id.. <- left_join(Daily_Location_Id.., Error_Codes.., by = c("Device_Name","User_Type_Desc","Date"))

#Append some columns
#############
#Descriptive of Error Code
Daily_Sub_Location_Id..$Error_Code_Desc <- names(Error_Code.)[match(Daily_Sub_Location_Id..$Error_Code, Error_Code.)]
Daily_Location_Id..$Error_Code_Desc <- names(Error_Code.)[match(Daily_Location_Id..$Error_Code, Error_Code.)]

#Load projection file from file or download if not on file
#Download spatial data for select counties for defining project 
if(!(any(list.files(paste(getwd(),"/Supporting Data/Spatial/Census/",sep=""))%in%"County_Spatial_Data.RData"))){
  County_Sp <- counties("Oregon")
  save(County_Spatial, file = paste(getwd(),"/Supporting Data/Spatial/Census/County_Spatial_Data.RData",sep=""))
} else {County_Sp <- assignLoad(file = paste(getwd(),"/Supporting Data/Spatial/Census/County_Spatial_Data.RData",sep=""))}


