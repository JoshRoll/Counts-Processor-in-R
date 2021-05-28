#Author: Josh Roll Oregon Department of Transportation Research Unit
#Date: 4/24/2018
#Description: Process bike/ped and vehicle counts from the Unified Oregon Access Eco-Visio Platform



	
#Set up console conditions
#------------------------
	#Characters as factors will create problem for API so turn this off
	options(stringsAsFactors = FALSE)
	
#Load custom functions
#--------------------------
	#Load processing functions
	source("Scripts/functions.r")
	attach(Functions,warn.conflicts=FALSE)

	#Load Parameters file
	#####################################
	Parameters.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "Parameters")
	
#Load Data Dictionary Definitions
#--------------------------
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
		
#Load Stored data
#--------------------------------------
	#Counts Data 
	###################
	#Load latest file
	#Determine latest write off
	Files. <- file.info(dir(paste("Counts Data/Step 1 - Raw Counts",sep=""), full.names=TRUE))$ctime
	names(Files.) <- dir(paste("Counts Data/Step 1 - Raw Counts",sep=""), full.names=TRUE)
	Files. <- Files.[agrep(".RData",names(Files.))]
	Raw_Count_File <- names(Files.[Files.%in%max(Files.)])
	Counts_Data.. <- assignLoad(Raw_Count_File)
	#Supporting Data
	###################
	#Locate latest files
	All_Files. <- file.info(dir(paste("Supporting Data/API Call Summary Information",sep=""), full.names=TRUE))$ctime
	names(All_Files.) <- dir(paste("Supporting Data/API Call Summary Information",sep=""), full.names=TRUE)
	All_Files. <- All_Files.[agrep(".RData",names(All_Files.))]
	#Site information form API Call----
	Select_Files. <- All_Files.[grep("Site_Location", names(All_Files.))]
	File <- names(Select_Files.[Select_Files.%in%max(Select_Files.)])
	Site_Location_Info.. <- assignLoad(file = File)
	#Channel Information--- 
	Select_Files. <- All_Files.[grep("Channel", names(All_Files.))]
	File <- names(Select_Files.)[Select_Files.%in%max(Select_Files.)]
	Channel_Info..  <- assignLoad(file = File)
	
#Prepare data
#---------------------------
	#Determine which sites are permanent by looking at sites with a recorded latitude and Longitude and that have records
	#Latitude/Longitude---
	Site_Location_Info.. <- Site_Location_Info..[!(is.na(Site_Location_Info..$Latitude)),]
	#Have more than 0 records---
	#Site_Location_Info.. <- Site_Location_Info..[Site_Location_Info..$Record_Num > 
	
	#Initiate an index for each site based on the lat/long and remove duplicates
	Site_Location_Info..$LatLong_Index. <- paste(Site_Location_Info..$Latitude, Site_Location_Info..$Longitude, sep=" ")
	LatLong_Index_Count. <- table(Site_Location_Info..$LatLong_Index.)
	Site_Location_Info..$LatLong_Count <- LatLong_Index_Count.[match(Site_Location_Info..$LatLong_Index., names(LatLong_Index_Count.))]

	######################################################
	#Process permanent count data 
	######################################################
	#Collection type designation
	#Define known Portable Counting Device Serial Number
	Portable_Name. <- c("BendTUBE538", "BendMULTI547", "BendTUBE539", "BendTUBE541",  "BendMULTI549", "BendMULTI550", "BendMULTI545", "BendMULTI546", "BendTUBE542",  "BendMULTI548",
		"BendTUBE540",  "BendMULTI543", "BendMULTI544","BendMULTI551","BendMULTI552")
	Counts_Data..$Collection_Type_Desc <- "Permanent"
	Counts_Data..$Collection_Type_Desc[Counts_Data..$Device_Name%in%Portable_Name.] <- "Mobile"
	
	#If the site has an lat and long consider it a permanent site
	Perm_Data.. <- Counts_Data..[Counts_Data..$Collection_Type_Desc%in%"Permanent",]
	#Add direction by searching using regular expressions
	Perm_Data..$Direction <- NA
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='EAST', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "East"
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='WEST', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "West"
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='NORTH', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "North"
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='SOUTH', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "South"
	#Try other search patterns
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='EB', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "East"
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='WB', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "West"
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='NB', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "North"
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='SB', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "South"
	#Categorizae in/out
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='IN', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "In"
	Perm_Data..$Direction[unlist(lapply(str_locate_all(pattern ='OUT', toupper(Perm_Data..$Channel_Name)), length)) > 0 ] <- "Out"
	
	#Summarize by Site ID into hourly counts
	#########################################
	#Site_Id---
	#Summarize Hourly counts by Vendor_Site_Id
	Hourly_Site_Id.. <- Perm_Data.. %>%
		group_by(Device_Name, Vendor_Site_Id, Date, Hour, User_Type_Desc, Direction) %>%
		summarise(Counts = sum(Counts))
	#Sum hourly directions for a total 
	Temp.. <-  Perm_Data.. %>%
		group_by(Device_Name, Vendor_Site_Id, Date, Hour, User_Type_Desc) %>%
		summarise(Counts = sum(Counts))
	#Add direction column for total
	Temp.. <- mutate(Temp.., Direction = "Total")
	#Append with directional data
	Hourly_Site_Id..  <- bind_rows(Hourly_Site_Id.. , Temp..)	
	#Count and add the number of hourly observations
	ObsHours_Site_Id.. <- Hourly_Site_Id.. %>%
		group_by(Device_Name, Vendor_Site_Id, Date, User_Type_Desc, Direction) %>%
		summarise(Obs_Hours = length(Counts))
	Hourly_Site_Id.. <- left_join(Hourly_Site_Id.., ObsHours_Site_Id.., by = c("Device_Name", "Vendor_Site_Id", "Date", "User_Type_Desc", "Direction"))
	#Make copy to stream line adding attribute
	Temp.. <- Hourly_Site_Id..
	#Apply daily characteristics to Daily Summary
	Temp.. <- mutate(Temp.., Weekday = weekdays(Date))
	Temp.. <- mutate(Temp.., Is_Weekday = "Weekday")
	Temp.. <- mutate(Temp.., Is_Weekday = ifelse(Weekday%in% c("Saturday","Sunday"), "Weekend", Is_Weekday))
	Temp.. <- mutate(Temp.., Month  = months(Date))
	Temp.. <- mutate(Temp.., Year  = as.character(year(Date)))
	#Combine permanent counts with portable counts
	Hourly_Site_Id.. <- Temp.. 		
	
	#Summarize by Site ID into daily counts
	#########################################
	#Summarize Daily counts by Vendor_Site_Id ----
	Daily_Site_Id.. <- Perm_Data.. %>%
		group_by(Device_Name, Vendor_Site_Id, Date, Direction, User_Type_Desc) %>%
		summarise(Counts = sum(Counts))
	#Summarize but without direction to aggregate each directional flow
	Temp.. <- Perm_Data.. %>%
		group_by(Device_Name, Vendor_Site_Id, Date, User_Type_Desc) %>%
		summarise(Counts = sum(Counts))
	#Add direction column for total
	Temp.. <- mutate(Temp.., Direction = "Total")
	#Combine each direction data with total direction
	Daily_Site_Id..  <- bind_rows(Daily_Site_Id.. , Temp..)	
	#Append number of hours for each daily record
	Daily_Site_Id.. <- left_join(Daily_Site_Id.., ObsHours_Site_Id..,by = c("Device_Name", "Vendor_Site_Id", "Date", "User_Type_Desc", "Direction"))
	#Make copy to stream line adding attribute
	Temp.. <- Daily_Site_Id..
	#Apply daily characteristics to Daily Summary
	Temp.. <- mutate(Temp.., Weekday = weekdays(Date))
	Temp.. <- mutate(Temp.., Is_Weekday = "Weekday")
	Temp.. <- mutate(Temp.., Is_Weekday = ifelse(Weekday%in% c("Saturday","Sunday"), "Weekend", Is_Weekday))
	Temp.. <- mutate(Temp.., Month  = months(Date))
	Temp.. <- mutate(Temp.., Year  = as.character(year(Date)))
	Daily_Site_Id.. <- Temp..
	
	#Calculate hourly proportions for hourly counts 
	######################
	#Site ID ---
	#Add daily counts to hourly counts records
	#rename counts top Daily Counts
	y <- mutate(Daily_Site_Id.., Daily_Counts = Counts)
	Join_Vector. <- c("Date", "Device_Name","Vendor_Site_Id","User_Type_Desc","Direction","Daily_Counts")
	y <- y %>% select(Join_Vector.)
	#Add Daily Counts
	Hourly_Site_Id.. <- left_join(Hourly_Site_Id.. ,y, by = Join_Vector.[1:length(Join_Vector.)-1])
	#Calculate proportion of total daily..
	Hourly_Site_Id.. <- mutate(Hourly_Site_Id.., Hour_Pct = round(Counts / Daily_Counts, 4))
	#Statwide counts are coming in without the Sub_Location_Id column
	Daily_Site_Id..$Sub_Location_Id <- Daily_Site_Id..$Vendor_Site_Id
	Hourly_Site_Id..$Sub_Location_Id <- Hourly_Site_Id..$Vendor_Site_Id
	#Daily
	Daily_Site_Id..$Collection_Type_Desc <- "Permanent"
	Daily_Site_Id..$Collection_Type_Desc[Daily_Site_Id..$Device_Name%in%Portable_Name.] <- "Mobile"
	#Hourly
	Hourly_Site_Id..$Collection_Type_Desc <- "Permanent"
	Hourly_Site_Id..$Collection_Type_Desc[Hourly_Site_Id..$Device_Name%in%Portable_Name.] <- "Mobile"
	#Define holidays 
	Holidays. <-  c("USNewYearsDay","USInaugurationDay","USMLKingsBirthday","USMemorialDay","USIndependenceDay","USLaborDay","USVeteransDay","USThanksgivingDay","USChristmasDay")
	Holiday_Dates.  <-  as.Date(dates(as.character(holiday(as.numeric(unique(Daily_Site_Id..$Year)),Holidays.)),format ="Y-M-D"),format = "%m/%d/%Y")
	#Append to daily and hourly data sets
	#Daily
	Daily_Site_Id..$Is_Holiday <- FALSE
	Daily_Site_Id..$Is_Holiday[Daily_Site_Id..$Date%in%Holiday_Dates.] <- TRUE
	#Hourly
	Hourly_Site_Id..$Is_Holiday <- FALSE
	Hourly_Site_Id..$Is_Holiday[Hourly_Site_Id..$Date%in%Holiday_Dates.] <- TRUE
		
	#Delete data with obvious problems
	##################################
	#Daily---
	Daily_Site_Id.. <- Daily_Site_Id..[!(Daily_Site_Id..$Year%in%"1970"),]
	Daily_Site_Id.. <- Daily_Site_Id..[nchar(Daily_Site_Id..$Year) ==4,]
	#Hourly---
	Hourly_Site_Id.. <- Hourly_Site_Id..[!(Hourly_Site_Id..$Year%in%"1970"),]
	Hourly_Site_Id.. <- Hourly_Site_Id..[nchar(Hourly_Site_Id..$Year) ==4,]
	
	######################################
	#Write out processed data to file
	######################################
	print("Storing processed hourly and daily data.  If .csv format is selected this will take a few minutes longer.....")
	#Determine if .csv files are desired - this takes a while for reading/writing count data
	#RData files
	save(Hourly_Site_Id.., file = paste("Counts Data/Step 2 - Processed Counts/Hourly_Site_Id_",nrow(Hourly_Site_Id..),".RData",sep=""))
	save(Daily_Site_Id.., file = paste("Counts Data/Step 2 - Processed Counts/Daily_Site_Id_",nrow(Daily_Site_Id..),".RData",sep=""))
	
	
	#Determine if .csv files are desired - this takes a while for reading/writing count data
	Write_Csv <- as.logical(Parameters..$Value[Parameters..$Parameter%in%"Write_Csv"])
	#Report to user is Csvs are being written
	if(Write_Csv){print("Processed data (Step 2) file output will include .csv formatted files.")} else {print("Processed data (Step 2) file output will include .RData format only.")}
	if(Write_Csv){
		write.csv(Hourly_Site_Id.., file = paste("Counts Data/Step 2 - Processed Counts/Hourly_Site_Id_",nrow(Hourly_Sub_Location_Id..),".csv",sep=""))
		write.csv(Daily_Sub_Location_Id.., file = paste("Counts Data/Step 2 - Processed Counts/Daily_Site_Id_",nrow(Daily_Sub_Location_Id..),".csv",sep=""))
	}	
	 

	 
	