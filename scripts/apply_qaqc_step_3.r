#Author: Josh Roll Oregon Department of Transportation Research Unit
#Date: 2/28/2018
#Description: This script loads pre-processed data and applies a process to flag data with likely problems.
#Accomplishes:
#1 - Clustering and finding common excessive values due to special events
#2 - Consecutive zeros 
#3 - excessive values
#4 - Splits user counts at select locations, into bicycle and pedestrian specific counts
#5 - Suspicious flag added

#Version < 0.0
#- Use simple approach to determine if counter were within expected rolling average cound with some slack (e.g. 0.25)
#Version 1.0
#- Use climate data to estimate seasonal adjustment models (SARM)and use confidence intervals for defining bounds
#- For large outliers:
#------ look at clustering of days where outliers exist across count locations
#------ look at federal holidays
#------ look across count locations to see if similar changes occured
#- Implement way to id outliers by applying a rolling average by weekend/weekday designation
#- Flag consecutive zeros
#- Remove the SARM method and applying the rolling average to all applicable counts
#Added process for doing same flagging process to Location_ID level data
#Updated for delivery to Bend MPO
#Add user feedback for script wrapper
#Added splitting functionality for user data where for locations with observed bike and ped data exist.  These splits are shown in a pdf for review and a flag called "Est_Split" is applied to those daily counts that are
#derived from this estimation process

#Version Notes:
#Version 1.1
#Added hard coding of known sites with errors with new 'Suspicious' data flag (numeric 5)
#Cleanup of old code
#Version 1.2 
#Changed the potential event flag form the top 90th percetnile to 95th percentile
#Version 1.3 
#Move the user split funcitonality to after the effor flagging and now don;t use records with suspected errors in splitting
#Version 1.4 
#Move harded coded values to Parameters.csv input file stored at /Parameters/
#Append lower/upper bound rolling mean values to output data for easier/unified calculating.
#Version 1.6
#Removed the Location_Id processed and are now just doing the Sub_Location_Id process and aggregating to location
#Added an input file to manual flag known bad data

#Report process to user
print("Starting data splitting and error flagging step.  This process will split user data into bike/ped where information allows followed by assignment of error flags to processed data.")


	
#Set up console conditions
#------------------------
	#Characters as factors will create problem for API so turn this off
	options(stringsAsFactors = FALSE)
	#Set working directory (uncomment if running as stand alone script)
	#setwd("F:/Data/Counts/Non Motorized/Bend MPO")


#Load functions
#--------------------------
	#Load processing functions
	source("Scripts/Functions/Functions.r")
    attach(Functions,warn.conflicts=FALSE)
		
#Create script default vectors
#-----------------------------
	#Load Parameters file
	#####################################
  Parameters.. <- 	read_excel("Documentation/Data Dictionary.xlsx", sheet = "Parameters")

	#Items from Data Dictionary
	######################################
	#Load User Type sheet---
	Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "User_Type_Code")
	User_Type. <- Data_Dictionary..$User_Type
	names(User_Type.) <- Data_Dictionary..$User_Type_Desc
	User_Type. <- User_Type.[!(is.na(names(User_Type.)))] 
	#Error Codes
	#------------------------
	#0	Valid
	#1	Consecutive Zeros
	#2	Rolling Mean
	#3	Excessive Value
	#4	Manual Error Check
	#5	Suspicious
	Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "Error_Codes")
	Error_Code. <- Data_Dictionary..$Error_Code
	names(Error_Code.) <- Data_Dictionary..$Error_Name
	
	#Load Stored processed counts data 
	######################
	#Hourly
	#Sub Location Id---
	All_Files. <- file.info(dir(paste(getwd(),"/Counts Data/Step 2 - Processed Counts",sep=""), full.names=TRUE))$ctime
	names(All_Files.) <- dir(paste(getwd(),"/Counts Data/Step 2 - Processed Counts",sep=""), full.names=TRUE)
	All_Files. <- All_Files.[agrep(".RData",names(All_Files.))]
	File <- All_Files.[grep("Hourly_Site_Id", names(All_Files.))]
	File <- names(File[File%in%max(File)])
	Load_Hourly_Sub_Location_Id.. <- assignLoad(File)
	#Daily
	#Sub Location Id---	
	File <- All_Files.[grep("Daily_Site_Id", names(All_Files.))]
	File <- names(File[File%in%max(File)])
	Load_Daily_Sub_Location_Id.. <- assignLoad(File)
	
	#Load count location spatial data
	######################
	
  #Load site infomration
	#################
	#Supporting Data
	###################
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
	#Latitude/Longitude---
	Site_Location_Info.. <- Site_Location_Info..[!(is.na(Site_Location_Info..$Latitude)),]
	#Convert cities with NAs to "Rural" designation
	Site_Location_Info..$City[is.na(Site_Location_Info..$City)] <- "Rural"

#Sub_Location_Id Error Flagging
#-----------------------------------------
	#Provide user report
	print("Starting error checks for Sub_Location_Id level data")
	#Log start time for reporting progress
	Start_Time <- Sys.time()
	#Make a copy 
	Daily_Sub_Location_Id.. <- Load_Daily_Sub_Location_Id..
	
	#initialize a column to store error codes
	Daily_Sub_Location_Id..$Error_Code <- 0
	#Create a cloumn for potential special events
	Daily_Sub_Location_Id..$Potential_Special_Event <- 0
	
	#Convert all NAs to zeros
	Daily_Sub_Location_Id..$Counts[is.na(Daily_Sub_Location_Id..$Counts)] <- 0
	#Initialize an index based on date and sub_location name
	Names. <- unique(paste(Daily_Sub_Location_Id..$Sub_Location_Id, Daily_Sub_Location_Id..$Date, Daily_Sub_Location_Id..$User_Type_Desc, sep="-"))
	Index. <- 1:length(Names.)
	names(Index.) <- Names.
	Daily_Sub_Location_Id..$Index <- 0
	Daily_Sub_Location_Id..$Index <- Index.[match(paste(Daily_Sub_Location_Id..$Sub_Location_Id, Daily_Sub_Location_Id..$Date, Daily_Sub_Location_Id..$User_Type_Desc, sep="-"),names(Index.))]
	
	#Intitialize lower and upper bound
	Daily_Sub_Location_Id..$Lb_Conf_Bound <- NA
	Daily_Sub_Location_Id..$Ub_Conf_Bound <- NA
	
	#Remove data with undefeined users
	Daily_Sub_Location_Id.. <- Daily_Sub_Location_Id..[!(Daily_Sub_Location_Id..$User_Type_Desc%in%c("Horse","Undefined")),]
	#Fix some other issues manually
	Daily_Sub_Location_Id..$User_Type_Desc[Daily_Sub_Location_Id..$Device_Name%in%"Fern Ridge Path west of Chambers - Users"] <- "User"
	
	table(Daily_Sub_Location_Id..$Device_Name[is.na(Daily_Sub_Location_Id..$User_Type_Desc)])
	#Manual Error Flagging - This process loads an input file with user specified dates, location id, and user and facility type. 
	#-----------------------
	#Load manual error flag records
	#Check to make sure file exists
	if(!(file.exists("Supporting Data/Error Flags/Manual Error Tracking.csv"))){print("No 'Manual Error Tracking.csv' file found at 'Supporting Data/Error Flags/'.  Read how to guide and please update.")}
	Manual_Error_Flags.. <- read.csv("Supporting Data/Error Flags/Manual Error Tracking.csv")
	#Format input start and end date
	Manual_Error_Flags..$Start_Date <- as.Date(Manual_Error_Flags..$Start_Date, format = "%m/%d/%Y")
	Manual_Error_Flags..$End_Date <- as.Date(Manual_Error_Flags..$End_Date, format = "%m/%d/%Y")
	if(nrow(Manual_Error_Flags..) >0){
	for(i in 1:nrow(Manual_Error_Flags..)){
		#Specify each element of the record flagging
		Start_Date <- Manual_Error_Flags..$Start_Date[i]
		End_Date <- Manual_Error_Flags..$End_Date[i]
		Dates. <- seq(as.Date(Start_Date),as.Date(End_Date),1)
		Sub_Location_Id <- Manual_Error_Flags..$Sub_Location_Id[i]
		Facility_Type <- Manual_Error_Flags..$Facility_Type[i]
		User <- Manual_Error_Flags..$User_Type[i]
		#APply to records
		Daily_Sub_Location_Id..$Error_Code[(Daily_Sub_Location_Id..$Sub_Location_Id%in%Sub_Location_Id & Daily_Sub_Location_Id..$Date%in%as.Date(Dates.) & 
			Daily_Sub_Location_Id..$Facility_Type%in%Facility_Type & Daily_Sub_Location_Id..$User_Type_Desc%in%User)]  <- 5
	}
	}
	#Do a precheck of potential events days. This process determines if days with the highest 95th percentile counts at a given location occur at other locations and if so will flag that 
	#day as a potential special event.  The day for a given location must also be found to be in the 95th percentile at 3 other locations
	#----------------------------------------
	Temp.. <- Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Obs_Hours%in%24 & Daily_Sub_Location_Id..$Direction%in%"Total",]
	#Temp.. <- Temp..[!(Temp..$User_Type_Desc%in%"Vehicle"),]
	#Gather days in the top decile
	Top_Decile_Store.. <- data.frame()
	
	##########################
	for(sub_location_id in unique(Daily_Sub_Location_Id..$Sub_Location_Id)){
	  #Do each user type separatley
	  for(user in unique(Daily_Sub_Location_Id..$User_Type_Desc[Daily_Sub_Location_Id..$Sub_Location_Id%in%sub_location_id])){
	    #Select counts (not using filter() because it seems slower)
	    Data_Select_1.. <- Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Sub_Location_Id%in%sub_location_id & Daily_Sub_Location_Id..$Direction %in%"Total" & 
	                             Daily_Sub_Location_Id..$User_Type_Desc%in%user & Daily_Sub_Location_Id..$Obs_Hours%in%24 & Daily_Sub_Location_Id..$Error_Code%in%0,]
		#Initiazte process to find 3 or more consecutive zeros
		#--------------------------------------------
		rnums <-  Data_Select_1.. 
		runs <- rle(Data_Select_1.. $Counts == 0)
		#Next, we find indices of the runs with length of at least 3
		myruns <- which(runs$values == TRUE & runs$lengths >= 3)
		# check if myruns has any value in it 
		if(any(myruns)){
			runs.lengths.cumsum <-  cumsum(runs$lengths)
			#Find End positions of these runs
			Ends. <- runs.lengths.cumsum[myruns]
			#Next, we find the start positions of these runs
			newindex = ifelse(myruns>1, myruns-1, 0)
			Starts. <-  runs.lengths.cumsum[newindex] + 1
			if (0 %in% newindex) Starts. = c(1,Starts.)
			Index_Select. <- NULL
			for(i in 1:length(Starts.)){
				Index_Select. <- c(Index_Select., Data_Select_1.. $Index[(Starts.[i]-1):(Ends.[i]+1)] )
			}					
			#Remove  those cases with excessive zeros
			Data_Select_1..  <- Data_Select_1.. [!(Data_Select_1.. $Index%in%Index_Select.),]
		#End consecutive zeros check
		}	
		#Removing zeros can remove all data sometimes
		if(nrow(Data_Select_1.. ) > 7){
			Deciles. <- quantile(Data_Select_1.. $Counts, prob = seq(0, 1, length = 21), type = 5)
			Deciles. <- Deciles. [!(duplicated(Deciles. ))]
			Decile_Labels. <- label_Bins(names(Deciles.))
			#This doesnt work when most values are low
			if(max(Data_Select_1.. $Counts) > 25){
			Data_Select_1.. $Decile <- cut(Data_Select_1.. $Counts,breaks = Deciles., labels = Decile_Labels.)
				#Gather days in the 90th percentile
				Top_Decile. <- Data_Select_1.. $Date[Data_Select_1.. $Decile%in%Decile_Labels.[20]]
				Top_Decile_Store.. <- rbind(Top_Decile_Store.., data.frame(Sub_Location_Id = sub_location_id, User_Type_Desc = user, Date = Top_Decile.))
			
		  #
			}
		#
		}						
	  #Close uer type loop
		}
	#Close location loop
	}
	 
	#Determine size of groups
	Group_Top_Decile. <- sort(table(Top_Decile_Store..$Date))
	#Select dates where they occur in the top decile 3 or more times
	Group_Top_Decile. <- Group_Top_Decile.[Group_Top_Decile. >=3]
	
	#Initialize a data frame to store CV info
	Coefficient_Variation.. <- data.frame()
	
	#Open .pdf for charting data and error codes
#	pdf(paste("Reports/Diagnostic/Sub_Location_Id_Daily_Counts_with_Variable_Conf_Boundary_v1.5.pdf",sep=""),width = 11, height = 11)

	
	#Apply daily hard cap for all directions for each mode using parameters setting---PARAMETERS INPUT----
	#------------------------------------
	#Initiate a index vector 
	Index. <- NULL
	for(Device_Name in unique(Daily_Sub_Location_Id..$Device_Name)){
	  #Do each user type separatley
	  for(user in unique(Daily_Sub_Location_Id..$User_Type_Desc[Daily_Sub_Location_Id..$Device_Name%in%Device_Name])){ 
  	  #Determine geography - this determines the hard car since it varies by city 
  	  Geography <- Site_Location_Info..$City[Site_Location_Info..$Name%in%Device_Name]
  	  HardCap <- as.numeric(Parameters..$Value[Parameters..$User_Type%in%user & Parameters..$Geography%in%Geography])
  	  Data_Select_1.. <- Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Device_Name%in%Device_Name& Daily_Sub_Location_Id..$Direction %in%"Total" & 
  	                                               Daily_Sub_Location_Id..$User_Type_Desc%in%user & Daily_Sub_Location_Id..$Obs_Hours%in%24,]
  	  Data_Select_1..$Error_Code[Data_Select_1..$Counts > HardCap] <- 3
  	  Temp_Index. <- Data_Select_1..$Error_Code
  	  names(Temp_Index.) <- Data_Select_1..$Index
  	  Index. <- c(Index., Temp_Index.)
	  } 
	}
	#Apply error flags
	Daily_Sub_Location_Id..$Error_Code <- Index.[match(Daily_Sub_Location_Id..$Index,names(Index.))]

	
	#Flag Data with 3 or more consecutive zeros in the counts and also apply lower and upper confidence boundary 
	#--------------------------------------------
	for(sub_location_id in unique(Daily_Sub_Location_Id..$Sub_Location_Id)){
		#Do each user type separatley
		for(user in unique(Daily_Sub_Location_Id..$User_Type_Desc[Daily_Sub_Location_Id..$Sub_Location_Id%in%sub_location_id])){
		  #Select counts (not using filter() because it seems slower)
			Data_Select_1.. <- Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Sub_Location_Id%in%sub_location_id & Daily_Sub_Location_Id..$Direction %in%"Total" & 
				Daily_Sub_Location_Id..$User_Type_Desc%in%user & Daily_Sub_Location_Id..$Obs_Hours%in%24 & Daily_Sub_Location_Id..$Error_Code%in%0,]
			#Check to see if data still exists after first Error Code check
			if(nrow(Data_Select_1..) > 0){			
				#Initiazte process to find 3 or more consecutive zeros
				#--------------------------------------------
				rnums <-  Data_Select_1..
				runs <- rle(Data_Select_1..$Counts == 0)
				#Next, we find indices of the runs with length of at least 3
				myruns <- which(runs$values == TRUE & runs$lengths >= 3)
				# check if myruns has any value in it 
				if(any(myruns)){
					runs.lengths.cumsum <-  cumsum(runs$lengths)
					#Find End positions of these runs
					Ends. <- runs.lengths.cumsum[myruns]
					#Next, we find the start positions of these runs
					newindex = ifelse(myruns>1, myruns-1, 0)
					Starts. <-  runs.lengths.cumsum[newindex] + 1
					if (0 %in% newindex) Starts. = c(1,Starts.)
					Index_With_Errors. <- NULL
					for(i in 1:length(Starts.)){
						#Find indices and append 
						Index_With_Errors. <- c(Index_With_Errors., Data_Select_1..$Index[(Starts.[i]-1):(Ends.[i]+1)] )
							
					}					
					#Apply error code
					Daily_Sub_Location_Id..$Error_Code[Daily_Sub_Location_Id..$Index%in%Index_With_Errors.] <- 1
					
				#End consecutive zeros check
				}				
				#Reselect Data but leave out any data error
				Data_Select_1.. <- 	Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Sub_Location_Id%in%sub_location_id & Daily_Sub_Location_Id..$Direction %in%"Total" & Daily_Sub_Location_Id..$User_Type_Desc%in%user & Daily_Sub_Location_Id..$Obs_Hours%in%24 & 
					 Daily_Sub_Location_Id..$Error_Code%in%0,]
				
				#Calculate a coefficient of variation---
				CV <- round(sd(Data_Select_1..$Counts) / mean(Data_Select_1..$Counts),3)
				Coefficient_Variation.. <-rbind(Coefficient_Variation.., data.frame(Sub_Location = sub_location_id,User_Type_Desc = user,
					CV =  CV))
				
				#Use simple rolling mean 
				####################################
				#Separate weekday and week end
				for(weekday in unique(Data_Select_1..$Is_Weekday)){
					#Select data
					Data_Select_3.. <- filter(Data_Select_1.., Is_Weekday == weekday)
					#add bounds
					#Data_Select_3..$Lb_Conf_Bound <- NA
					#Data_Select_3..$Ub_Conf_Bound <- NA
					#Determine Index
					Index_Select. <- Data_Select_3..$Index
					#If fewer than 3 observations don;t do any rolling average----
					if(nrow(Data_Select_3..) <= 3){
						#Set manual check error code
						Daily_Sub_Location_Id..$Error_Code[Daily_Sub_Location_Id..$Index%in%Index_Select.] <- 4
						Data_Select_3..$Lb_Conf_Bound <- 0
						Data_Select_3..$Ub_Conf_Bound <- 0
					} else {						
						#Calculate rolling mean using a centered
						Counts. <- Data_Select_3..$Counts
						names(Counts.) <- Data_Select_3..$Date
						#Define starting rolling mean window
						Ma_Size  <- 5
						Left_Right_Ma_Size <- 5
						Ma_Compare.. <- data.frame()
						#Truncate rolling mean window if less than 5 observations
						if(length(Data_Select_3..$Counts) <=5){ 
							Left_Right_Ma_Size <- 3
							Ma_Size <- 3
						}  
						#Use base:rollmean for retaining vector names
						Centered_Mean_Names. <- names(rollmean(Counts.,Ma_Size))
						#Exclude current observation
						Centered_Mean. <- rollapply(Counts., Ma_Size, function(x)mean(x[-3], na.rm = TRUE))
						names(Centered_Mean.) <- 	Centered_Mean_Names.
						#Must do beginning and end of vectors separatley defining the allignment---
						#Determine remaining rows 
						Remain.. <- filter(Data_Select_3.., !(Date%in%as.Date(names(Centered_Mean.))))
						#Determine which values to select
						Left_Remain. <- Counts.[as.Date(names(Counts.))%in%Remain..$Date[1:round(Ma_Size /2)]]
						Right_Remain. <- Counts.[(length(Counts.)+1-round((Ma_Size /2))):length(Counts.)]
						#Calculate rolling mean for left values
						Left_Mean_Names. <- names(rollmean(Counts.,Left_Right_Ma_Size, align = "left"))
						Left_Mean. <- rollapply(Counts., Ma_Size, function(x)mean(x[-3], na.rm = TRUE), align = "left")
						names(Left_Mean.) <- Left_Mean_Names.
						Left_Mean. <- Left_Mean.[names(Left_Mean.)%in%names(Left_Remain.)]				
						#Calcualte rolling mean for right values
						Right_Mean_Names. <- names(rollmean(Counts.,Left_Right_Ma_Size, align = "right"))
						Right_Mean. <- rollapply(Counts., Ma_Size, function(x)mean(x[-3], na.rm = TRUE), align = "right")
						names(Right_Mean.) <- Right_Mean_Names.
						Right_Mean. <- Right_Mean.[names(Right_Mean.)%in%names(Right_Remain.)]				
						#Compile all means
						Means. <- c(Left_Mean., Centered_Mean., Right_Mean.)
						#Calculate mean again in case of duplictes above
						Means. <- tapply(Means., names(Means.), mean)
						#Store in dtate frame
						Counts_With_Rolling_Mean.. <- data.frame(Sub_Location_Id = sub_location_id, Date = names(Counts.), User_Type_Desc = user, Is_Weekday = weekday, 
							Counts = Counts. , Rolling_Mean = Means., Index = Index_Select.,Lb_Conf_Bound = NA, Ub_Conf_Bound = NA, Error_Code = 0)

						#If user is not defined then specify it as User
						if(is.na(user)){ user <- "User"}
						#Calculate and apply an counfidence boundary---PARAMETERS INPUT----
						#Use weekday parameters
						if(weekday == "Weekday"){
						  Counts_With_Rolling_Mean..$Ub_Conf_Bound <- Counts_With_Rolling_Mean..$Rolling_Mean * as.numeric(Parameters..$Value[Parameters..$User_Type%in%user & Parameters..$Parameter%in%c("Upper Bound Rolling Mean Weekday")])
						  Counts_With_Rolling_Mean..$Lb_Conf_Bound <- Counts_With_Rolling_Mean..$Rolling_Mean * as.numeric(Parameters..$Value[Parameters..$User_Type%in%user &  Parameters..$Parameter%in%c("Lower Bound Rolling Mean Weekday")])
						}
						#Use weekend parameters
						if(weekday == "Weekend"){
						  Counts_With_Rolling_Mean..$Ub_Conf_Bound <- Counts_With_Rolling_Mean..$Rolling_Mean * as.numeric(Parameters..$Value[Parameters..$User_Type%in%user & Parameters..$Parameter%in%c("Upper Bound Rolling Mean Weekend")])
						  Counts_With_Rolling_Mean..$Lb_Conf_Bound <- Counts_With_Rolling_Mean..$Rolling_Mean * as.numeric(Parameters..$Value[Parameters..$User_Type%in%user &  Parameters..$Parameter%in%c("Lower Bound Rolling Mean Weekend")])
						}
						#Change negative lower bound confidence intervals to zero
						Counts_With_Rolling_Mean..$Lb_Conf_Bound[Counts_With_Rolling_Mean..$Lb_Conf_Bound < 0] <- 0
						#Apply error type code
						#Find indices of records outside the threshold
						Index_With_Errors. <- Counts_With_Rolling_Mean..$Index[Counts_With_Rolling_Mean..$Counts < Counts_With_Rolling_Mean..$Lb_Conf_Bound | Counts_With_Rolling_Mean..$Counts > Counts_With_Rolling_Mean..$Ub_Conf_Bound]
						#Add Error Code
						Daily_Sub_Location_Id..$Error_Code[Daily_Sub_Location_Id..$Index%in%Index_With_Errors.] <- 2
						Counts_With_Rolling_Mean..$Error_Code[Counts_With_Rolling_Mean..$Index%in%Index_With_Errors.] <- 2
						#Add lower and upper bound values
						Daily_Sub_Location_Id..$Lb_Conf_Bound[Daily_Sub_Location_Id..$Index%in%Counts_With_Rolling_Mean..$Index] <- Counts_With_Rolling_Mean..$Lb_Conf_Bound
						Daily_Sub_Location_Id..$Ub_Conf_Bound[Daily_Sub_Location_Id..$Index%in%Counts_With_Rolling_Mean..$Index] <- Counts_With_Rolling_Mean..$Ub_Conf_Bound
						#Reverse rolling mean error if day falls on Potential Special Event Flag
						#Add potential event data
						Daily_Sub_Location_Id..$Potential_Special_Event[Daily_Sub_Location_Id..$Date%in%as.Date(names(Group_Top_Decile.)) & Daily_Sub_Location_Id..$Index%in%Index_With_Errors.] <- 1
						#Reverse rolling mean flag if its a special event 
						#Index_With_Errors. <- Ma_Compare..$Index[ Ma_Compare..$Counts > Ma_Compare..$Ub_Compare]  i dont htink we need to creat this index again
						Daily_Sub_Location_Id..$Error_Code[Daily_Sub_Location_Id..$Index%in%Index_With_Errors. & Daily_Sub_Location_Id..$Potential_Special_Event%in%TRUE] <- 0
					#Close less than 3 observations loop
					}
					#Clean up work space
					rm(Index_Select.)
				#Close weeday/weekend loop	
				}			
				#Graph data
				##################
				# if(nrow(Data_Select_1..) > 0){
				# 	Title <- paste("Daily Counts with Confidence Boundary\n",Site_Location_Info..$Name[Site_Location_Info..$Vendor_Site_Id%in%sub_location_id]
				# 	,sep="")				
				# 	
				# 		Site_Location_Info..$Name[Site_Location_Info..$Vendor_Site_Id%in%sub_location_id]
				# 	#Reselect data to get all the error codes
				# 	Temp.. <- Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Sub_Location_Id%in%sub_location_id & Daily_Sub_Location_Id..$Direction %in%"Total" & Daily_Sub_Location_Id..$User_Type_Desc%in%user & Daily_Sub_Location_Id..$Obs_Hours%in%24	,]
				# 	#Assign lb and ub values
				# 	#Temp..$Lb_Conf_Bound <- Data_Select_1..$Lb_Conf_Bound[match(as.Date(Temp..$Date), as.Date(Data_Select_1..$Date))]
				# 	#Temp..$Ub_Conf_Bound <- Data_Select_1..$Ub_Conf_Bound[match(as.Date(Temp..$Date), as.Date(Data_Select_1..$Date))]
				# 	Temp..$Confidence_Interval <- "Confidence_Boundary"
				# 	Temp..$Confidence_Interval[!(is.na(Data_Select_1..$Lb_Conf_Bound) & Data_Select_1..$Ub_Conf_Bound)] <- ""
				# 	Temp..$Error_Code_Desc <- names(Error_Code.)[match(Temp..$Error_Code, Error_Code.)]
				# 	#Add potential event data
				# 	Temp..$Potential_Special_Event[Temp..$Potential_Special_Event%in%0] <- "No"
				# 	Temp..$Potential_Special_Event[Temp..$Potential_Special_Event%in%1] <- "Potential Special Event"
				# 	Temp..[,c("Date","Counts","Error_Code","Lb_Conf_Bound", "Ub_Conf_Bound")]
				# 	#Create custom shapes based on error codes
				# 	Shapes. <- c(3,15,17,16,12,6)
				# 	names(Shapes.) <- names(Error_Code.)
				# 	
				# 	#Convert data to character to make the x labels easier to read
				# 	if(nrow(Temp..) < 50){Temp..$Date <- as.character(Temp..$Date)}
				# 	Plot <- Temp.. %>%
				# 	ggplot(aes(x = Is_Weekday, y = Counts)) +   	
				# 		geom_line(aes(x = Date, y = Counts),color = "black", group = 1) +
				# 		geom_point(aes(x = Date, y = Counts, pch = Error_Code_Desc, color = Potential_Special_Event), group = 1, size = 2) +
				# 		#scale_colour_manual(c("No","Potential_Special_Event"),values=c("red","green")) +
				# 		geom_ribbon(aes(x = Date, ymin = Lb_Conf_Bound, ymax = Ub_Conf_Bound,fill = "Confidence Boundary"), alpha = 0.5, group = 1) +
				# 		facet_wrap(~User_Type_Desc, nrow = 3) +
				# 		scale_fill_manual("",values="red")+
				# 		labs(fill="") +
				# 		#scale_fill_manual(values= c("lightgreen","skyblue")) +
				# 		theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=.5)) +
				# 		ggtitle(Title) +
				# 		ylab("Counts with Confidence Boundary")  +  
				# 		xlab("Date") + 
				# 		 scale_shape_manual(values=Shapes.) +
				# 		theme(plot.title = element_text(hjust = 0.5)) 
				# 	#Print to pdf
				# 	suppressWarnings(print(Plot))	
				# }
			#Close initial data check loop
			}
		#User Type Loop
		}
	#Sub Location Id loop
	}
#	dev.off()
		
	
	
	
	
#Split User data into bike/ped splits
#-----------------------------------
	#Provide user interface feedback
	print("Splitting user data into bike/ped for Sub_Location_Data")
	#Sub Location ID-----
	#Make a copy 
	#Daily_Sub_Location_Id.. <- Daily_Sub_Location_Id..
	#Add flag to Cleaned Data indicating estimated split of users into bike/ped
	Daily_Sub_Location_Id..$Est_Split <- FALSE
	#Make a copy and select proper records (24 hours, Non-directional(total), and no error code) thathave both bike and ped records
	Bike_Ped_Daily.. <- Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Direction%in%"Total" & Daily_Sub_Location_Id..$Obs_Hours%in%24 & 
		Daily_Sub_Location_Id..$User_Type_Desc%in%c("Pedestrian","Bicycle"),]
	User_Daily.. <- Daily_Sub_Location_Id..[Daily_Sub_Location_Id..$Direction%in%"Total" & Daily_Sub_Location_Id..$Obs_Hours%in%24 &  
		Daily_Sub_Location_Id..$User_Type_Desc%in%c("User"),]
	#Determine locations where user data and bike/ped data exist to estimate split
	Compare_Locations. <- names(table(User_Daily..$Sub_Location_Id[User_Daily..$Sub_Location_Id%in%Bike_Ped_Daily..$Sub_Location_Id]))
	#Open pdf to store observed split
	pdf(paste("Reports/Diagnostic/Bike_Ped_Split_Sub_Location.pdf",sep=""),width = 11, height = 11)
	
	for(location in Compare_Locations.){
		#Collect and calculate the bike proportion ratio to apply below
		####################
		#Selectlocation wand records with no errors
		Temp_Bike_Ped_Daily.. <- Bike_Ped_Daily..[Bike_Ped_Daily..$Sub_Location_Id%in%gsub("[A-Z]","",location) & Bike_Ped_Daily..$Error_Code%in%0,]
		Ped_Counts.. <- Temp_Bike_Ped_Daily.. %>% 
			filter(User_Type_Desc == "Pedestrian") %>%
			group_by(User_Type_Desc, Date) %>%
			summarise(Counts = sum(Counts))
		Bike_Counts.. <- Temp_Bike_Ped_Daily.. %>% 
			filter(User_Type_Desc == "Bicycle") %>%
			group_by(User_Type_Desc, Date) %>%
			summarise(Counts= sum(Counts))
		#Check to see if bike AND ped counts exist
		if(!(any(nrow(Bike_Counts..) == 0 | nrow(Ped_Counts..)==0))){
			Counts.. <- data.frame(Date = Ped_Counts..$Date, Ped_Counts = Ped_Counts..$Counts)
			Counts..$Bike_Counts <- Bike_Counts..$Counts[match(Counts..$Date, Bike_Counts..$Date)]
			Counts..$User_Counts <- Counts..$Bike_Counts + Counts..$Ped_Counts
			Counts..$Prop_Bike <- Counts..$Bike_Counts / Counts..$User_Counts
			Counts..$Prop_Ped <- 1-Counts..$Prop_Bike
			Counts..$Month <- months(Counts..$Date)
			Counts..$Year <- year(Counts..$Date)
			Counts..$Weekday <- weekdays(Counts..$Date)
			Counts.. <- mutate(Counts.., Is_Weekday = "Weekday")
			Counts.. <- mutate(Counts.., Is_Weekday = ifelse(Weekday%in% c("Saturday","Sunday"), "Weekend", Is_Weekday))
			#Makes sure all data is complete 
			Counts.. <- Counts..[complete.cases(Counts..),]
			Temp_Split_Summary.. <- Counts.. %>%
				group_by(Is_Weekday, Month, Year) %>%
				summarise(Prop_Bike = mean(Prop_Bike,na.rm=T), N = length(Bike_Counts))
			
			#Chart the split for review
			#####################
			#Create an average for weekday and weekend 
			dat <- melt(Counts.. %>% group_by(Month, Is_Weekday) %>% summarise(Bicycle = mean(Prop_Bike), Pedestrian = mean(Prop_Ped)), id.vars = c("Is_Weekday","Month"), variable.name = "User_Type_Desc",value.name = "Proportion")
			Plot <- ggplot(dat, aes(x = Month, y = Proportion, fill = User_Type_Desc)) +
			  geom_bar(stat="identity", width=.5, position = "dodge")  + 
			  theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=.5)) +
			  facet_grid(~Is_Weekday) +
			  ggtitle(paste("Proportional split between Bicyle and Pedestrian Traffic at \n",location,sep="")) +
			  ylab("Average Proportion of Daily Count")  +  
			  xlab("Month") + 
			  theme(plot.title = element_text(hjust = 0.5)) +
			  scale_y_continuous(breaks = seq(0,1,.1),labels=percent)
			print(Plot)
			
			#Select USer Data
			Temp_User_Data.. <- User_Daily..[User_Daily..$Sub_Location_Id%in%location,]
			Temp_User_Data.. <- left_join(Temp_User_Data.., Temp_Split_Summary..,by = c("Is_Weekday", "Month", "Year"))
			Bike_Data.. <- Temp_User_Data.. 
			Ped_Data.. <- Temp_User_Data.. 
			#Calculate modal volumes and create new data frames of separated bike and ped counts
			Bike_Data..$Counts <- round(Bike_Data..$Counts * Bike_Data..$Prop_Bike)
			Bike_Data..$User_Type_Desc <- "Bicycle"
			Bike_Data..$Est_Split <- TRUE
			#Replace lower and upper bound rolling mean values with NA
			Bike_Data..$Lb_Conf_Bound <- NA
			Bike_Data..$Ub_Conf_Bound <- NA
			#Select proper rows
			Bike_Data.. <- Bike_Data..[,colnames(Daily_Sub_Location_Id..)]
			Ped_Data..$Counts <- round(Ped_Data..$Counts * (1-Ped_Data..$Prop_Bike))
			Ped_Data..$User_Type_Desc <- "Pedestrian"
			#Select proper rows
			#Add synthetic split flag 
			Ped_Data..$Est_Split <- TRUE
			Ped_Data.. <- Ped_Data..[,colnames(Daily_Sub_Location_Id..)]
			#Replace lower and upper bound rolling mean values with NA
			Ped_Data..$Lb_Conf_Bound <- NA
			Ped_Data..$Ub_Conf_Bound <- NA
			#Append counts to data frame
			Daily_Sub_Location_Id.. <- rbind(Daily_Sub_Location_Id..,Bike_Data.., Ped_Data..)
		}
	}		
	 #Close pdf 
	dev.off()
	
	
	
	#Store sumamrize with new error codes assigned
	###################################################
	#Provide user report
	print(paste("Total process time: ", round(as.numeric(Sys.time() - Start_Time,units = "hours")*60,1)," Minutes", sep=""))
	print("Storing Sub_Location_Id data with error flags.....")

	#Append daily records error codes to hourly data records
	Hourly_Sub_Location_Id.. <- Load_Hourly_Sub_Location_Id..
	Hourly_Sub_Location_Id..$Error_Code <- Daily_Sub_Location_Id..$Error_Code[match(paste(Hourly_Sub_Location_Id..$Date, Hourly_Sub_Location_Id..$Sub_Location_Id, Hourly_Sub_Location_Id..$User_Type_Desc),
		paste(Daily_Sub_Location_Id..$Date, Daily_Sub_Location_Id..$Sub_Location_Id,Daily_Sub_Location_Id..$User_Type_Desc))] 
	#Append daily potential special events codes to hourly data records
	Hourly_Sub_Location_Id..$Potential_Special_Event <- Daily_Sub_Location_Id..$Potential_Special_Event[match(paste(Hourly_Sub_Location_Id..$Date, Hourly_Sub_Location_Id..$Sub_Location_Id, Hourly_Sub_Location_Id..$User_Type_Desc),
		paste(Daily_Sub_Location_Id..$Date, Daily_Sub_Location_Id..$Sub_Location_Id,Daily_Sub_Location_Id..$User_Type_Desc))] 
	
	#Write out new files
	save(Daily_Sub_Location_Id..,file = paste("Counts Data/Step 3 - Counts with Errors/Daily_Sub_Location_Id_",nrow(Daily_Sub_Location_Id..),".RData",sep=""))
	#Write out new files
	save(Hourly_Sub_Location_Id..,file = paste("Counts Data/Step 3 - Counts with Errors/Hourly_Sub_Location_Id_",nrow(Hourly_Sub_Location_Id..),".RData",sep=""))
	
	#Determine if .csv files are desired - this takes a while for reading/writing count data
	Write_Csv <- as.logical(Parameters..$Value[Parameters..$Parameter%in%"Write_Csv"])
	#Report to user is Csvs are being written
	if(Write_Csv){"Raw data file output will include .csv files.";
		write.csv(Hourly_Sub_Location_Id.., file = paste("Counts Data/Step 3 - Counts with Errors/Daily_Sub_Location_Id_",nrow(Hourly_Sub_Location_Id..),".csv",sep=""))
		write.csv(Daily_Sub_Location_Id.., file = paste("Counts Data/Step 3 - Counts with Errors/Daily_Sub_Location_Id_",nrow(Daily_Sub_Location_Id..),".csv",sep=""))
		
	} else{"Raw data file output will include .RData only."}
	
	#Provide user report
	print("Data (Step 3) cleaning process complete.")
	
