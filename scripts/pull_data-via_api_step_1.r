#Author: Josh Roll Oregon Department of Transportation Research Unit
#Date: 2/14/2018
#Description: This script uses credentials supplied by Eco-Counter to call the Oregon DOT domain 
#set up to house counts data through an Application Programming Interface (API).  These data are 
#and stored for further processing.  

#Notes:
#Script currently only working with R version 3.3.2



#Load Parameters file
#-----------------------
	Parameters.. <- 	read_excel("Documentation/Data Dictionary.xlsx", sheet = "Parameters")

#Load custom functions
#--------------------------
	#Load processing functions
	source("Scripts/functions.r")
	attach(Functions,warn.conflicts=FALSE)

#Set up environment conditions
#----------------------------------
	#Characters as factors will create problem for API so turn this off
	options(stringsAsFactors = FALSE, warning = 2)
	#Define State Domain 
	State_Domain. <- as.character(Parameters..$Value[Parameters..$Parameter%in%"State Domain"])
	#Create directories if they do not exist
	#########################
	#Supporting data
	if(!(dir.exists("Supporting Data"))){dir.create("Supporting Data")}
	#Spatial data
	if(!(dir.exists("Supporting Data/Spatial"))){dir.create("Supporting Data/Spatial")}
	#Census data 
	if(!(dir.exists("Supporting Data/Spatial/Census"))){dir.create("Supporting Data/Spatial/Census")}
	#API call information
	if(!(dir.exists("Supporting Data/API Call Summary Information"))){dir.create("Supporting Data/API Call Summary Information")}
	#Deployment photos directory
	if(!(dir.exists("Supporting Data/Deployment Information"))){dir.create("Supporting Data/Deployment Information")}
	#Deployment photos sub directory
	if(!(dir.exists("Supporting Data/Deployment Information/Deployment Pictures"))){dir.create("Supporting Data/Deployment Information/Deployment Pictures")}
	#Count Data folder
	if(!(dir.exists("Counts Data"))){dir.create("Counts Data")}
	#Raw Counts
	if(!(dir.exists("Counts Data/Step 1 - Raw Counts"))){dir.create("Counts Data/Step 1 - Raw Counts")}
	#Processed
	if(!(dir.exists("Counts Data/Step 2 - Processed Counts"))){dir.create("Counts Data/Step 2 - Processed Counts")}
	#Cleaned
	if(!(dir.exists("Counts Data/Step 3 - Counts with Errors"))){dir.create("Counts Data/Step 3 - Counts with Errors")}
	#Annualized
	if(!(dir.exists("Counts Data/Step 4 - Daily and Annual Estimates"))){dir.create("Counts Data/Step 4 - Daily and Annual Estimates")}
	#Report directories
	if(!(dir.exists("Reports"))){dir.create("Reports")}
	#Diagnotstic reports
	if(!(dir.exists("Reports/Diagnostic"))){dir.create("Reports/Diagnostic")}
	#ANnual estimation reports
	if(!(dir.exists("Reports/Annual Estimation"))){dir.create("Reports/Annual Estimation")}
	#Mode comparison
	if(!(dir.exists("Reports/Mode Comparison"))){dir.create("Reports/Mode Comparison")}
	#Spatial 
	if(!(dir.exists("Reports/Spatial"))){dir.create("Reports/Spatial")}

#Define script vectors and data frames
#----------------------
	#Items from Data Dictionary
	######################################
	#Load User Type sheet---
	Data_Dictionary.. <- read_excel("Documentation/Data Dictionary.xlsx", sheet = "User_Type_Code")
	Eco_User_Type. <- Data_Dictionary..$User_Type
	names(Eco_User_Type.) <- Data_Dictionary..$Eco_User_Type_Desc
	Eco_User_Type. <- Eco_User_Type.[!(is.na(names(Eco_User_Type.)))] 

	#Load census spatial data to append to count locations for better spatial information
	####################
	#Determine if files exist
	#Place data 
	if(!(any(list.files(paste(getwd(),"/Supporting Data/Spatial/Census/",sep=""))%in%"Places_Spatial_Data.RData"))){
	Places_Sp <- as_Spatial(places(State_Domain.))
	save(Places_Sp, file = paste(getwd(),"/Supporting Data/Spatial/Census/Places_Spatial_Data.RData",sep=""))
	} else {Places_Sp <- assignLoad(file = paste(getwd(),"/Supporting Data/Spatial/Census/Places_Spatial_Data.RData",sep=""))}
	#County data
	if(!(any(list.files(paste(getwd(),"/Supporting Data/Spatial/Census/",sep=""))%in%"County_Spatial_Data.RData"))){
	County_Sp <- as_Spatial(counties(State_Domain.))
	save(County_Sp, file = paste(getwd(),"/Supporting Data/Spatial/Census/County_Spatial_Data.RData",sep=""))
	} else {County_Sp <- assignLoad(file = paste(getwd(),"/Supporting Data/Spatial/Census/County_Spatial_Data.RData",sep=""))}

#Set credentials
#------------------
	#Load .csv file with stored credentials
	Api_Credentials.. <- read.csv("Supporting Data/API Call Summary Information/API_Credentials.csv")
	#Scrub white space from cells to ensure proper input
	Api_Credentials..$Credential <- gsub(" ","",Api_Credentials..$Credential)
	Api_Credentials..$Element <- gsub(" ","",Api_Credentials..$Element)
	Consumer_Key <-  Api_Credentials..$Credential[Api_Credentials..$Element%in%"Consumer_Key"]
	Consumer_Secret <- Api_Credentials..$Credential[Api_Credentials..$Element%in%"Consumer_Secret"]
	#Get token
	#######################
	#Use basic authentication
	url <- "https://apiadmin.eco-counter-tools.com/store"
	#Encode
	secret <- base64_enc(paste(Consumer_Key, Consumer_Secret, sep = ":"))
	req <- POST("https://apieco.eco-counter-tools.com/token",
			  add_headers(
				"Authorization" = paste("Basic", gsub("\n", "", secret)),
				"Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"
			  ),
			  body = "grant_type=client_credentials"
	)
	#Extract the access token
	token <- paste("Bearer", content(req)$access_token)
	#Actual API call
	req <- GET(url, add_headers(Authorization = token))
	#Test connection
	if(req$status_code == 200 ){print("API connection successfully made!")}else{print("API connection was not established.  Check credientials and/or internet connection")}
	#Request all site information 
	url <- paste("https://apieco.eco-counter-tools.com/api/1.0/site",sep="")
	All_Site_Info_Req <- GET(url, add_headers(Authorization = token))
	
	#Perform a pre-check to determine what data is avialable in API and only download data iwhtin the domain
	################################
	#Initialize a data frameto store possible sites to pull data
	Init_Site_Location_Info.. <- data.frame()
	#Loop through each counter
	for(i in 1:length(content(All_Site_Info_Req))){
	#Create and object representing the Site Id
	Vendor_Site_Id <- content(All_Site_Info_Req)[[i]]$id
	#Create an object representing the device name
	Device_Name <- content(All_Site_Info_Req)[[i]]$name
	#Construct data frame
	Temp_Init_Site_Location_Info.. <- 
	   #Store location information
		data.frame(
			Vendor_Site_Id = Vendor_Site_Id, 
			Name = Device_Name,
			Latitude = content(All_Site_Info_Req)[[i]]$latitude,
			Longitude = content(All_Site_Info_Req)[[i]]$longitude,
			Channel_Count = length(content(All_Site_Info_Req)[[i]]$channels),
			Interval = content(All_Site_Info_Req)[[i]]$interval,
			Domain = content(All_Site_Info_Req)[[i]]$domain
		)	
		#Need to handle the install data in a different way
		if(length(content(All_Site_Info_Req)[[i]]$installationDate) > 0 ){
			Temp_Init_Site_Location_Info..$Install_Date  <-  unlist(strsplit(content(All_Site_Info_Req)[[i]]$installationDate,"T"))[1]
			} else {Temp_Init_Site_Location_Info..$Install_Date  <- NA
		}#Bind
		Init_Site_Location_Info.. <- rbind(Init_Site_Location_Info.., Temp_Init_Site_Location_Info..)
	#Print UI feedback
	if(i == 1){print("Calling Eco Counter API and setting up initial set of sites to pull data......")}
	}
	#Convert spatial data frame into a spatial data frame for determineing geographic location
	Site_Location_Info_Sp <- Init_Site_Location_Info..
	#Create coordinates
	coordinates(Site_Location_Info_Sp) <- ~ Longitude + Latitude
	# Set the projection of the SpatialPointsDataFrame using the projection of the county shape file
	proj4string(Site_Location_Info_Sp) <- CRS(proj4string(County_Sp))
	#Perform the overlay for counties---
	Init_Site_Location_Info..$County <- over(Site_Location_Info_Sp, County_Sp)$NAME
	#Sites wihout lat/long return <NA> which does not work well for the logiv statement below so fix 
	Init_Site_Location_Info..$County[is.na(Init_Site_Location_Info..$County)] <- "No Latitude/Longitude"
	#Trim white space from beginning and end of domain name
	Init_Site_Location_Info..$Domain <- trimws(Init_Site_Location_Info..$Domain)

	#Create a list of names to pull data based on county and domain ---PARAMETERS INPUT----
	#Select County domain(s) from parameters 
	County_Domain. <- trimws(Parameters..$Value[Parameters..$Parameter%in%"County Domain"])
	if(  County_Domain. == "All Counties"){  County_Domain. <-  unique(County_Sp@data$NAME)}
	#Select local city domain(s) from parameters 
	Eco_Domain. <- trimws(unlist(strsplit(as.character(Parameters..$Value[Parameters..$Parameter%in%"Eco Counter Domain"]),",")))
	#Pull from initial set of sites count device names within the domain specified in parameters file
	Select_Device_Names. <- Init_Site_Location_Info..$Name[Init_Site_Location_Info..$County%in%County_Domain.  | Init_Site_Location_Info..$Domain%in%Eco_Domain. ]
	Select_Site_Ids. <- as.numeric(rownames(Init_Site_Location_Info..)[Init_Site_Location_Info..$Name%in%Select_Device_Names.])

	#Do an error check to make sure some device names are found
	if(length(Select_Site_Ids. ) == 0 ){
		print("No device names found in specified domains.  Check Parameters.csv inputs.")
		stop
	}
  
	#Initialize data frames to store data  
	Site_Location_Info.. <- data.frame()
	Channel_Info.. <- data.frame()
	Counts_Data.. <- data.frame()
	#Start timer to track processing time
	Start_Time <-  Sys.time()
	#Initiate a process tracker
	Process_i <- 1
	#Loop through each counter
	for(i in Select_Site_Ids.){
		Start_Time_i <-  Sys.time()	
		#Create and object representing the Site Id
		Vendor_Site_Id <- content(All_Site_Info_Req)[[i]]$id
		#Create an object representing the device name
		Device_Name <- content(All_Site_Info_Req)[[i]]$name
		#Construct data frame
		Site_Location_Info.. <- rbind(Site_Location_Info..,

		#Store location information
		##################################
		data.frame(
		  Vendor_Site_Id = Vendor_Site_Id, 
		  Name = Device_Name,
		  Latitude = content(All_Site_Info_Req)[[i]]$latitude,
		  Longitude = content(All_Site_Info_Req)[[i]]$longitude,
		  Channel_Count = length(content(All_Site_Info_Req)[[i]]$channels),
		  Interval = content(All_Site_Info_Req)[[i]]$interval,
		  Domain = content(All_Site_Info_Req)[[i]]$domain,
		  Install_Date = unlist(strsplit(content(All_Site_Info_Req)[[i]]$installationDate,"T"))[1]
		))
	#Call site specific 
    url <- paste("https://apieco.eco-counter-tools.com/api/1.0/site/", Vendor_Site_Id, sep="")
    #Call the site information.  This call has each channel's information
    Site_Req <- GET(url, add_headers(Authorization = token))
    if(Site_Req$status_code != 200){print(paste("API Call for Site ",Device_Name," not successful",sep=""))}
    
    #Store channel information
    ##################################
    #Go through each channel
    #Ensure channels exist
    if(length(content(All_Site_Info_Req)[[i]]$channel) > 0){
      #Now cycle through each
      for(j in 1:length(content(All_Site_Info_Req)[[i]]$channel)){
        #Serial number is not always present (need to investigate this further but serial number less important for permanent stations)
        Serial_Num  <- content(All_Site_Info_Req)[[i]]$counter
        if(length(Serial_Num) < 1){Serial_Num <- NA}else{Serial_Num <- content(All_Site_Info_Req)[[i]]$counter}
        #Store in data frame
        Channel_Info.. <- rbind(Channel_Info..,				
                                data.frame(
                                  Vendor_Site_Id = Vendor_Site_Id,
                                  Channel_Id  = as.character(content(All_Site_Info_Req)[[i]]$channels[[j]]$id),
                                  Channel_Name = content(All_Site_Info_Req)[[i]]$channels[[j]]$name,
                                  User_Type = content(All_Site_Info_Req)[[i]]$channels[[j]]$userType,
                                  Interval = content(All_Site_Info_Req)[[i]]$channels[[j]]$interval,
                                  Device_Name = Device_Name,
                                  Serial_Num = Serial_Num					
                                ))
        
        #Store count data information
        ###########################			
        Channel_Id <- content(All_Site_Info_Req)[[i]]$channels[[j]]$id
        url <- paste("https://apieco.eco-counter-tools.com/api/1.0/data/site/", Channel_Id,sep="")
        Site_Data_Req <- GET(url, add_headers(Authorization = token))
        #Check to make sure data is available by looking at status code
        if(Site_Data_Req$status_code == 200){
          #Convert data into a data frame
          Temp_Counts_Data.. <- fromJSON(content(Site_Data_Req, as = "text"))
          #Check to make sure counts data exists
          if(length(nrow(Temp_Counts_Data..))>0){
            #Format data
            Temp_Counts_Data..$Date <- as.Date(do.call("rbind",strsplit(Temp_Counts_Data..$date,"T"))[,1], format = "%Y-%m-%d")
            #Format Time
            Temp_Counts_Data..$Time  <- do.call("rbind",strsplit(do.call("rbind",strsplit(Temp_Counts_Data..$date,"T"))[,2],"\\+"))[,1]
            #Create time stamp 
            Temp_Counts_Data..$Time_Stamp <- as.POSIXct(paste(Temp_Counts_Data..$Date, Temp_Counts_Data..$Time,sep="-"), format = "%Y-%m-%d-%H:%M")
            #Format hour
            Temp_Counts_Data..$Hour  <- paste(do.call("rbind",strsplit(Temp_Counts_Data..$Time,":"))[,1],":00",sep="")
            #Add Device name
            Temp_Counts_Data..$Device_Name <- Device_Name
            #Add device serial number
            Temp_Counts_Data..$Serial_Num <- Serial_Num
            #Rename some columns and select final data frame columns
            Temp_Counts_Data..$Counts <- Temp_Counts_Data..$counts
            #Append channel Id
            Temp_Counts_Data..$Channel_Id <- content(All_Site_Info_Req)[[i]]$channels[[j]]$id
            #Append channel name
            Temp_Counts_Data..$Channel_Name <- content(All_Site_Info_Req)[[i]]$channels[[j]]$name
            #User_Type 
            Temp_Counts_Data..$User_Type <- content(All_Site_Info_Req)[[i]]$channels[[j]]$userType
            #User_Type Description
            Temp_Counts_Data..$User_Type_Desc <- names(Eco_User_Type.)[match(Temp_Counts_Data..$User_Type, Eco_User_Type.)]
            #Add Vendor Site Id
            Temp_Counts_Data..$Vendor_Site_Id <- Vendor_Site_Id
            #Add Site Name
            #Rearrange the column headers
            Temp_Counts_Data.. <- Temp_Counts_Data..[,c("Vendor_Site_Id","Device_Name","Channel_Id","Channel_Name",
                                                        "Date","Time","Hour","Time_Stamp","Counts","User_Type_Desc","Serial_Num")]
            #Store---
            Counts_Data.. <- rbind(Counts_Data.., Temp_Counts_Data..)
            #Check for counts data loop
          }
        }
      }		
    }
    #Report progress to User
    ########################
    if(Process_i == 1){print(paste("Processing ",length(Select_Site_Ids.)," Eco Counter Sites.",sep=""))}
    #Calculate estimated time remaining
    Duration_i <- as.numeric(round(Sys.time() - Start_Time_i,1), units ="hours") * 60
    Remaining_Time <- (length( Select_Site_Ids.) - Process_i) * Duration_i * 60
    print(paste(Device_Name, " Done(", Process_i ," of ",length(Select_Site_Ids.),"). Estimated ",Remaining_Time, " Minutes Remaining",sep=""))
    Process_i <- Process_i + 1
  }
	
	#Report run time
	print(paste("Total process time: ", round(as.numeric(Sys.time() - Start_Time,units = "hours")*60,1)," Minutes", sep=""))

	#Clean up names---
	#Remove ":" from device names
	Counts_Data..$Device_Name <- gsub(":", "",Counts_Data..$Device_Name)
	Site_Location_Info..$Name <- gsub(":", "",Site_Location_Info..$Name)
	Channel_Info..$Device_Name <-  gsub(":", "",Channel_Info..$Device_Name )
	#Add User type description---
	User_Type_Site_Id. <- tapply(Counts_Data..$User_Type_Desc, Counts_Data..$Vendor_Site_Id, unique)
	User_Type_Site_Id. <- unlist(lapply(User_Type_Site_Id. ,function(x){
		x <- x[!(x%in%"Undefined")]
		x <- paste(sort(x),collapse="-")
		x
	}))
	Site_Location_Info..$User_Type_Desc <- User_Type_Site_Id.[match(Site_Location_Info..$Vendor_Site_Id, names(User_Type_Site_Id.))]

	#Apply a flag for permanent, mobile, and manual counts
	##########################
	#Define known Portable Counting Device Serial Number
	Portable_Name. <- c("BendTUBE538", "BendMULTI547", "BendTUBE539", "BendTUBE541",  "BendMULTI549", "BendMULTI550", "BendMULTI545", "BendMULTI546", "BendTUBE542",  "BendMULTI548",
						"BendTUBE540",  "BendMULTI543", "BendMULTI544","BendMULTI551","BendMULTI552")
	Counts_Data..$Collection_Type_Desc <- "Permanent"
	Counts_Data..$Collection_Type_Desc[(Counts_Data..$Device_Name%in%Portable_Name.)] <- "Mobile"
	  
	#Create spatial data for count locations and append geographic variables
	#######################################
	#Make a spatial version of the count locations	
	Site_Location_Info_Sp <- Site_Location_Info..
	#Create coordinates
	coordinates(Site_Location_Info_Sp) <- ~ Longitude + Latitude
	# Set the projection of the SpatialPointsDataFrame using the projection of the county shape file
	Projection <- CRS(proj4string(County_Sp))
	proj4string(Site_Location_Info_Sp) <- Projection
	#Ensure the two projections match
	proj4string(County_Sp) <- Projection
	proj4string(Places_Sp) <- Projection 
	#Perform the overlay for counties---
	Site_Location_Info..$County <- over(Site_Location_Info_Sp, County_Sp)$NAME
	Site_Location_Info_Sp@data$County <- over(Site_Location_Info_Sp, County_Sp)$NAME
	#Perform the overlay for places(cities)---
	Site_Location_Info..$City <- over(Site_Location_Info_Sp, Places_Sp)$NAME
	Site_Location_Info_Sp@data$City <- over(Site_Location_Info_Sp, Places_Sp)$NAME

	#Save data.frames to file (.csv) - with .csv the data classes are lost and take longer
	########################################
	#Report to user
	print("Storing data pulled down from API now.  If .csv format is selected this will take a few minutes longer.....")
	#Determine if .csv files are desired - this takes a while for reading/writing count data
	Write_Csv <- as.logical(Parameters..$Value[Parameters..$Parameter%in%"Write_Csv"])
	#Report to user is Csvs are being written
	if(Write_Csv){print("Raw data file output will include .csv formatted files.")} else {print("Raw data (Step 1) file output will include .RData format only.")}
	#Counts Data (add the number of rows to the file name)---
	if(Write_Csv){
		write.csv(Counts_Data..,file = paste("Counts Data/Step 1 - Raw Counts/All_Raw_Counts_Data_",nrow(Counts_Data..),".csv",sep=""),row.names = F)
	}
	save(Counts_Data..,file = paste("Counts Data/Step 1 - Raw Counts/All_Raw_Counts_Data","_",nrow(Counts_Data..),".RData",sep=""))
	#Count Location file
	if(Write_Csv){
		write.csv(Site_Location_Info..,file = paste("Supporting Data/API Call Summary Information/Site_Location_Info","_",nrow(Site_Location_Info..),".csv",sep=""),row.names=FALSE)
	}
	save(Site_Location_Info..,file = paste("Supporting Data/API Call Summary Information/Site_Location_Info","_",nrow(Site_Location_Info..),".RData",sep=""))
	#Save spatial version of the site location data
	save(Site_Location_Info_Sp, file = paste("Supporting Data/API Call Summary Information/Site_Location_Info_Sp","_",nrow(Site_Location_Info_Sp),".RData",sep=""))

	#Channel information file
	if(Write_Csv){
		write.csv(Channel_Info..,file = paste("Supporting Data/API Call Summary Information/Channel_Info","_",nrow(Site_Location_Info..),".csv",sep=""),row.names=FALSE)
	}
	save(Channel_Info..,file = paste("Supporting Data/API Call Summary Information/Channel_Info","_",nrow(Channel_Info..),".RData",sep=""))

	#Print user interface report
	print("API Call Completed")
  