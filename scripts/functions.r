#Author: Josh Roll Oregon Department of Transportation Research Unit
#Date: 5/14/2019
#Description: This script is called by the Oregon Bicycle and Pedestrian Counts in R (OBAPCR) process scripts
#Version Notes:

#Initialize list to store custom functions
Functions <- list()


#Function to properly capitalize character strings
#======================================================
Functions$simpleCap <- function(x) {
		s <- strsplit(x, " ")[[1]]
		paste(toupper(substring(s, 1,1)), substring(s, 2),
		sep="", collapse=" ")
	}
	
#Function to set capitalization
#======================================================	
#Proper name
Functions$toProperName <- function(X){
          EndX <- nchar(X)
          paste(toupper(substring(X, 1, 1)), tolower(substring(X, 2, EndX)), sep="")
     }

#For loading R data objects
#======================================================
Functions$assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }


#Function to check  proper format and existence of deployment/pickup date/time
#======================================================
	Functions$checkDateTimeFormat <- function(Deploy.., Sub_Location_Id){
		
		#Check date/time to ensure proper format
		#Deployment Date--
		Deploy_Date <- Deploy..$Deployment_Date[Deploy..$Sub_Location_Id%in%Sub_Location_Id]
		if(is.na(Deploy_Date)){
			stop(paste("Deployment date not found in 'Deployment_Information.csv' for Sub_Location_Id ",Sub_Location_Id,sep=""));
		}
		if(!(is.Date(Deploy_Date))){stop(paste("Problem with deployment date in 'Deployment_Information.csv' for Location Id ",Sub_Location_Id,sep=""))}
		#Deployment Time--
		Deploy_Time <- Deploy..$Deployment_Time[Deploy..$Sub_Location_Id%in%Sub_Location_Id]
		if(is.na(Deploy_Time)){
			stop(paste("Deployment time not found in 'Deployment_Information.csv' for Sub_Location_Id ",Sub_Location_Id,sep=""))
		}
			
		#Check character length
		if(!(nchar(Deploy_Time) == 5)){stop(paste("Problem in deployment time in 'Deployment_Information.csv' for Location Id ",Sub_Location_Id,sep=""))}
		
		#Pickup Date--
		Pickup_Date <- Deploy..$Pickup_Date[Deploy..$Sub_Location_Id%in%Sub_Location_Id]
		if(is.na(Deploy_Date)){
			stop(paste("Pickup date not found in 'Deployment_Information.csv' for Sub_Location_Id ",Sub_Location_Id,sep=""));
		}
		if(!(is.Date(Pickup_Date))){stop(paste("Problem with deployment date in 'Deployment_Information.csv' for Location Id ",Sub_Location_Id,sep=""))}
		#Deployment Time--
		Pickup_Time <- Deploy..$Pickup_Time[Deploy..$Sub_Location_Id%in%Sub_Location_Id]
		if(is.na(Pickup_Time)){
			stop(paste("Pickup time not found in 'Deployment_Information.csv' for Sub_Location_Id ",Sub_Location_Id,sep=""));
		}
		#Check character length
		if(!(nchar(Pickup_Time))){stop(paste("Problem in pickup time in 'Deployment_Information.csv' for Location Id ",Sub_Location_Id,sep=""))}
	
	#Close function 
	}


#Function to create bin labels
#=====================================
	Functions$label_Bins <- function(Breaks.){
		Labels_ <- list()
		for(i in 1:(length(Breaks.)-1)){
			Labels_[[i]] <- paste(Breaks.[[i]], Breaks.[[i + 1]], sep="-")
		}
		#Return result
		unlist(Labels_)
	}	

#Function to calculate psuedo r squared
#=====================================
Functions$pseudoR2 <- function(Model){
		df <- summary(Model)$df[2]
		x <- summary(Model)$deviance
		y <- summary(Model)[["null.deviance"]]
		R2 <- 1 - (x / y)
		R2
	}