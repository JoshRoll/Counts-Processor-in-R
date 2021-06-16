#Author: Josh Roll Oregon Department of Transportation Research Unit
#Date: 6/6/2019
#Name - run_main
#Description: This script wraps scripts that process traffic count data collected via Eco Counter Products
#
#Notes:
#Script currently only working with R version 3.6.2
#Setup
#-------------------
#1. Create a master working directory. This will be the same as that specified in line 47 below within the setwd())
#2. Ensure you have the Supporting Data/ folder populated with the latest files including:
#---/API Call Summary Information/API_Credentials.csv - This file is necessary to access the Eco Counter API.  This file should not need to be updated unless something changes on Eco Counters end. This file shoudl not 
#be shared with anyone else as they are Bend MPO's credentials.  
#---/Spatial/Bend_Counts_Spatial_Data.gdb.gdb  - This file is necessary to properly process the portable counts data and assign a locationin space for the permanent counts. 
#3. Ensure you have the Documentation folder populated with the latest Data Dictionary.xlsx file - This file is necessary to propoerly decode domain values in the Bend_Spatial_Data_2018.gdb.  
#4. Ensure you have the /Parameters/Parameters.csv file updated and present in the propoer directory
#4. Data Visualization - The dynamic data visualization tools are built in R Shiney and require the user to use R Studio to work properly 

#Load libraries
#-------------------------------------
	library(httr)
	library(jsonlite)
	library(lubridate)
	library(sp)
	library(maps)
	library(maptools)
	library(rgeos)
	library(tigris)
	library(ggplot2)
	library(curl)
	library(readxl)
	library(dplyr)
	library(tibble)
	library(scales)
	library(forecast)
	library(rgdal)
	library(stringr)
	library(googlesheets)
	library(chron)
	library(timeDate)
	library(shiny)  
	#library(leaflet)  
	library(sf)
	library(plotly)
	library(shinyWidgets)
	library(spData)   
	library(googledrive)
	library(zoo)
	library(DT)
  library(forcats)

	#If packages do not exist uncomment the line below
	#########################################
	#install.packages(c("httr","jsonlite","lubridate","Hmisc","sp","maps","maptools","rgeos","tictoc","tigris","ggplot2","reshape2","rcurl","readxl","reshape2","dplyr","tibble","scales",
	#	"forecast","rgdal","StreamMetabolism","MASS","pscl","stringr","googlesheets","chron","timeDate","shiny",
	#"leaflet","sf","plotly","shinyWidgets"))
	
	
#Start Processing Data 
#----------------------------

	#Set working directory (user must create this directory)
	##################################
	setwd("//wpdotfill09/R_VMP3_USERS/tdb069/Git_Staging/Counts-Processor-in-R")
 
	#Start a timer to track runtime
	Start_Time_Master <- Sys.time()
 
	#Run API Call Script - Step 1
	###########################################
	source("scripts/pull_data-via_api_step_1.r")
	 
	#Run Process Data Script - Step 2
	##########################################
	source("scripts/process_raw_counts_step_2.r")
	 
	#Run Error Flagging Script - Step 3
	##########################################
	source("scripts/Process/apply_qaqc_step_3.r")
	
	#Print the total run time 
	print(paste("Total process time: ", round(as.numeric(Sys.time() - Start_Time_Master,units = "hours")*60,1)," Minutes", sep=""))

#Data visualization
#---------------------------------------------------------
	#Run R Shiney app do explore all data with daily and monthly summaries
 	runApp("scripts")
	
 
 
 
 
 
 
  
 
 
 
 
 
 
 