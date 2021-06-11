# Project Summary  
This repository contains scripts and inputs that allow for accessing bicycle and pedestrian traffic counts data from traffic count device manufacturer Eco Counters Application Programming Interface (API).  These data are prepared for 
analysis by formatting and adding temporal information as well as applying an automated QAQC process.  These data are then displayed in an R Shiny app for exploration.  This project supports the Oregon Department of Transportation's 
non-motorized traffic monitoring efforts.  All the scripts in this directory will run with the test data in this repo except for the Step 1 script whihc required an paid API credential.  

# Setup  
1. After downloading the repository be sure to update the API_Credentials.csv in the Supporting Data\API Call Summary Information directory. 
2. From the run_main.r script ensure all packages are up to date and available for use.
3. Once steps 1 through 3 are finished run the shinyApp in the data visualization section and enjoy those sweet data.  

# Script Details  
The scripts in this repository allow users to employ the Eco Counter Application Programming Interface (API) and relevant credentials to download and prepare non-motorized traffic counts data.  The process involved includes 
three steps explained below.  The repo also includes a basic data viewer which utilizes R Shiny packages.  Since you the user needs an API credential from Eco Counter the first step will not run without the credential updated 
but the other scripts (2, 3 and the data viz) will run with the test data stored in this repo.  

## pull_data_via_api_step_1.r  
This script requires an API credential to properly utilize and depending on the domain details linked to the credentials will allow the user to access all the data included in that domain's database stored by Eco Couner.
## process_raw_counts_step_2.r  
This script processes the raw counts data downloaded in Step 1 formatting data elements like date and time and appending helpful temporal details like month and day of week.  
## apply_qaqc_step_3.r
This script applies Quality Assurance and Quality Control (QAQC) steps documented in [this report](https://www.oregon.gov/odot/Programs/ResearchDocuments/SPR_813Final-Nonmotorized.pdf) in Chapter 4.  Suspect daily data are flagged 
using four different flags including consecutive zeros, data outside expected bounds based on recent hsitory, excessively high values, and days of suspect data from a manually entered set of suspect data.  
## run_main.r  
This script is a main script used as a wrapper for the scripts 1 through 3 and also allows for running the basic data visualization script.  This data visualization is previewed in the screen shot below

### Data Visualization Preview
![Data Explorer](Data_Explorer_Screenshot.png)


# Contact
Josh Roll  josh.f.roll@odot.state.or.us  



