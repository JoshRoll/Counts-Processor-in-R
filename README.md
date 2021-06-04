# Project Summary  
This project created information for the Oregon Department of Transportation (ODOT) Transportation System Action Plan (TSAP) which was summarized in a technical memo titled Pedestrian Injury and Social Equity in Oregon.  The technical memo can be accessed at the link below.    
https://www.oregon.gov/odot/Safety/Documents/Pedestrian_Safety_and_Social_Equity.pdf  

The data and scripts in this repository gather data from Fatal Accident Reporting System (FARS) and Census data and then calculates an age-adjusted population-based pedestrian death rates for each represented racial group to assess disparities.  These rates use the US population as the standard population and employ five years population data using person-years as the denominator.  The analysis script also calculates population-based injury rates for other modes including bicycle and motor vehicle.

# Script Details  
Three scripts are available in this repository.  The download_format_FARS_data.r script downloads and prepares FARS data for injury rate analysis.  The download_prepare_census_population_data.r script uses R's Census API tools to download 
and prepare Census data by age and race for states. The script analyze_FARS_race.r combines the FARS data and Census population data to analyze the rate of injury per 100,000 person-years.  The script can be changed to analyze
any of the 50 state by toggling a few inputs (see in-line comments).  This script calculates confidence intervals so users can communicate the uncertainty in the measured outcomes.  This script also aggregates  Black, Indigenous, 
and People of Color race categories into a single group in order to measure the injury rate with more certainty than what is possible with disaggregate BIPOC groups (at least for Oregon).  

## download_format_FARS_data.r  
This script downloads raw FARS data from NHTSA FTP site and formats it for analysis.  Working with all the files through the NHTSA FTP site can be challenging and this script is meant to simplify pulling all the files and preparing for 
this analysis.  Other analyses would likely require preparing the data in different way but this should get you started. Starting in 2019 NHTSA stopped putting the Race data element in the person records and you know have to join it from 
a separate file becuase NHTSA now takes multiple races, if reported on death certificate, and includes them in this new race table.  This script only uses the first reported race from the race table to be consistent with past data but for 
multi-race persons these data would be need to be processed differently.  This script works in 3 steps:  
### Step 1 -  Download RAW Data - Download zipped files and unzip them to local drive
### Step 2 -  Process Person table records and prepare for analysis
### Step 3 -  Finalize formatting to make merging with race and age cohort Census data simple  

## download_prepare_census_population_data.r  
This script uses R's Census API tools to download and format state level population data for use in calculating age-adjusted population-based fatal injury rates for traffic injury. If other Census data elements are of interest beyond population
by age and race this script would need to be modified.   

## analyze_fars_race_prod.r
This script combines FARS person level fatal death data with Census population data to calculate age-adjusted population-based fatal injury rates by racial category.  The analysis uses the US population as the standard population to 
weight the rates by age cohort in order to make the composite rates by race comparable across the US.  A composite BIPOC rate is constructed to improve confidence in the point estimates for these non-White racial categories since some disaggregate BIPOC groups have small numbers of either population, injuries, or both.  

## Results  
Information featured in charts below are examples of information produced by this repository.  Repository materials are capable of producing other information on rates for other modes (bicycle, motor vehicle).
### Fatal Pedestrian Injury Rates 2014-2018
![Ped_Rates_2014-2018](www/Ped_Rates_2014-2018.png)  
### Fatal Pedestrian Injury Rates Over Time
![Ped_Rates_2014-2018](www/Ped_Rates_Over_Time.png)  
