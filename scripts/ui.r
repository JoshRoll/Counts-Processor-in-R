


######################################
######################################
#Build ODOT basic viz
######################################
######################################
#Version 14
##########################
#Visualization Elements:
#1) Daily plot
#2) Monthly plot
#3) Data table



#Set up User Interface
#---------------------------------------------
ui <- fluidPage(
  #titlePanel(title=div(img(src="ODOT_Logo.jpg"))),
  #titlePanel(title=div(img(src="ODOT_Logo.jpg")),"Oregon Bicycle & Pedestrain Traffic Count Data Viewer"),
  titlePanel(div("Oregon DOT Bicycle & Pedestrain Traffic Counts Explorer", 
                 img(height = 105, width = 600, 
                     src = "ODOT_Logo.jpg", 
                     class = "pull-right"))),
  tabsetPanel(
    #Sub Location Id Data Explorer Tab
    tabPanel("Daily and Monthly Data",
             #Define row # 1--
             #Daily plot
             fluidRow(
               column(12,plotlyOutput("Daily_Plot_Location_Id",height = 400))),
             #Define row # 2--
             #Monthly plot column 1
             fluidRow(
               column(6,plotlyOutput("Monthly_Plot_Location_Id",height = 400)),
               #Temporal options column 2
               column(2,
                      h4("Select Temporal Details"),
                      selectInput("Location_Select", "Select Location Name",unique(Daily_Location_Id..$Device_Name)),
                      uiOutput("Year_Select_Location_Id"),
                      uiOutput("Month_Select_Location_Id")),
               #Site Options options column 3
               column(2,
                      h4("Select User/Facility Details"),
                      uiOutput("User_Type_Select_Location_Id"),
                      uiOutput("Facility_Type_Select_Location_Id"),
                      uiOutput("Date_Select_Location_Id")
                      ),
               #Error Code options column 4
               column(2,
                      h4("Select Errors"),
                      uiOutput("Error_Code_Select_Location_Id"))
               
             #close row
             ),
             #Daily records tables
             fluidRow(column(width = 12,
                             h4("Data Table"),
                             dataTableOutput("Daily_Table_Location_Id"))
             #Close row
              )
      #Close panel
      )
    #Close setPanel
    )
    #Close page
  )
  
