





#Define the Server 
server <- function(input, output, session) {
  
  
  #---------------------------------------------------------------------------------------------------------------------------
  #Develop filtered data sets
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Daily records Location Id data
  ###########################################
  Filtered_Data_Location_Id <- reactive({
    req(!(is.null(input$Date_Select_Location_Id)))
    Selected_Dates. <- seq.Date(input$Date_Select_Location_Id[1],as.Date(input$Date_Select_Location_Id[2]),1)
    Daily_Location_Id..[Daily_Location_Id..$Device_Name%in%input$Location_Select & 
                              Daily_Location_Id..$Year%in%input$Year_Select_Location_Id & 
                              Daily_Location_Id..$Month%in%input$Month_Select_Location_Id & 
                              Daily_Location_Id..$Date%in% Selected_Dates. & 
                              Daily_Location_Id..$User_Type_Desc%in%input$User_Type_Select_Location_Id & 
                              Daily_Location_Id..$Error_Code_Desc%in%input$Error_Code_Select_Location_Id,]
    
  })
  
  
  #---------------------------------------------------------------------------------------------------------------------------
  #Data Tables
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Location Id Daily records #################
  ###########################################
  observe({
    output$Daily_Table_Location_Id <- renderDataTable(
      Filtered_Data_Location_Id()
    )
    #Close observe  
  })
  
  #---------------------------------------------------------------------------------------------------------------------------
  #Year Select
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Location Id Daily records ##################
  ###########################################
  observe({
    output$Year_Select_Location_Id <- renderUI({
      #Create a vector  
      Select_Years. <- unique(Daily_Location_Id..$Year[Daily_Location_Id..$Device_Name%in%input$Location_Select])
      #Make null if nothing is selected    
      if (is.null(Select_Years.)) Select_Years. <- character(0)
      pickerInput(session,inputId = "Year_Select_Location_Id", label = "Select Year(s)",
                  choices = Select_Years., 
                  options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                  multiple = TRUE,selected = Select_Years.)
      #Close rendorUI
    })
    #Close observe  
  })
  #---------------------------------------------------------------------------------------------------------------------------
  #Month Select
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Location Id Daily records ############################
  ###########################################
  observe({
    output$Month_Select_Location_Id <- renderUI({
      #Create a vector  
      Select_Months. <- unique(Daily_Location_Id..$Month[Daily_Location_Id..$Device_Name%in%input$Location_Select & 
                                                               Daily_Location_Id..$Year%in%input$Year_Select_Location_Id])
      Select_Months. <- month.name[month.name%in%Select_Months.]
      # Can use character(0) to remove all choices
      if (is.null(Select_Months. ))Select_Months. <- character(0)
      pickerInput(session,inputId = "Month_Select_Location_Id", label = "Select Month(s)", 
                  choices = Select_Months., 
                  options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"),
                  multiple = TRUE,selected = Select_Months.)
      
      #Close rendorUI
    })
    #Close observe  
  })
  
  #---------------------------------------------------------------------------------------------------------------------------
  #User Type Select
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Sub Location Id Daily records ############################
  ###########################################
  observe({
    output$User_Type_Select_Location_Id <- renderUI({
      User_Types. <- Daily_Location_Id..[["User_Type_Desc"]][Daily_Location_Id..$Device_Name %in% input$Location_Select]
      pickerInput(inputId = "User_Type_Select_Location_Id", label = "Select User Type(s)", choices = as.character(unique(User_Types.)), 
                  options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple = TRUE,selected = as.character(unique(User_Types.)))
    })
  }) 
  
  # #---------------------------------------------------------------------------------------------------------------------------
  # #Facility Type Select
  # #---------------------------------------------------------------------------------------------------------------------------
  # ###########################################
  # #Sub Location Id Daily records #################
  # ###########################################
  # observe({
  #   output$Facility_Type_Select_Sub_Location_Id <- renderUI({
  #     Facility_Types. <- Daily_Sub_Location_Id..[["Facility_Type"]][Daily_Sub_Location_Id..$Sub_Location_Id %in% input$Sub_Location_Select]
  #     pickerInput(inputId = "Facility_Type_Select_Sub_Location_Id", label = "Select Facility Type(s)", choices = as.character(unique(Facility_Types.)), 
  #                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple = TRUE,selected = as.character(unique(Facility_Types.)))
  #   })
  # })
  # ###########################################
  # #Location Id Daily records #################
  # ###########################################
  # observe({
  #   output$Facility_Type_Select_Location_Id <- renderUI({
  #     Facility_Types. <- Daily_Location_Id..[["Facility_Type"]][Daily_Location_Id..$Device_Name %in% input$Location_Select]
  #     pickerInput(inputId = "Facility_Type_Select_Location_Id", label = "Select Facility Type(s)", choices = as.character(unique(Facility_Types.)), 
  #                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple = TRUE,selected = as.character(unique(Facility_Types.)))
  #   })
  # })
 
   #---------------------------------------------------------------------------------------------------------------------------
  #Date Select
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Sub Location Id Daily records ############################
  ###########################################
  observe({
    output$Date_Select_Location_Id <- renderUI({
      #Create a vector of available dates based on selected location
      req(nrow(Daily_Location_Id..) > 0)
      Dates. <- as.Date(Daily_Location_Id..[["Date"]][Daily_Location_Id..$Device_Name %in% input$Location_Select])
      dateRangeInput("Date_Select_Location_Id", "Input Date Range",
                     start = Dates.[1],
                     end = Dates.[length(Dates.)]
      )
    })
  })
  
  #---------------------------------------------------------------------------------------------------------------------------
  #Error Code Select
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Location Id Daily records #################
  ###########################################
  observe({
    output$Error_Code_Select_Location_Id <- renderUI({
      Error_Codes. <- Daily_Location_Id..[["Error_Code_Desc"]][Daily_Location_Id..$Device_Name %in% input$Location_Select]
      pickerInput(inputId = "Error_Code_Select_Location_Id", label = "Select Error Code(s) to Include", choices = as.character(unique(Error_Codes.)), 
                  options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple = TRUE,selected = as.character(unique(Error_Codes.)))
    })
  })
  
  
  
  
  #---------------------------------------------------------------------------------------------------------------------------
  #Charts
  #---------------------------------------------------------------------------------------------------------------------------
  ###########################################
  #Daily Location Id records #################
  ###########################################
  observe({ 
    output$Daily_Plot_Location_Id <- renderPlotly({
      #MAke a copy of selected data for brevity's sake
      dat <- Filtered_Data_Location_Id()
      #Trouble shooting---
      #print(head(dat))
      #dat <- Daily_Location_Id..[Daily_Location_Id..$Device_Name%in%"HAWTHORNE BR north side",]
      #dat <- dat[dat$Month%in%"March",]
      #Check to make sure data is present 
      req(nrow(dat) > 0)
      #Remove excessive values
   #   dat <- dat[!(dat$Excessive_Value%in%"Excessive Value"),]
     # dat$Date <- as.character(dat$Date)
      #Look up descriptive locaiton name
      Location_Description <-unique(dat$Device_Name)
        #Initialze chart
      Daily_Plot <- ggplot(dat,aes(x = Date, y = Counts, fill = Is_Weekday), aes_string(label = "Error_Code_Desc",label2 = "Potential_Special_Event",
                 label3 = Is_Holiday,label4 = "Weekday", label5 = "Date"))  +   	
                 geom_bar(stat = "identity", position = 'dodge', aes(text = paste("Error Code: ", Error_Code_Desc,sep=""))) +
                theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) +
                 facet_wrap(~User_Type_Desc, nrow = 3, scales = "free") +
                 theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=.5)) +
                 ggtitle(Location_Description) +
                scale_y_continuous(labels=comma) + 
                 ylab("")  +  
                 xlab("")  +  
                 labs(fill = "Weekday\nStatus") + 
                 theme(plot.title = element_text(hjust = 0.5)) + 
                 theme(strip.text = element_text(size=8, lineheight=5))
      #Logic for deciding axis labels
      #if(nrow(dat< 180))
        #ggplotyl(Daily_Plot + scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x),by = "1 month")))
      if( nrow(dat) < 360 ){
        ggplotly(Daily_Plot + scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x),by = "1 month")),tooltip = c("x","y","fill","text"))
      }
     if( nrow(dat) > 360 & nrow(dat) <720 ){
        ggplotly(Daily_Plot + scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x),by = "3 month")),tooltip = c("x","y","fill","text"))
      }
      if( nrow(dat) > 720 ){
        ggplotly(Daily_Plot + scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x),by = "1 year")),tooltip = c("x","y","fill","text"))
      }
  
      
      #End output
    })
    #End Observe
  })
  ###########################################
  #monthly Location Id records #################
  ###########################################
  observe({ 
    output$Monthly_Plot_Location_Id <- renderPlotly({
      #MAke a copy of selected data for brevity's sake
      dat <- Filtered_Data_Location_Id()
      #Trouble shooting---
      #print(head(dat))
      #dat <- Daily_Location_Id..[Daily_Location_Id..$Device_Name%in%"HAWTHORNE BR north side",]
      dat <- dat %>% group_by(Device_Name,User_Type_Desc, Year, Month) %>% summarise(Counts = sum(Counts), Days_of_Data = length(Date)) %>% 
        mutate(Month_Year = paste(Month, Year, sep ="-")) 
      Years. <- unique(dat$Year) 
      Months. <- unique(dat$Month) 
      dat <- dat %>% mutate(Year = factor(Year, Years.), Month = factor(Month, month.name))
      #Determien color pallete
      Colors. <- colorRampPalette(c("lightblue", "darkgreen"))(12)
      #Check to make sure data is present 
      req(nrow(dat) > 0)
      #Remove excessive values
      #   dat <- dat[!(dat$Excessive_Value%in%"Excessive Value"),]
     # dat$Date <- as.character(dat$Date)
      #Look up descriptive locaiton name
      Location_Description <-unique(dat$Device_Name)
      #Set up axis parameters depending on amount of data 
      labels <- dat$Date[seq(0, nrow(dat), by= 10)]
      #Initialze chart
      Monthly_Plot <- ggplot(dat,aes(x = Year, y = Counts, fill = Month, group = Month), aes_string(label1 = "Year", label2 = "Days_of_Data")) +
          geom_col(colour="black",width=.95,  position=position_dodge(.95), aes(text = paste("Days of Data: ", Days_of_Data,sep=""))) +
          #geom_text(aes(x = Year, y = Counts, ) +
          scale_fill_manual( values=Colors. ) +
          scale_y_continuous(labels=comma) + 
          facet_wrap(~User_Type_Desc, nrow = 2, scales = "free") +
          theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=.5)) +
          ggtitle(Location_Description) +
          ylab("")  +  
          xlab("")  +  
          labs(fill = "Month") + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme(strip.text = element_text(size=8, lineheight=5))
      #Call chart
      ggplotly(Monthly_Plot,tooltip = c("x","y","fill","text"))
      
    
    
    
      #End output
    })
    #End Observe
  })
  #Close Server    
}
