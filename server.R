library(shiny)

options(shiny.maxRequestSize=30*1024^2) 

function(input, output) {
##We are creating a data frame that is not reactive
    values <- reactiveValues(df_data = NULL)
##When the button to upload the file is clicked we fill the data frame    
    observeEvent(input$file, {
    	values$df_data <- read.csv(input$file$datapath, sep = input$sep, stringsAsFactors = F)
    })
    
#########################################    
##When the go button is clicked we run the script to filter out the file    
#########################################

##Changing the separator
    observeEvent(input$change, {
    	values$df_data <- read.csv(input$file$datapath, sep = input$sep, stringsAsFactors = F)
    })
##Rendering relevant UI depending on event choice
output$ui <- renderUI({
    if (is.null(input$event))
      return()

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client. 
    switch(input$event,
      "1" = {list(
        selectInput("lead_source", "Pick the source of leads", c("Databse scrapinghub",
                                                                 "Manual Scraping ", "Social Media",
                                                                 "Databse BuiltWith","Blog",
                                                                 "Parnership","Advertisement",
                                                                 "TradeShow")),
        selectInput("lead_status", "Pick the status of leads", c("Marketing Nurturing","Processing"))
            )},
      "2" = {list(
        textInput("agent", "Pick an Agent", placeholder = "For example: GeorgeM"),
        textInput("country", "Pick a country", placeholder = "For example: GB,US,IT,ES,CN"),
        dateInput("date", "Pick update date")
      )},
      "3" = sliderInput("dynamic", "webstore",
                             min = 1, max = 20, value = 10)
    )
  })

    
##Combined changes    
    observeEvent(input$go, {
##############
##GMV UPDATE##
##############      
      if(input$event == 3) {
        ###AMAZON###
          if(input$platform == 1){
              colnames(values$df_data) <- columns_update("amazon_merchant_id", values$df_data, input$event)
              values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
          }
        ###EBAY###
          else if(input$platform == 2){
            colnames(values$df_data) <- columns_update("ebay_username", values$df_data, input$event)
            values$df_data <- empty_rows(values$df_data, values$df_data$ebay_username)
          }
      } 
################
##LEADS UPDATE##
################
      else if(input$event == 2) {
      ###AMAZON###
    	  if(input$platform == 1){
      		colnames(values$df_data) <- columns_update("amazon_merchant_id", values$df_data, input$event)
      		values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
      	}
      ###EBAY###   
      	else if(input$platform == 2){
      	  colnames(values$df_data) <- columns_update("ebay_username", values$df_data, input$event)
        	values$df_data <- empty_rows(values$df_data, values$df_data$ebay_username)
      	}
###COMMON CHANGES###
        
######Filtering phones
        values$df_data$phone <- phone_repair(values$df_data$phone)
      
######Filling relevant columns with user input
      	idx <- (is.na(values$df_data$agent_signature) | values$df_data$agent_signature == '')
      	values$df_data$agent_signature[idx] <- input$agent
      	values$df_data$contact_details_update <- as.character(input$date)
      
######Mail filtering
      	values$df_data$email <- email_repair(values$df_data$email)

######country filtering
      values$df_data$country <- country_repair(values$df_data ,values$df_data$country ,input$country)
      }
#############
##NEW LEADS##
#############
      else if (input$event == 1){
###data frame template###        
        columns <- c("company_name", "amazon_merchant_id", "amazon_feedback", "ebay_username"
                     ,"webstore_url", "webstore_traffic", "category", "platform"              
                     ,"country", "first_name", "last_name", "phone", "email", "language"
                     ,"lead_source", "lead_status", "marketing_campaign_2", "webstore_platform"      
                     ,"predicted_ebay_gmv", "predicted_amazon_gmv", "predicted_webstore_gmv" 
                     ,"ebay_item_location", "ebay_posts_to")
        
        values$df_data <- values$df_data[,1:23]
        colnames(values$df_data) <- columns
        if (input$platform == 1){
          values$df_data <- platform_update("amazon", values$df_data, values$df_data$platform)
          values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
        } else if (input$platform == 2){
          values$df_data <- platform_update("ebay", values$df_data, values$df_data$platform)
          values$df_data <- empty_rows(values$df_data, values$df_data$ebay_username)
        }
        else {
          values$df_data <- platform_update("webstore", values$df_data, values$df_data$platform)
        }
######Filtering Countries
        values$df_data$country <- country_repair(values$df_data,values$df_data$country, input$country)
######Matching countries to empty language
        #TO DO
######Filtering phone numbers
        values$df_data$phone <- phone_repair(values$df_data$phone)
######Filtering emails
        values$df_data$email <- email_repair(values$df_data$email)
      }
    })

######Downloading the file
    output$downloadData <- downloadHandler(filename = function() { 
      paste(input$date, '.csv', sep='') }, content = function(file)
      	{write.csv(values$df_data, row.names = F, file)}, contentType = 'csv')
    
    output$df_data_out <- renderTable(head(values$df_data,10))
    
######Table with sum of rows with phone, mail, mail + phone
    rowSummary <- reactive({
      
      # Compose data frame  
      	data.frame(
        	Name = c("No. of rows with phone", 
                "No. of rows with mail",
                "No. of rows with mail + phone"),
        	Value = as.character(c(sum(values$df_data$phone != ""), 
                sum(values$df_data$email != ""),
                sum(values$df_data$email != "" & values$df_data$phone != ""))), 
        	stringsAsFactors=FALSE, row.names = NULL)
    }) 
    
    output$values <- renderTable({
      	rowSummary()
    })
}