library(shiny)

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
          if(input$platform == 2){
            colnames(values$df_data) <- columns_update("ebay_username", values$df_data, input$event)
            values$df_data <- empty_rows(values$df_data, values$df_data$ebay_username)
          }
      } 
################
##LEADS UPDATE##
################
      if(input$event == 2) {
      ###AMAZON###
    	  if(input$platform == 1){
      		colnames(values$df_data) <- columns_update("amazon_merchant_id", values$df_data, input$event)
      		values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
      	}
      ###EBAY###   
      	if(input$platform == 2){
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
      if (input$event == 1){
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
          batch_platform <- "amazon"
          values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
        } if (input$platform == 2){
          batch_platform <- "ebay"
          values$df_data <- empty_rows(values$df_data, values$ebay_username)
        }
        else {
          batch_platform <- "webstore"
        }
      
        if ( any( is.na(values$df_data$platform) | values$df_data$platform == "")) {
          values$df_data[is.na(values$df_data$platform), "platform"] <- batch_platform
          values$df_data[values$df_data$platform == "", "platform"] <- batch_platform
        }
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