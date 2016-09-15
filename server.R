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
              values$df_data <- values$df_data[,1:2]
              columns <- c("amazon_merchant_id", "predicted_gmv")
              colnames(values$df_data) <- columns
              values$df_data <- values$df_data[!(is.na(values$df_data$amazon_merchant_id) | values$df_data$amazon_merchant_id == ""),] 
          }
        ###EBAY###
          if(input$platform == 2){
              values$df_data <- values$df_data[,1:2]
              columns <- c("ebay_username", "predicted_gmv")
              colnames(values$df_data) <- columns
              values$df_data <- values$df_data[!(is.na(values$df_data$ebay_username) | values$df_data$ebay_username == ""),] 
          }
      } else {
################
##LEADS UPDATE##
################
      ###AMAZON###
    	  if(input$platform == 1){
######Preparing columns in the right order and names
      		values$df_data <- values$df_data[,1:9]
      		columns <- c("amazon_merchant_id", "country", "first_name", "last_name", "email", "phone", 
                "agent_signature", "contact_details_update", "status")
      		colnames(values$df_data) <- columns
      #TO DO: sprawdzenie czy plik nie ma pozamienianych kolumn
      
######Deleting rows with empty id
      		values$df_data <- values$df_data[!(is.na(values$df_data$amazon_merchant_id) | values$df_data$amazon_merchant_id == ""),] 
      	}
      ###EBAY###   
      	if(input$platform == 2){
        	temp <- values$df_data[,1:9]
        	values$df_data <- temp
        	columns <- c("ebay_username", "country", "first_name", "last_name", "email", "phone", 
                "agent_signature", "contact_details_update", "status")
        	colnames(values$df_data) <- columns
        #TO DO: sprawdzenie czy plik nie ma pozamienianych kolumn
        
        ######Deleting rows with empty id
        	values$df_data <- values$df_data[!(is.na(values$df_data$ebay_username) | values$df_data$ebay_username == ""),] 
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

######Status filtering
      
      ##TO DO
      
######country filtering
      	
      values$df_data$country <- country_repair(values$df_data ,values$df_data$country ,input$country)
      
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