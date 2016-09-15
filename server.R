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
      	options(scipen=999)
      	phones <- values$df_data$phone 
      	ind <- which(substring(phones, 1, 1) == 0)
      	y <- as.numeric(gsub("\\D", "", phones))
      	y[ind] <- paste0('0', y[ind])
      	phones <- y
      	idx <- nchar(phones) < 7
      	phones[idx | is.na(phones)] <- '' #or 000000000
      	values$df_data$phone <- phones
      
######Filling relevant columns with user input
      	idx <- (is.na(values$df_data$agent_signature) | values$df_data$agent_signature == '')
      	values$df_data$agent_signature[idx] <- input$agent
      	values$df_data$contact_details_update <- as.character(input$date)
      
######Mail filtering
      	
      	values$df_data$email <- gsub('\\(at\\)', '@', values$df_data$email)
      	idx <- grepl( "^[^@]+@[^@]+\\.", values$df_data$email)
      	values$df_data$email[!idx] <- ""
######Status filtering
      
      ##TO DO
      
######country filtering
      	library(countrycode)
      	
      	idx <- countrycode(values$df_data$country, "iso3c", "iso2c")
      	values$df_data$country[!is.na(idx)] <- idx[!is.na(idx)] 
      	if (any(!toupper(unique(values$df_data$country)) %in% countrycode_data$iso2c)) {
       		idx <- !toupper(values$df_data$country) %in% countrycode_data$iso2c
       		values$df_data$country[idx] <- input$country
      	}
      	if (any(is.na(values$df_data$country) | values$df_data$country == "")) {
      	    values$df_data[(values$df_data$country == "" | is.na(values$df_data$country)), "country"] <- input$country
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