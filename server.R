library(shiny)
library(dplyr)
library(countrycode)
library(stringr)

options(shiny.maxRequestSize=30*1024^2) 

function(input, output) {
##Creating a data frame that is not reactive
values <- reactiveValues(df_data = NULL, df = NULL)
##When the button to upload the file is clicked we fill the data frame    
observeEvent(input$file, {
 values$df_data <- read.csv(input$file$datapath, sep = input$sep, stringsAsFactors = F)
 })

##Changing the separator
observeEvent(input$change, {
 values$df_data <- read.csv(input$file$datapath, sep = input$sep, stringsAsFactors = F)
 })
##Rendering relevant UI depending on event choice
output$ui <- renderUI({
  if (is.null(input$event))
  return()
    switch(input$event,
      "1" = {list(
        textInput("country", "Pick a country", placeholder = "For example: GB,US,IT,ES,CN"),
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
      "3" = NULL
      )
    })

#########################################
##When the go button is clicked we run the script to filter out the file    
#########################################

observeEvent(input$go, {

  ##############
  ##GMV UPDATE##
  ##############      
  if(input$event == 3) {
          ###AMAZON###
          if(input$platform == 1){
            values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
          }
          ###EBAY###
          else if(input$platform == 2){
            values$df_data <- empty_rows(values$df_data, values$df_data$ebay_username)
          }
        } 
  ################
  ##LEADS UPDATE##
  ################
  else if(input$event == 2) {
        ###AMAZON###
        if(input$platform == 1){
          values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
        }
        ###EBAY###   
        else if(input$platform == 2){
         values$df_data <- empty_rows(values$df_data, values$df_data$ebay_username)
       }
  ###COMMON CHANGES FOR UPDATES###
  
    
  ###Removing special characters
  values$df_data$first_name <- str_replace_all(values$df_data$first_name, "[^a-zA-Z0-9]", "")
  values$df_data$last_name <- str_replace_all(values$df_data$last_name, "[^a-zA-Z0-9]", "")  
    
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

  if (input$platform == 1){
    values$df_data$platform <- "amazon"
    values$df_data <- empty_rows(values$df_data, values$df_data$amazon_merchant_id)
    } else if (input$platform == 2){
      values$df_data$platform <- "ebay"
      values$df_data <- empty_rows(values$df_data, values$df_data$ebay_username)
    }
    else {
      values$df_data$platform <- "webstore"
    }
  ######Removing special characters
  values$df_data$first_name <- str_replace_all(values$df_data$first_name, "[^a-zA-Z0-9]", "")
  values$df_data$last_name <- str_replace_all(values$df_data$last_name, "[^a-zA-Z0-9]", "")
  ######Filtering Countries
  values$df_data$country <- get_country(values$df_data)
  values$df_data$country <- country_repair(values$df_data, values$df_data$country, input$country)
  ######Matching countries to empty language
          #TO DO
  ######Filtering phone numbers
  values$df_data$phone <- phone_repair(values$df_data$phone)
  ######Adding user input
  values$df_data$lead_source <- input$lead_source
  values$df_data$lead_status <- input$lead_status
  ######Filtering language
  values$df_data$language <- set_language(values$df_data)
  if (any(is.na(values$df_data$language) | values$df_data$language == "")) {
    values$df_data[(values$df_data$language == "" | is.na(values$df_data$language)), "language"] <- "en"
  }
  ######Filtering emails
  if("email" %in% colnames(values$df_data)){
    values$df_data$email <- email_repair(values$df_data$email)
  }
  }
  values$df <- build_df(input$event, input$platform, values$df_data)
  df_names <- colnames(values$df)
  
  for (i in colnames(values$df_data)){
    if(i %in% df_names){
      values$df[,i] <- values$df_data[,i]
    }else{
    }
  }
})

######Downloading the file
output$downloadData <- downloadHandler(filename = function() { 
  paste(event(input$event),'_',platform(input$platform),'_',as.character(Sys.Date()),'_raw','.csv', sep='') }, content = function(file){
    write.csv(values$df_data, row.names = F, file)
    }, contentType = 'csv')

output$downloadData2 <- downloadHandler(filename = function() { 
  paste(event(input$event),'_',platform(input$platform),'_',as.character(Sys.Date()), '_ready','.csv', sep='') }, content = function(file){
    write.csv(values$df, row.names = F, file)
  }, contentType = 'csv')

######Rendering the tables

output$df_data_out <- renderTable(head(values$df_data,5))
output$df_table <- renderTable(head(values$df,5))



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