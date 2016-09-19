email_repair <- function(data){
  data <- gsub('\\(at\\)', '@', data)
  idx <- grepl( "^[^@]+@[^@]+\\.", data)
  data[!idx] <- ""
  return(data)
}

columns_update <- function(id_name, data, event){
  if (event == 2){
    data <- data[,1:9]
    columns <- c(id_name, "country", "first_name", "last_name", "email", "phone", 
     "agent_signature", "contact_details_update", "status")
  } 
  if (event == 3){
    data <- data[,1:2]
    columns <- c(id_name, "predicted_gmv")
  }
  return(columns)
}

empty_rows <- function(data, column){
  return(data[!(is.na(column) | column == ""),])
}

phone_repair <- function(data){
	options(scipen=999)
  phones <- data
  ind <- which(substring(phones, 1, 1) == 0)
  y <- as.numeric(gsub("\\D", "", phones))
  y[ind] <- paste0('0', y[ind])
  phones <- y
  idx <- nchar(phones) < 7
  phones[idx | is.na(phones)] <- '' #or 000000000
  data <- phones
  return(phones)
}

country_repair <- function(df,data,input){
  library(countrycode)
  idx <- countrycode(data, "iso3c", "iso2c")
  data[!is.na(idx)] <- idx[!is.na(idx)]
  if (any(!toupper(unique(data)) %in% countrycode_data$iso2c)) {
    idx <- !toupper(data) %in% countrycode_data$iso2c
    data[idx] <- input
  }
  if (any(is.na(data) | data == "")) {
    df[(data == "" | is.na(data)), "country"] <- input
  }
  return(data)
}

platform_update <- function(batch_platform, data, column){
  if (any(is.na(column) | column == "")) {
    data[is.na(column), "platform"] <- batch_platform
    data[column == "", "platform"] <- batch_platform
  }
  return(data)
}

build_df <- function(event, platform, my_data){
  if(event == 1){
    ##NEW LEADS
    columns <- c("company_name", "amazon_merchant_id", "amazon_feedback", "ebay_username"
     ,"webstore_url", "webstore_traffic", "category", "platform"
     ,"country", "first_name", "last_name", "phone", "email", "language"
     ,"lead_source", "lead_status", "marketing_campaign_2", "webstore_platform"
     ,"predicted_ebay_gmv", "predicted_amazon_gmv", "predicted_webstore_gmv"
     ,"ebay_item_location", "ebay_posts_to")
    df <- data.frame(matrix("", ncol = length(columns), nrow = nrow(my_data)), stringsAsFactors = FALSE)
    colnames(df) <- columns
  }
  else if(event == 2){
    if(platform == 1){
      ##AMAZON UPDATE
      columns <- c("amazon_merchant_id", "country", "first_name", "last_name", "email", "phone", 
      "agent_signature", "contact_details_update", "status")
      df <- data.frame(matrix("", ncol = length(columns), nrow = nrow(my_data)), stringsAsFactors = FALSE)
      colnames(df) <- columns
    }
    else if(platform == 2){
      ##EBAY UPDATE
      columns <- c("ebay_username", "country", "first_name", "last_name", "email", "phone", 
      "agent_signature", "contact_details_update", "status")
      df <- data.frame(matrix("", ncol = length(columns), nrow = nrow(my_data)), stringsAsFactors = FALSE)
      colnames(df) <- columns
    }
    else{
      ##TO DO WEBSTORE
    }
  }
  else{
    ##GMV UPDATE
    columns <- c("ebay_username", "country", "first_name", "last_name", "email", "phone", 
    "agent_signature", "contact_details_update", "status")
    df <- data.frame(matrix("", ncol = length(columns), nrow = nrow(my_data)), stringsAsFactors = FALSE)
    colnames(df) <- columns
  }

  return(df)
}