email_repair <- function(data){
  data <- gsub('\\(at\\)', '@', data)
  idx <- grepl( "^[^@]+@[^@]+\\.", data)
  data[!idx] <- ""
  return(data)
}

columns_update <- function(id_name, data){
  data <- data[,1:9]
  columns <- c(id_name, "country", "first_name", "last_name", "email", "phone", 
               "agent_signature", "contact_details_update", "status")
  return(columns)
}

empty_rows <- function(data, column){
  return(data[!(is.na(column) | column == ""),])
}

