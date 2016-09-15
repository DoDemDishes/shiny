email_repair <- function(data){
  data <- gsub('\\(at\\)', '@', data)
  idx <- grepl( "^[^@]+@[^@]+\\.", data)
  data[!idx] <- ""
  return(data)
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