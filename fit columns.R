library(stringr)
## CONTACT UPDATE AMAZON##

##READ FILE##
my_data <- read.csv("C:/Users/Marta Klimaszewska/Documents/shiny files/amazon_contact_update.csv")
my_data[my_data == ""] <- NA
for (i in 1:ncol(my_data)){
  my_data[,i] <- gsub('\\(at\\)', '@', my_data[,i])
}
head(my_data)
##CREATE EMPTY DF##
temp <- data.frame(matrix("", ncol = 9, nrow = nrow(my_data))) 

##ASSIGN COLNAMES##

colnames(temp) <- c("amazon_merchant_id", "country", "first_name", "last_name", "email", "phone", 
                    "agent_signature", "contact_details_update", "status")

## FIND AVG nchar in each column ##
col_char_len <- c()
find_mean <- function(data){
  for (i in 1:ncol(data))
    col_char_len[i] <- mean(nchar(as.character(data[,i])), na.rm = TRUE)
    return(col_char_len)
  }

col_char_len <- find_mean(my_data)

## INDENTIFY AND ASSIGN amazon_merchant_id ##
for (i in 1:length(col_char_len)){
  if ((!is.nan(col_char_len[i])) & (col_char_len[i]>13) & (col_char_len[i]<14) & 
      (substring(toupper(my_data[min(which(!is.na(my_data[,i]))),i]), 1, 1) == "A")) {
    temp$amazon_merchant_id <- my_data[,i]
    print(1)
    cond <- FALSE
  } else {
    print(2)
  }
} 

## IDENTIFY AND ASSIGN country ##
 
for (i in 1: length(col_char_len)){
  if ((!is.nan(col_char_len[i])) & (col_char_len[i]>2) & (col_char_len[i]<3)){
    temp$country <- my_data[,i]
  } else{
  }
}

## IDENTIFY AND ASSIGN email ##

num_at <- c()
for (i in 1:ncol(my_data)){
  num_at[i] <- table(str_detect(my_data[,i], "@"))["TRUE"]
}
temp$email <- my_data[,which.max(num_at)]

head(temp)