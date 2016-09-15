email_repair <- function(data){
  data <- gsub('\\(at\\)', '@', data)
  idx <- grepl( "^[^@]+@[^@]+\\.", data)
  data[!idx] <- ""
  return(data)
}
