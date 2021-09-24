#' Multi-panel map theme
#' @export

theme_multi_map <- function() {
  return(theme_base() %+replace%
           theme(panel.background = element_blank(),
                 panel.border = element_rect(color = "black", fill = NA),
                 panel.grid = element_blank(),
                 plot.title = element_text(size = 9, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                 axis.title = element_blank(),
                 axis.text = element_text(size = 9),
                 legend.position = "none",
                 plot.background = element_rect(fill = NA, color = NA)))
}

#' Retrieve SQL query string from .sql files
#' 
#' This function extracts a query string from simple .sql file. The SQL file should only contain a comment block at the top and the SQL query itself.
#' 
#' @param sql_path File path to .sql file as a character vector.
#' @return Returns an SQL statement as a character vector, which can be executed on a database connection using functions in the RODBC or ROracle packages.
#' @export

sql_to_rqry <- function(sql_path) {
  in_string <- readr::read_file(sql_path)
  in_string <- sub("/*.*/", "", in_string)
  out_string <- stringr::str_replace_all(in_string, pattern = "\r\n", replacement = " ")
  
  return(out_string)
}


#' Create a database connection using RODBC
#' 
#' A function that accepts a data source name, username, and password to establish returns an Oracle DataBase Connection (ODBC) as an RODBC class in R.
#' 
#' @param schema Data source name (DSN) as a character vector.
#' @return An RODBC class ODBC connection. 
#' @export

get_connected <- function(schema = NA){
  (echo = FALSE)
  if(is.na(schema)) {
    schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
  }
  username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                 uid = paste(username),
                                 pwd = paste(password),
                                 believeNRows = FALSE)
}