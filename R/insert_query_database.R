#' insert_query_database
#'
#' Make a query to database modelport_db using lambda query_database
#' @param url_database url request to query_database lambda
#' @param df_new_register DataFrame with info to insert
#' @param db_table table insert SQL. 
#' @param url_token url request token
#' @param username_req Username who needs token. 
#' @param password_req Password
#' @param secretpool_req Secretpool used to get token
#' @param db_name Secretpool used to get token
#' @export
insert_query_database <- function(url_database, df_new_register, db_table,
                                  url_token, username_req, password_req, secretpool_req="SecretPoolINVPROD",
                                  db_name = "wmdb") {
  if (nrow(df_new_register) == 0) {
    stop("No se puede hacer la inserción de un data.frame vacío")
  }
  
  values_to_insert <- vector("character", nrow(df_new_register))
  
  for (i in seq_len(nrow(df_new_register))) {
    row <- df_new_register[i, ]
    
    row_sql <- sapply(names(row), function(col) {
      value <- row[[col]]
      if (is.numeric(value)) {
        as.character(value)
      } else if (inherits(value, "Date") || inherits(value, "POSIXt")) {
        paste0("'", as.character(value), "'")
      } else if (is.na(value)) {
        "NULL"
      } else {
        paste0("'", gsub("'", "''", as.character(value)), "'")
      }
    })
    
    values_to_insert[i] <- paste0("(", paste(row_sql, collapse = ", "), ")")
  }
  
  string_insert <- paste(values_to_insert, collapse = ", ")
  query_insert <- sprintf("INSERT INTO %s VALUES %s", db_table, string_insert)
  
  bearen_token <- get_token(url_token,username_req,password_req)
  
  # Header
  headers_database <- add_headers(
    "Accept"= "*/*",
    "Content-Type" = "application/json",
    "Access-Control-Allow-Origin" = "*",  # ← reemplazá con tu token real o quitá si no lo usás,
    "Authorization" = bearen_token
  )
  # Body
  body_database <- list(
    database = db_name,
    query_string = query_insert
  )
  # Post petition
  response_database <- POST(
    url = url_database,
    config = headers_database,
    body = toJSON(body_database, auto_unbox = TRUE),
    encode = "raw"
  )
  # Check status
  if (status_code(response_database) != 200){
    stop(paste("Error en la solicitud: código", status_code(response_database)))
  }
  # Get response list R
  content_text_database <- content(response_database, as = "text", encoding = "UTF-8")
  response_data_database <- fromJSON(content_text_database)
  
  return(sprintf("Inserción correcta en la tabla %s", db_table))
}
