#' query_database
#'
#' Make a query to database modelport_db using lambda query_database
#' @param url_database url request to query_database lambda
#' @param query Query SQL. 
#' @param url_token url request token
#' @param username_req Username who needs token. 
#' @param password_req Password
#' @param secretpool_req Secretpool used to get token
#' @param db_name Secretpool used to get token
#' @export
query_database <- function(url_database, query,
                           url_token, username_req, password_req, secretpool_req="SecretPoolInvSura",
                           db_name="wmdb"){
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
    query_string = query
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
  
  return(data = response_data_database$data)
}