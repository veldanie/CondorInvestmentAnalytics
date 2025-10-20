#' get_token
#'
#' Get a bearen token to use post
#' @param url_request url request
#' @param username_req Username who needs token. 
#' @param password_req Password
#' @param secretpool_req Secretpool used to get token
#' @export
get_token <- function(url_request,username_req,password_req,secretpool_req="SecretPoolInvSura"){
  # Header
  headers <- add_headers(
    "Accept"= "*/*",
    "Content-Type" = "application/json",
    "Access-Control-Allow-Origin" = "*"
  )
  # Body
  body <- list(
    username = username_req,
    password = password_req,
    secretpool = secretpool_req
  )
  # Post petition
  response <- POST(
    url = url_request,
    config = headers,
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )
  # Check status
  if (status_code(response) != 200){
    stop(paste("Error en la solicitud: código", status_code(response)))
  }
  # Get response list R
  content_text <- content(response, as = "text", encoding = "UTF-8")
  response_data <- fromJSON(content_text)
  
  return(token = response_data$id_token)
}