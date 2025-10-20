

#' Downloads data from DataLicense database
#'
#' @param target_data Dataframe containing the following columns:
#' ticker = column containing tickers that will be downloaded
#' alias = column containing the alias for each ticker (name with which it will be exported)
#' field = field to be downloaded for the ticker column
#' output_file_name = name of the .csv output file
#' file_type = timeseries or cross_section
#' @param output_path path to which the output should be exported. If NULL, returns dataframe.
#' @return dataframe
#' @export

retrieve_data <- function(target_data, output_path = NULL){
  library(tidyr)
  library(odbc)
  library(DBI)
  library(dbplyr)
  library(dplyr)
  library(purrr)
  # Crea conexión a DataLicense
  db <- try(DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", UID='user_sura', PWD='Sur4dano.',
                           Server='169.61.38.18', database = 'DataLicense', Port = 1433))
  # Tabla que trae los datos partickers y campos, por ejemplo: TOT_RETURN_INDEX_GROSS_DVDS para un determinado ETF
  df_series <- db %>%
    tbl("tbl_DatosBloomberg") %>%
    select(id_Ticker, id_Campo, dtmFecha, intValor, vchValor)
  # Tabla que contiene todos los tickers disponibles en la base de datos
  df_tickers <- db %>%
    tbl("tbl_Ticker") %>%
    select(id, vchNombre)
  # Tabla que contiene todos los campos disponibles en la base de datos
  df_fields <- db %>%
    tbl("tbl_Campo") %>%
    select(id, vchNombre)

  # Tickers y campos requeridos para la aplicaci?n
  req_tickers <- unique(target_data$ticker)
  req_fields <- unique(target_data$field)

  # IDs de campos requeridos
  existing_fields <- unique(as.vector(pull(df_fields, 2))[as.vector(pull(df_fields, 2)) %in% req_fields])
  field_ids <- which(as.vector(pull(df_fields, 2)) %in% existing_fields)
  names(field_ids) <- existing_fields
  missing_fields <- unique(req_fields[!(req_fields %in% existing_fields)])
  # IDs de tickers requeridos
  existing_tickers <- unique(as.vector(pull(df_tickers, 2))[as.vector(pull(df_tickers, 2)) %in% req_tickers])
  ticker_ids <- which(as.vector(pull(df_tickers, 2)) %in% existing_tickers)
  names(ticker_ids) <- existing_tickers
  missing_tickers <- unique(req_tickers[!(req_tickers %in% existing_tickers)])
  # Filtrar tabla para dejar solo tickers que existen en DataLicense
  target_data <- target_data %>%
    filter(ticker %in% existing_tickers)
  # Asignar c?digos de identificaci?n a tickers y campos
  target_data$field_id <- field_ids[match(target_data$field, names(field_ids))]
  target_data$ticker_id <- ticker_ids[match(target_data$ticker, names(ticker_ids))]
  target_data$aux <- paste0(target_data$field_id, '|', target_data$ticker_id)

  print(paste("The following ticker was not found in the DB: ", missing_tickers))
  # Extracci?n de datos
  for(output_file in unique(target_data$output_file_name)){
    file_data <- target_data %>% filter(output_file_name == output_file)
    if(length(unique(file_data$file_type)) != 1){
      stop("Output file can only be of one type, either 'timeseries' or 'cross_section'")
    }
    if(unique(file_data$file_type == 'timeseries')){
      output_data <- data.frame(df_series %>%
                                  filter(id_Ticker %in% !!file_data$ticker_id &
                                           id_Campo %in% !!file_data$field_id) %>%
                                  mutate(aux = concat(id_Campo, '|', id_Ticker)) %>%
                                  filter(aux %in% !!file_data$aux & !(is.na(intValor))) %>%
                                  arrange(id_Ticker, dtmFecha))
      all_data <- list()
      for(element in unique(output_data$aux)){
        all_data[[element]] <- output_data %>%
          filter(aux == element) %>%
          select(dtmFecha, intValor)
      }
      list_names <- strsplit(names(all_data), split = '|', fixed = TRUE)
      headers <- c(rbind(sapply(lapply(list_names, '[[', 2), function(x, ...){
        ticker_names <- names(ticker_ids)[match(as.integer(x), ticker_ids)]
        ticker_aliases <- file_data$alias[match(ticker_names, file_data$ticker)]
        return(ticker_aliases)
        }),
        sapply(lapply(list_names, '[[', 1), function(x, ...){
          names(field_ids)[match(as.integer(x), field_ids)]
        })))
      output_df <- reduce(all_data, merge, by = 'row.names', all = TRUE, sort = FALSE)
      output_df <- output_df[, colnames(output_df) != 'Row.names']
      colnames(output_df) <- c(1:ncol(output_df))
      for(i in seq(1, length(headers), by = 2)){
        output_df[, i:(i + 1)] <- output_df[order(as.vector(output_df[, i])), i:(i+1)]
      }
      colnames(output_df) <- headers
      if(!(is.null(output_path))){
        write.csv(output_df, paste0(output_path, '\\', output_file, '.csv'), na = "")
      }else{
        return(output_df)
      }
    }else if(unique(file_data$file_type) == 'cross_section'){
      output_data <- data.frame(df_series %>%
                                  filter(id_Ticker %in% !!file_data$ticker_id &
                                           id_Campo %in% !!file_data$field_id) %>%
                                  mutate(aux = concat(id_Campo, '|', id_Ticker)) %>%
                                  filter(aux %in% !!file_data$aux & !(is.na(vchValor))))
      if(!(is.null(output_path))){
        write.csv(output_data, paste0(output_path, '\\', output_file, '.csv'), na = "")
      }else{
        return(output_data)
      }
    }
  }
}
