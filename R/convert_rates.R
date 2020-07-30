

conv_tasas <- function(tasa_inicial, t, tipo_inicial, tipo_final) {
  stopifnot(tipo_inicial != tipo_final) # Detiene la función si tipo_inicial == tipo_final
  if (tipo_inicial == 'Efectiva') {
    if (tipo_final == 'Continua') {
      return(exp(tasa_inicial) - 1)
    } else if (tipo_final == 'Nominal') {
      return (t * ((tasa_inicial + 1) ^ (1 / t) - 1))
    }
  } else if (tipo_inicial == 'Nominal') {
    if (tipo_final == 'Efectiva') {
      return((1 + tasa_inicial / t) ^ t - 1)
    } else if (tipo_final == 'Continua') {
      return(exp((1 + tasa_inicial / t) ^ t - 1) - 1)
    } 
  } else if (tipo_inicial == 'Continua') {
    if (tipo_final == 'Efectiva') {
      return(log(tasa_inicial + 1))
    } else if (tipo_final == 'Nominal') {
      return(log((t * ((tasa_inicial + 1) ^ (1 / t) - 1)) + 1))
    }
  } else {
    print("No ha ingresado correctamente los parámetros, los tipos de tasa deben ser: Nominal, Efectiva o Continua")
  }
}