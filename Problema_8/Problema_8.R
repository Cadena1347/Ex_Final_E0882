library(tidyverse)
library(rio)

transacciones <- import("base_transacciones.csv")

str(transacciones)

transacciones_limpio <- transacciones %>%
  mutate(
    id_transaccion = as.integer(abs(id_transaccion)),
    id_cliente = as.integer(abs(id_cliente)),
    monto = as.numeric(ifelse(is.na(monto), -1, monto)),
    tipo_transaccion = as.character(ifelse(is.na(tipo_transaccion), "no identificado", tipo_transaccion)),
    estado_transaccion = as.character(ifelse(is.na(estado_transaccion), "no identificado", estado_transaccion))
  ) %>%
  mutate(
    fecha_transaccion = case_when(
      str_detect(fecha_transaccion, "\\d{4}-\\d{2}-\\d{2}") ~ as.Date(fecha_transaccion, format = "%Y-%m-%d"),
      str_detect(fecha_transaccion, "\\d{2}-\\d{2}-\\d{4}") ~ as.Date(fecha_transaccion, format = "%d-%m-%Y"),
      str_detect(fecha_transaccion, "\\d{4}/\\d{2}/\\d{2}") ~ as.Date(fecha_transaccion, format = "%Y/%m/%d"),
      str_detect(fecha_transaccion, "\\d{2}/\\d{2}/\\d{4}") ~ as.Date(fecha_transaccion, format = "%m/%d/%Y"),
      TRUE ~ as.Date(fecha_transaccion, format = "%Y-%m-%d")
    )
  ) %>%
  mutate(
    fecha_transaccion = format(fecha_transaccion, "%Y-%m-%d")
  )



transacciones_completadas <- transacciones_limpio %>%
  filter(estado_transaccion == "Completada")




mediana <- median(transacciones_limpio$monto, na.rm = TRUE)
Q1 <- quantile(transacciones_limpio$monto, 0.25, na.rm = TRUE)
Q3 <- quantile(transacciones_limpio$monto, 0.75, na.rm = TRUE)


rango_intercuartil <- Q3 - Q1


limite_superior <- mediana + rango_intercuartil


transacciones_montos_anormales <- transacciones_limpio %>%
  filter(monto > limite_superior)
