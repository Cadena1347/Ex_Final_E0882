library(tidyverse)
library(rio)

polizas <- import("base_polizas.csv", sheet = "base_polizas")

# Tabla con todas las transformaciones
polizas_corregido <- polizas %>%
  mutate(id_poliza = ifelse(id_poliza < 0, id_poliza * (-1), id_poliza)) %>%
  mutate(nombre_cliente = ifelse(is.na(nombre_cliente), "No registrado", nombre_cliente)) %>%
  mutate(fecha_inicio = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_inicio),
                               as.character(as.Date(fecha_inicio, format = "%d-%m-%Y")),
                               fecha_inicio)) %>%
  mutate(fecha_inicio = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_inicio),
                               as.character(as.Date(fecha_inicio, format = "%Y-%m-%d")),
                               fecha_inicio)) %>%
  mutate(fecha_fin = case_when(
    estado == "Activa" ~ NA_character_,
    estado == "Desconocido" ~ "Desconocido",
    TRUE ~ as.character(as.Date(fecha_fin, format = "%Y-%m-%d"))
  )) %>%
  mutate(monto_cobertura = ifelse(is.na(monto_cobertura), -1, monto_cobertura)) %>%
  mutate(estado = ifelse(is.na(estado), "Desconocido", estado)) %>%
  mutate(temp_fecha_inicio = ifelse(fecha_inicio > fecha_fin & !is.na(fecha_fin), fecha_fin, fecha_inicio),
         temp_fecha_fin = ifelse(fecha_inicio > fecha_fin & !is.na(fecha_fin), fecha_inicio, fecha_fin)) %>%
  select(-fecha_inicio, -fecha_fin) %>%
  rename(fecha_inicio = temp_fecha_inicio, fecha_fin = temp_fecha_fin) %>%
  mutate(fecha_inicio = as.Date(fecha_inicio),
         antiguedad_anios = round(as.numeric(difftime(Sys.Date(), fecha_inicio, units = "days")) / 365.25, 1))

# Tabla filtrada por pólizas activas
polizas_activas <- polizas_corregido %>%
  filter(estado == "Activa") %>%
  select(id_poliza, nombre_cliente, fecha_inicio, fecha_fin, monto_cobertura, estado, antiguedad_anios)