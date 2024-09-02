library(mapsPERU)
library(dplyr)
library(ggplot2)
library(sf)

# Data frame del paquete mapsPeru
df <- map_DEP


# Cargar dataframe con equipos
url <- "https://raw.githubusercontent.com/jsaraviadrago/Peru-julbol/main/Peruvian_football_map/data_equipos_vf1.csv"

# leer el data frame de equipos
data_equipos <- read.csv(url)

# agrupar por región para obtener la cantidad de equipos
data_equipos_departamento <- data_equipos |> 
  group_by(region) |> 
  summarise(Cantidad_equipos = n())

# Limpiar nombres para que estén igual 
data_equipos_departamento <- data_equipos_departamento |> 
  mutate(region = case_when(
    region == "Áncash" ~ "Áncash",
    region == "Lima Región" ~ "Lima",
    region == "Lima Metropolitana" ~ "Lima",
    TRUE ~ region))

# Dado que Lima se repite entonces nuevamente se agrupa
data_equipos_departamento <- data_equipos_departamento |> 
  group_by(region) |> 
  summarise(Cantidad_equipos = sum(Cantidad_equipos))


# Unir la información del mapa de mapsPeru y el data frame de equipos
fperu_dpto <- left_join(df, data_equipos_departamento,
                        by = c("DEPARTAMENTO" = "region"))



# Cargar información de la población
# https://www.datosabiertos.gob.pe/dataset/poblaci%C3%B3n-peru

url2 <- "https://raw.githubusercontent.com/jsaraviadrago/Peru-julbol/main/TB_POBLACION_INEI.csv"

data_poblacion <- read.csv(url2, sep = ";")  #Leer el data frame


# Agrupar para tener la población por región
data_poblacion <- data_poblacion |> 
  group_by(Departamento) |> 
  summarise(Poblacion = sum(Cantidad))

# Unir lo daata frames
fperu_dpto <- data.frame(fperu_dpto, data_poblacion)

# Borrar la columna rpetida
fperu_dpto <- fperu_dpto |> 
  select(-Departamento)

# Calcular la tasa de equipos por cada 10000 habitantes
fperu_dpto$mil_equipo <- (fperu_dpto$Cantidad_equipos/fperu_dpto$Poblacion)*10000

# Graficar el mapa por cantidad de equipos
mapa_equipos <- ggplot(fperu_dpto, aes(geometry=geometry)) +
  geom_sf(aes(fill = Cantidad_equipos)) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())+
  ggtitle("Cantidad de equipos por región")

# Graficar el mapa de la tasa de equipos por cada 10 mil personas. 
mapa_equipos_poblacion <- ggplot(fperu_dpto, aes(geometry=geometry)) +
  geom_sf(aes(fill = mil_equipo)) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())+
  ggtitle("Tasa de equipos por diez mil habitantes")






