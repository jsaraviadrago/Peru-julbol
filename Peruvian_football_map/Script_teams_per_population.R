library(mapsPERU)
library(dplyr)
library(ggplot2)
library(sf)

df <- map_DEP


# Cargar dataframe con equipos
url <- "https://raw.githubusercontent.com/jsaraviadrago/Peru-julbol/main/Peruvian_football_map/data_equipos_vf1.csv"

data_equipos <- read.csv(url)

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

data_equipos_departamento <- data_equipos_departamento |> 
  group_by(region) |> 
  summarise(Cantidad_equipos = sum(Cantidad_equipos))



fperu_dpto <- left_join(df, data_equipos_departamento,
                        by = c("DEPARTAMENTO" = "region"))



# https://www.datosabiertos.gob.pe/dataset/poblaci%C3%B3n-peru

url2 <- "https://raw.githubusercontent.com/jsaraviadrago/Peru-julbol/main/TB_POBLACION_INEI.csv"

data_poblacion <- read.csv(url2, sep = ";")



data_poblacion <- data_poblacion |> 
  group_by(Departamento) |> 
  summarise(Poblacion = sum(Cantidad))


fperu_dpto <- data.frame(fperu_dpto, data_poblacion)

fperu_dpto <- fperu_dpto |> 
  select(-Departamento)

fperu_dpto$mil_equipo <- (fperu_dpto$Cantidad_equipos/fperu_dpto$Poblacion)*10000

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






