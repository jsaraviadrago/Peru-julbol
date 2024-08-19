
library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)

url_data_rankings <- "https://raw.githubusercontent.com/jsaraviadrago/Misc-Portfolio/main/fifa_rankings.csv"
url_data_matches <- "https://raw.githubusercontent.com/jsaraviadrago/Misc-Portfolio/main/results.csv" 

urL_data_population <- "https://raw.githubusercontent.com/jsaraviadrago/Misc-Portfolio/main/0.%20Fun%203D%20plots%20and%20art/world_population.csv"


data_rankings <- read.csv(url_data_rankings)
data_matches <- read.csv(url_data_matches)  
data_population <- read.csv(urL_data_population)

data_population_summary <- data_population |> 
  select(Country.Territory, X2022.Population)


data_rankings <- data_rankings %>% 
  mutate(
    date = as.Date(rank_date),
    year = year(rank_date)) 

data_summary_rankings <- data_rankings |>
  dplyr::group_by(year, team) |> 
  dplyr::summarise(Points = max(total_points),
                   confederation = first(confederation),
                   country_abrv = first(country_abrv)) 

data_matches <- data_matches |> 
  mutate(
    date = as.Date(date),
    year = year(date)) 

data_matches <- data_matches |> 
  filter(year >= 1992)

data_matches_summary_local <- data_matches |> 
  group_by(year, home_team) |>
  summarise(Goles_local_favor = sum(home_score),
            Goles_local_contra = sum(away_score),
            Total_local_partidos = n())

data_matches_summary_local <- data_matches_summary_local |> 
  mutate(Dif_goles_local = Goles_local_favor - Goles_local_contra) |> 
  select(-Goles_local_favor, -Goles_local_contra)


data_matches_summary_visita <- data_matches |> 
  group_by(year, away_team) |>
  summarise(Goles_visita_favor = sum(away_score),
            Goles_visita_contra = sum(home_score),
            Total_visita_partidos = n())

data_matches_summary_visita <- data_matches_summary_visita |> 
  mutate(Dif_goles_visita = Goles_visita_favor - Goles_visita_contra) |> 
  select(-Goles_visita_favor, -Goles_visita_contra)



data_matches_summary <- full_join(data_matches_summary_local, 
                                   data_matches_summary_visita,
                                   by =c("year" = "year", "home_team" = "away_team"))

data_matches_summary <- data_matches_summary |> 
  mutate(Total_partidos = Total_local_partidos + Total_visita_partidos,
         Dif_goles = Dif_goles_local + Dif_goles_visita)


data_matches_summary <- data_matches_summary |> 
  mutate(Total_partidos_revisado = case_when(
    is.na(Total_partidos) & is.na(Total_visita_partidos) ~ paste0(Total_local_partidos),
    is.na(Total_partidos) & is.na(Total_local_partidos) ~ paste0(Total_visita_partidos),
    !is.na(Total_partidos) ~ paste0(Total_partidos)))

data_matches_summary <- data_matches_summary |> 
  mutate(Dif_goles_revisado = case_when(
    is.na(Dif_goles) & is.na(Dif_goles_visita) ~ paste0(Dif_goles_local),
    is.na(Dif_goles) & is.na(Dif_goles_local) ~ paste0(Dif_goles_visita),
    !is.na(Dif_goles) ~ paste0(Dif_goles)))


# Total partidos
data_matches_total <- data_matches_summary |> 
  group_by(home_team) |> 
  summarise(Total_matches = sum(as.numeric(Total_partidos_revisado)))

data_matches_summary <- left_join(data_matches_summary, data_matches_total,
                                  by = "home_team")


data_matches_summary$Dif_goles_revisado <- as.numeric(data_matches_summary$Dif_goles_revisado)

data_matches_summary <- data_matches_summary |> 
  select(year, home_team, Dif_goles_revisado, Total_matches)

data_complete <- dplyr::left_join(data_summary_rankings, data_matches_summary,
                                  by = c("team"="home_team", "year" = "year"))

data_complete <- left_join(data_complete, data_population_summary,
                           by = c("team" = "Country.Territory"))


world_champions <- c("Ecuador", "Argentina", "Peru", "Uruguay", "Brazil", "Venezuela", "Colombia",
                     "Paraguay", "Bolivia", "Chile")

data_complete_filtered <- data_complete |> 
  filter(team %in% world_champions)
  


#adding extra customization (labels, title) and changing size of bubbles
gap_plot <- ggplot(data_complete_filtered, aes(x = Points,
                                            y = Dif_goles_revisado, 
                                      color = team, size = X2022.Population, label = team)) +
  geom_point(alpha=0.8)+
  theme(panel.background = element_blank(), 
        legend.position = "none")+
  geom_text(hjust=0, vjust=0)+
  labs(x = 'Puntos Ranking FIFA', y = 'Diferencia de goles',
                title = "Ranking FIFA segun resultado") +
# gganimate code
ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

# animate
animate(gap_plot, width = 900, height = 450, fps = 10, duration = 20)
# save as a GIF
anim_save("C:\\Users\\JuanCarlosSaraviaDra\\Downloads\\output.gif")


