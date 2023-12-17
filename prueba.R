#Cargar librerías necesarias
library(ggplot2)
library(plotly)
library(corrplot)
library(dplyr)
library(reshape2)
library(tidyverse)
library(cluster)
library(gt)
library(gtExtras)
library(gridExtra)
library(fmsb)
library(radarchart)
library(gganimate)
library(ggwordcloud)
library(png)
library(ggrepel)


#Cargar los datos desde el archivo CSV
file_path <- "datos/spotify-2023.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)



#Preparar los datos: Limpieza y conversión de tipos de datos
df$streams <- as.numeric(gsub("[^0-9]", "", df$streams)) 
df$in_deezer_playlists <- as.numeric(gsub("[^0-9]", "", df$in_deezer_playlists))
df$in_shazam_charts <- as.numeric(gsub("[^0-9]", "", df$in_shazam_charts))
df$track_name_clean <- iconv(df$track_name, to = "ASCII", sub = "")
df$artist.s._name <- iconv(df$artist.s._name, to = "UTF-8")



#-------------------------------------------------------------------------

#Este histograma transformado logarítmicamente proporciona una manera clara de ver cómo se distribuyen los streams entre las canciones populares del dataframe, destacando las tendencias generales en la popularidad de las canciones.


ggplot(df, aes(x = streams)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black") +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_minimal() +
  labs(title = "Distribución de Streams de Canciones",
       x = "Streams (Escala Logarítmica)",
       y = "Frecuencia")

#-------------------------------------------------------------------------

#Características musicales por cuartiles de streams. Estos gráficos de cajas permiten ver qué valores de "danceability", "energy" y "valance" tienen las canciones más escuchas por cuartiles.

# Definir los cuartiles de streams
cuartile_breaks <- quantile(df$streams, probs = seq(0, 1, by = 0.25))

# Crear una nueva columna en el dataframe para indicar el grupo de cuartil
df$stream_cuartile_group <- cut(df$streams, 
                                breaks = cuartile_breaks, 
                                include.lowest = TRUE, 
                                labels = c("0-25%", "25-50%", "50-75%", "75-100%"))


#Transformar el dataframe para visualización
df_melted <- melt(df, id.vars = "stream_cuartile_group", measure.vars = c("danceability_.", "energy_.", "valence_."))

# Crear el gráfico de caja
ggplot(df_melted, aes(x = variable, y = value, fill = stream_cuartile_group)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Bailabilidad", "Energía", "Valentía")) +
  theme_minimal() +
  labs(title = "Características Musicales por Cuartiles de Streams",
       x = "Característica",
       fill = "Cuartiles",
       y = "Valor") +
  facet_wrap(~stream_cuartile_group, scales = "free")

#-------------------------------------------------------------------------

#Canciones populares por mes de lanzamiento
# Convertir 'released_month' a factor con etiquetas de mes
df$released_month <- factor(df$released_month, levels = 1:12, labels = month.abb)

# Crear el gráfico de barras
ggplot(df, aes(x = released_month)) +
  geom_bar(fill = "green") + 
  theme_minimal() +
  labs(title = "Canciones Populares por Mes de Lanzamiento",
       x = "Mes de Lanzamiento",
       y = "Número de Canciones")

#-------------------------------------------------------------------------

#Número de streams por cantidad de artistas participantes 

ggplot(df, aes(x = artist_count, y = as.numeric(streams))) +
  geom_point(aes(color = artist_count), alpha = 0.6) +
  scale_y_log10() +
  theme_minimal() +
  scale_color_continuous(name = "Número de Artistas") +
  labs(title = "Número de Artistas vs Streams",
       x = "Número de Artistas",
       y = "Streams")
  
  
#-------------------------------------------------------------------------
  
#Gráfico de canciones por clusters. RStudio clasifica las canciones por clusters de datos similares y agrupa las canciones más escuchadas en 4 grupos. De esta manera podemos saber qué canciones reúnen características similares dentro de las más escuchadas. 

# Análisis de clusters utilizando k-means
set.seed(123)  # Para reproducibilidad
kmeans_result <- kmeans(df[, c("danceability_.", "energy_.", "valence_.")], centers = 4)

# Añadir la asignación de clusters al dataframe
df$cluster <- as.factor(kmeans_result$cluster)

# Crear una nueva columna para nombres de cluster personalizados
df$cluster_name <- paste("Cluster", df$cluster)

plot_ly(df, x = ~danceability_., y = ~energy_., z = ~valence_., 
        color = ~cluster_name, text = ~track_name_clean, 
        colors = c('red', 'blue', 'green', 'yellow'),
        marker = list(size = 10), type = 'scatter3d', mode = 'markers') %>%
  layout(title = "Análisis de clusters de canciones",
         scene = list(xaxis = list(title = 'Bailabilidad'),
                      yaxis = list(title = 'Energía'),
                      zaxis = list(title = 'Positividad')))

#-------------------------------------------------------------------------


# Gráficas que muestran las diferencias de los valores de bailabilidad, energía y valentía con respecto a la aparición en listas de reproducción en cada plataforma.

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(patchwork)

# Función para normalizar los datos
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Normalizar las columnas de apariciones en listas de reproducción
df <- df %>%
  mutate(in_spotify_playlists_norm = normalize(in_spotify_playlists),
         in_apple_playlists_norm = normalize(in_apple_playlists),
         in_deezer_playlists_norm = normalize(in_deezer_playlists))

# Eliminar filas con valores NA en las columnas relevantes
df_clean <- df %>%
  filter(!is.na(danceability_.) & 
           !is.na(in_spotify_playlists_norm) & 
           !is.na(in_apple_playlists_norm) & 
           !is.na(in_deezer_playlists_norm))

# Bailabilidad
# Gráfico para Spotify
p1 <- ggplot(df_clean, aes(x = danceability_., y = in_spotify_playlists_norm)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Bailabilidad vs Apariciones Normalizadas en Spotify",
       x = "Bailabilidad (%)",
       y = "Apariciones Normalizadas en Spotify") +
  theme_minimal()

# Gráfico para Apple Music
p2 <- ggplot(df_clean, aes(x = danceability_., y = in_apple_playlists_norm)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Bailabilidad vs Apariciones Normalizadas en Apple Music",
       x = "Bailabilidad (%)",
       y = "Apariciones Normalizadas en Apple Music") +
  theme_minimal()

# Gráfico para Deezer
p3 <- ggplot(df_clean, aes(x = danceability_., y = in_deezer_playlists_norm)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "Bailabilidad vs Apariciones Normalizadas en Deezer",
       x = "Bailabilidad (%)",
       y = "Apariciones Normalizadas en Deezer") +
  theme_minimal()

# Combinar los gráficos
combined_plot <- p1 + p2 + p3 + plot_layout(guides = 'collect')

# Mostrar el gráfico combinado
combined_plot

# Energía
# Gráfico para Spotify
p1 <- ggplot(df_clean, aes(x = energy_., y = in_spotify_playlists_norm)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Energía vs Apariciones Normalizadas en Spotify",
       x = "Energía (%)",
       y = "Apariciones Normalizadas en Spotify") +
  theme_minimal()

# Gráfico para Apple Music
p2 <- ggplot(df_clean, aes(x = energy_., y = in_apple_playlists_norm)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Energía vs Apariciones Normalizadas en Apple Music",
       x = "Energía (%)",
       y = "Apariciones Normalizadas en Apple Music") +
  theme_minimal()

# Gráfico para Deezer
p3 <- ggplot(df_clean, aes(x = energy_., y = in_deezer_playlists_norm)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "Energía vs Apariciones Normalizadas en Deezer",
       x = "Energía (%)",
       y = "Apariciones Normalizadas en Deezer") +
  theme_minimal()

# Combinar los gráficos
combined_plot1 <- p1 + p2 + p3 + plot_layout(guides = 'collect')

# Mostrar el gráfico combinado
combined_plot1

# Valentía
# Gráfico para Spotify
p1 <- ggplot(df_clean, aes(x = valence_., y = in_spotify_playlists_norm)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Valentía vs Apariciones Normalizadas en Spotify",
       x = "Valentía (%)",
       y = "Apariciones Normalizadas en Spotify") +
  theme_minimal()

# Gráfico para Apple Music
p2 <- ggplot(df_clean, aes(x = valence_., y = in_apple_playlists_norm)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Valentía vs Apariciones Normalizadas en Apple Music",
       x = "Valentía (%)",
       y = "Apariciones Normalizadas en Apple Music") +
  theme_minimal()

# Gráfico para Deezer
p3 <- ggplot(df_clean, aes(x = valence_., y = in_deezer_playlists_norm)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "Valentía vs Apariciones Normalizadas en Deezer",
       x = "Valentía (%)",
       y = "Apariciones Normalizadas en Deezer") +
  theme_minimal()

# Combinar los gráficos
combined_plot2 <- p1 + p2 + p3 + plot_layout(guides = 'collect')

# Mostrar el gráfico combinado
combined_plot2



#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#A partir de ahora nos centramos en las canciones de Feid.

#Filtramos el df para Feid.
df_feid <- df %>%
  filter(grepl("Feid", artist.s._name, ignore.case = TRUE))

#-------------------------------------------------------------------------
#Mostramos en un grafico circular cuantas canciones exitosas tiene Feid en comparación al resto.
#Primero comparamos todos los cantantes

# Contamos la cantidad de canciones por artista
count_df_artist <- table(df$artist.s._name)

# Filtramos los artistas con más de cinco canciones
filtered_count_df_artist <- count_df_artist[count_df_artist > 5]

# Creamos un dataframe para los gráficos
df_circ_artist <- data.frame(artist = names(filtered_count_df_artist), count = as.numeric(filtered_count_df_artist))

# Crear el gráfico circular para artistas
c1 <- ggplot(df_circ_artist, aes(x = "", y = count, fill = artist)) +
  geom_bar(stat = "identity", width = 1, color = ifelse(df_circ_artist$artist == "Feid", "black", NA)) +
  coord_polar("y", start = 0) +
  labs(title = "Proporción de Canciones por Artista", 
       fill = "Artista",
       x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "bottom")

# Creamoa el gráfico circular para la comparación de Feid vs Otros sin leyenda
c2 <- ggplot(df, aes(x = "", fill = ifelse(grepl("Feid", artist.s._name, ignore.case = TRUE), "Feid", "Otros"))) +
  geom_bar(stat = "count", width = 1) + 
  coord_polar("y", start = 0) +
  labs(title = "Proporción de Canciones de Feid vs Otros",
       fill = "Artista",
       x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "bottom") +  # Eliminar la leyenda
  scale_fill_manual(values = c("Feid" = "#1DB954", "Otros" = "pink"))  # Resaltar la sección de Feid en verde

# Mostramos ambos gráficos:
grid.arrange(c1, c2, ncol = 2)

#-------------------------------------------------------------------------
# Vamos a sacar un top 10 de sus canciones más escuchadas:

# Filtramos el df para Feid y ordenamos según la variable streams
df_feid <- df %>%
  filter(grepl("Feid", artist.s._name, ignore.case = TRUE)) %>%
  arrange(desc(streams))

# Sacamos las 10 canciones más escuchadas
top_10_feid <- df_feid[1:10, ]

# Renombramos las columnas
top_10_feid <- top_10_feid %>%
  rename(
    "Cancion" = track_name,
    "Artista" = artist.s._name,
    "Reproducciones" = streams,
    "Bailabilidad" = danceability_.,
    "Valentia" = valence_.,
    "Energia" = energy_.,
    "Acustica" = acousticness_.,
    "Viveza" = liveness_.
  )

#Cambiamos el nombre de una canción:

top_10_feid <- top_10_feid %>%
  mutate(Cancion = ifelse(Cancion == "Feliz Cumplea��os Fe", "Feliz Cumpleaños Ferxxo", Cancion))

# Seleccionamos las variables de interés
tabla_top_10_feid <- top_10_feid %>%
  select(Cancion, Artista, Reproducciones, Bailabilidad, Valentia, Energia, Acustica, Viveza) %>%
  gt(rowname_col = "Cancion") %>%
  tab_header(
    title = md("**TOP 10 FEID**"),
    subtitle = md("*Canciones más escuchadas*")
  ) %>%
  cols_align(align = "center") %>%
  cols_align(align = "right", columns = c("Cancion", "Artista")) %>%
  cols_merge(columns = c("Cancion", "Artista"), pattern = "{1} - {2}") %>%
  cols_label(
    Cancion = "Canción",
    Artista = "Artista",
    Reproducciones = paste0("🔄", " Reproducciones"),
    Bailabilidad = paste0("💃", " Bailabilidad"),
    Valentia = paste0("🦸️", " Valentia"),
    Energia = paste0("⚡️", " Energia"),
    Acustica = paste0("🎸", " Acustica"),
    Viveza = paste0("🔊", " Viveza"),
  ) %>%
  opt_row_striping() %>%
  opt_table_font(font = google_font("Amatic SC")) %>%
  tab_options(
    column_labels.background.color = "#39423c",
    footnotes.background.color = "#39423c",
    source_notes.background.color = "#39423c",
    heading.background.color = "#39423c",
    heading.align = "left"
  ) %>%
  tab_style(style = cell_text(color = "#1DB954", size = px(32)), locations = cells_title("title"))


tabla_top_10_feid

#-------------------------------------------------------------------------

#Vamos a crear un gráfico radar asignando a cada canción un color para ver sus características

# Seleccionar variables de interés
top_10_radar <- top_10_feid %>%
  select(Cancion, Bailabilidad, Valentia, Energia, Acustica, Viveza)

# Crear datos para el gráfico de radar principal
set.seed(99)
data_for_radar <- as.data.frame(matrix(sample(0:100, 50, replace = TRUE), ncol = 5))
colnames(data_for_radar) <- c("Bailabilidad", "Valentia", "Energia", "Acustica", "Viveza")

# Convertir los datos a formato largo
data_for_radar_long <- pivot_longer(data_for_radar, cols = starts_with("Bailabilidad"), names_to = "Variable", values_to = "Value")

# Convertir la Variable a factor con los niveles deseados
data_for_radar_long$Variable <- factor(data_for_radar_long$Variable, levels = c("Bailabilidad", "Valentia", "Energia", "Acustica", "Viveza"))

# Crear un gráfico de radar principal
radarchart(data_for_radar_long, title = "Gráfico de Radar - Top 10 Feid",
           pfcol = rainbow(nrow(top_10_radar)),
           plwd = 1, plinetype = 1, # Usar línea continua
           axistype = 1, # Para que use nombres de ejes
           axislabcol = "grey", # Color de las etiquetas de los ejes
           pcol = rgb(0.2, 0.5, 0.2, alpha = 0.2), # Color de fondo del área bajo la curva
           vlcex = 0, # Ajustar el tamaño de las etiquetas del eje radial
           cglcol = "grey", # Color de las líneas del eje radial
           cglty = 1, # Tipo de línea del eje radial
           cglwd = 0.5, # Ancho de las líneas del eje radial
           plpos = c(0, 0) # Posición de la leyenda (x, y)
)

# Crear datos para el gráfico de radar adicional
set.seed(99)
data_additional <- as.data.frame(matrix(sample(0:100, 50, replace = TRUE), ncol = 5))
colnames(data_additional) <- c("Bailabilidad", "Valentia", "Energia", "Acustica", "Viveza")
rownames(data_additional) <- paste("Cancion", 1:10)

data_additional <- rbind(rep(100, 5), rep(0, 5), data_additional)

# Convertir los datos adicionales a formato largo
data_additional_long <- pivot_longer(data_additional, cols = starts_with("Bailabilidad"), names_to = "Cancion", values_to = "Value")

# Convertir la Variable a factor con los niveles deseados
data_additional_long$Cancion <- factor(data_additional_long$Cancion, levels = c("Bailabilidad", "Valentia", "Energia", "Acustica", "Viveza"))

# Crear un vector de colores numéricos para la leyenda
legend_colors <- as.numeric(factor(top_10_radar$Cancion))

# Crear un gráfico de radar adicional
radarchart(data_additional, axistype = 1, title = "Gráfico de Radar - Top 10 Feid", lty = 1, cglty = 1, cglcol = "gray", cglwd = 1, pcol = legend_colors, plwd = 2, plty = 1) 

# Obtener el número total de canciones
num_songs <- nrow(top_10_radar)

# Dividir las canciones en dos filas
songs_row1 <- top_10_radar$Cancion[1:5]
songs_row2 <- top_10_radar$Cancion[6:10]

legend("left",
       title = expression(bold("Canciones")),
       legend = c(songs_row1, songs_row2),
       bty = "n", pch = 20, col = legend_colors,
       text.col = "black", pt.cex = 1.5, ncol = 1, inset = c(-0.001, -0.04),
       cex = 0.7)  # Ajusta el tamaño general de la leyenda


#---------------------------------------------------------------------------------------------
#A partir del top_10_radar vamos a crear cinco graficos de dispersión, uno para cada caracteristica Bailabilidad, Viveza, Valentia, Energia, Acustica

# Creamos el gráfico de ggplot
combi <- ggplot(top_10_radar, aes(x = Cancion)) +
  geom_point(aes(y = Bailabilidad,color = "Bailabilidad", text = paste("Bailabilidad:", round(Bailabilidad, 1))), size = 3, alpha = 0.7) +
  geom_point(aes(y = Viveza, color = "Viveza", text = paste("Viveza:", round(Viveza, 2))), size = 3, alpha = 0.7) +
  geom_point(aes(y = Valentia, color = "Valentia", text = paste("Valentia:", round(Valentia, 2))), size = 3, alpha = 0.7) +
  geom_point(aes(y = Energia, color = "Energia", text = paste("Energia:", round(Energia, 2))), size = 3, alpha = 0.7) +
  geom_point(aes(y = Acustica, color = "Acustica", text = paste("Acustica:", round(Acustica, 2))), size = 3, alpha = 0.7) +
  geom_line(aes(y = Bailabilidad, group = 1), color = "purple", size = 1) +
  geom_line(aes(y = Viveza, group = 1), color = "blue", size = 1) +
  geom_line(aes(y = Valentia, group = 1), color = "pink", size = 1) +
  geom_line(aes(y = Energia, group = 1), color = "green", size = 1) +
  geom_line(aes(y = Acustica, group = 1), color = "aquamarine", size = 1) +
  geom_text(aes(y = Bailabilidad + 0.02, label = round(Bailabilidad, 2)), color = "purple", size = 3, hjust = 0, vjust = 0.5) +
  geom_text(aes(y = Viveza + 0.02, label = round(Viveza, 2)), color = "blue", size = 3, hjust = 0, vjust = 0.5) +
  geom_text(aes(y = Valentia + 0.02, label = round(Valentia, 2)), color = "pink", size = 3, hjust = 0, vjust = 0.5) +
  geom_text(aes(y = Energia + 0.02, label = round(Energia, 2)), color = "green", size = 3, hjust = 0, vjust = 0.5) +
  geom_text(aes(y = Acustica + 0.02, label = round(Acustica, 2)), color = "aquamarine", size = 3, hjust = 0, vjust = 0.5) +
  scale_color_manual(
    values = c(
      Bailabilidad = "purple",
      Viveza = "blue",
      Valentia = "pink",
      Energia = "green",
      Acustica = "aquamarine"
    ),
    name = "Característica"
  ) +
  labs(title = "Gráfico de Dispersión - Todas las Características",
       x = "Canción",
       y = "Valor de la Característica",
       color = "Característica") +
  theme_minimal()

# Convertimos el gráfico de ggplot a plotly
combi_plotly <- ggplotly(combi)

# Mostramos el gráfico interactivo
combi_plotly


#---------------------------------------------------------------------------------------------
#Comparamos la popularidad entre Spotify y Apple de cada canción

# Crear el gráfico ggplot con barras horizontales
newtop <- top_10_feid %>%
  select(Cancion, in_spotify_playlists, in_apple_playlists)

comp_playlist <- ggplot(newtop, aes(y = factor(Cancion))) +
  geom_bar(aes(x = in_spotify_playlists, fill = "Spotify"), stat = "identity", position = "dodge") +
  geom_bar(aes(x = in_apple_playlists, fill = "Apple Music"), stat = "identity", position = "dodge") +
  geom_point(aes(x = in_spotify_playlists, fill = "Spotify"),color = "#1DB954")+
  geom_point(aes(x = in_apple_playlists, fill = "Apple Music"),color = "#FF2F54")+
  labs(title = "Popularidad por Canción en Spotify y Apple Music",
       x = "Playlists",
       y = "Canción") +
  scale_fill_manual(values = c("Spotify" = "#1DB954", "Apple Music" = "#FF2F54")) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  

# Mostrar el gráfico

# Convertimos el gráfico de ggplot a plotly
comp_plotly <- ggplotly(comp_playlist)

# Mostramos el gráfico interactivo
comp_plotly

#---------------------------------------------------------------------------------------------
#Colaboradores en las Canciones más populares de Feid

# Creamos un nuevo df de artistas colaboradores y eliminamos a Feid de estos:

df_colab <- df_feid %>%
  select(artist.s._name) %>%
  mutate(artist.s._name = gsub("Feid", "", artist.s._name)) %>%
  filter(!is.na(artist.s._name)) %>%
  separate_rows(artist.s._name, sep = ",")%>%
  filter(artist.s._name != "")%>%
  distinct() %>%
  filter(artist.s._name != "")%>%
  mutate(artist.s._name = str_trim(artist.s._name)) %>%
  filter(artist.s._name != "")%>%
  rename(
    "Colaboradores" = artist.s._name)

set.seed(42)

# Creamos el word cloud de los colaboradores de Feid:
ggplot(df_colab, aes(label = Colaboradores, color = Colaboradores, size = runif(nrow(df_colab)))) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 30) + 
  theme_minimal()

