library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

# Visualización
library(ggplot2)
library(visdat)
library(treemapify)
library(gganimate)
library(gifski)

df_clean <- read_csv("data/df_clean.csv")
# # ------------- GRÁFICA 1 --------------------
animacion_actividad <- df_clean %>%
  ggplot(aes(x = Edad, y = Tasa_actividad, color = Sexo, group = Edad)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(aes(group = Edad), alpha = 0.5) +
  labs(
    title = "Tasa de Actividad por Edad y Sexo - Año: {as.integer(frame_time)}",
    subtitle = "La brecha de género en la participación laboral.",
    y = "Tasa de Actividad (%)",
    x= "Edad (años)"
  ) +
  theme_minimal() +
  transition_time(Periodo) +
  ease_aes('linear')
anim_save(
  filename = "gifs/evolucion_actividad.gif",
  animation = animacion_actividad,
  renderer = gifski_renderer(loop = TRUE)
)

# #------------------------------- GRÁFICA 2 ------------------------------------
brecha_edades <- df_clean %>%
  select(Sexo, Edad, Periodo,Tasa_paro) %>%
  pivot_wider(names_from = Sexo, values_from = Tasa_paro) %>%
  mutate(brecha_paro=Mujeres-Hombres)


medias_anio <- brecha_edades %>%
  group_by(Periodo) %>%
  summarise(media = mean(brecha_paro, na.rm = TRUE))

# Gráfico
grafico_animado <- ggplot(brecha_edades,
                          aes(x = factor(Edad, levels = unique(Edad)),
                              y = brecha_paro)) +
  geom_col(aes(fill = "Brecha de paro")) +
  geom_hline(data = medias_anio,
             aes(yintercept = media, color = "Media anual"),
             linetype = "dashed", linewidth = 1) +
  scale_fill_manual(name="Relleno", values = c("Brecha de paro" = "steelblue")) +
  scale_color_manual( name="Linea",values = c("Media anual" = "black")) +
  labs(
    title = "Brecha de paro por grupo de edad — Año: {closest_state}",
    x = "Grupo de edad",
    y = "Brecha"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        legend.position = "top") +
  transition_states(Periodo, transition_length = 2, state_length = 1) +
  ease_aes("linear")

# Animar
animacion <- animate(
  grafico_animado,
  nframes = length(unique(brecha_edades$Periodo)) * 10,
  fps = 10,
  renderer = gifski_renderer(),
  width = 800,
  height = 600
)

anim_save("gifs/brecha_paro.gif", animation = animacion)


# ------------------ GRÁFICA 3 --------------------------------
datos_composicion<- df_clean  %>%
  select(Periodo,Edad,Sexo,Activos,Ocupados,Parados,Inactivos) %>%
  pivot_longer(cols = c(Inactivos,Ocupados,Parados),names_to = "Estado",values_to = "N_Personas")
graph1 <- ggplot(datos_composicion,
                 aes(x = factor(Edad, levels = unique(Edad)),
                     y = N_Personas, fill = Estado)) +
  geom_col(position = "fill") +
  facet_wrap(~Sexo) +
  labs(
    title = "Composición del mercado laboral por grupo de edad - Año: {as.integer(frame_time)}",
    x = "Grupo de edad",
    y = "Proporción",
    fill = "Estado"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_time(Periodo) +
  ease_aes('linear')
animacion <- animate(graph1, nframes = 100, fps = 10, renderer = gifski_renderer())
anim_save("gifs/composicion_mercado.gif", animation = animacion)

# ------------------------ GRÁFICA 4 ------------------

datos_anim <- df_clean %>%
  group_by(Sexo, Periodo, Edad) %>%
  summarise(Parados = sum(Parados, na.rm = TRUE)) %>%
  group_by(Sexo, Periodo) %>%
  mutate(Proporcion_parados_en_sexo = Parados / sum(Parados)) %>%
  ungroup()

edades_orden <- datos_anim %>%
  distinct(Edad) %>%
  mutate(inicio = as.integer(str_extract(Edad, "\\d+"))) %>%
  arrange(inicio) %>%
  pull(Edad)

datos_anim <- datos_anim %>%
  mutate(Edad = factor(Edad, levels = edades_orden))

grafico_animado <- ggplot(datos_anim,
                          aes(x = Edad,
                              y = Proporcion_parados_en_sexo,
                              fill = Sexo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribución del paro dentro de cada sexo - Año: {as.integer(frame_time)}",
    x = "Grupo de edad",
    y = "Proporción del total de parados del mismo sexo",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_time(Periodo) +
  ease_aes("linear")

animacion <- animate(
  grafico_animado,
  nframes = length(unique(datos_anim$Periodo)) * 10,
  fps = 10,
  width = 800,
  height = 600,
  renderer = gifski_renderer()
)
anim_save("gifs/distribucion_paro_sexo.gif", animation = animacion)

# ------------------- GRAFICA 5 --------------------------------
df_ocup_inact <- df_clean %>%
  group_by(Periodo, Sexo) %>%
  summarise(
    Ocupados = sum(Ocupados, na.rm = TRUE),
    Inactivos = sum(Inactivos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Ocupados, Inactivos), names_to = "Situacion", values_to = "Personas")

# gráfico animado
graph1 <- ggplot(df_ocup_inact, aes(x = Sexo, y = Personas, fill = Situacion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribución de ocupados e inactivos por sexo – Año: {as.integer(frame_time)}",
    x = "Sexo",
    y = "Personas",
    fill = "Situación"
  ) +
  theme_minimal() +
  transition_time(Periodo) +
  ease_aes('linear')

#  GIF
anim_save("gifs/ocupados_inactivos_sexo.gif", animation = graph1, renderer = gifski_renderer())


# ----------------------------- GRÁFICA 6 ------------------------------
#  Preparar los datos por año
df_act_inact <- df_clean %>%
  group_by(Periodo, Sexo) %>%
  summarise(
    Activos = sum(Activos, na.rm = TRUE),
    Inactivos = sum(Inactivos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Activos, Inactivos), names_to = "Situacion", values_to = "Personas")

#  Crear el gráfico animado
graph1 <- ggplot(df_act_inact, 
                 aes(x = Sexo, y = Personas, fill = Situacion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribución de activos e inactivos por sexo – Año: {as.integer(frame_time)}",
    x = "Sexo",
    y = "Personas",
    fill = "Situación"
  ) +
  theme_minimal() +
  transition_time(Periodo) +
  ease_aes('linear')

# 3 Crear la animación desde el gráfico
animacion <- animate(graph1, nframes = 100, fps = 10, renderer = gifski_renderer())

#  Guardar el GIF
anim_save("gifs/activos_inactivos_sexo.gif", animation = animacion, renderer = gifski_renderer())