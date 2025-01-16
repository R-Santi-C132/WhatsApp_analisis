
# --------------------------- WHATSAPP DATA ANALYSIS ---------------------------
# ------------------------------------------------------------------------------

# ---------------  Análisis de texto y visualización de datos ------------------

# -------------------------- SETTING WORKING DIRECTORY -----------------------------
getwd()
setwd("C:/Users/biobl/Desktop/RStudio/DATAVIZ/CHARTS/WA")


# ---------------------------   PACKAGE LOADING    -----------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("rwhatsapp", "lubridate", "tidyverse", "tidytext", "kableExtra", 
              "RColorBrewer", "knitr", "plotly", "scales", "ggtext", "showtext",
              "colorspace", "paletteer", "sysfonts", "glue", "dplyr", "stopwords")

ipak(packages)

# rwhatsapp   : Carga el paquete `rwhatsapp` para analizar chats de WhatsApp exportados.  
# lubridate   : Facilita el manejo de fechas y horas.  
# tidyverse   : Manipulación, análisis y visualización de datos.  
# tidytext    : Herramientas para análisis de texto de forma ordenada.  
# kableExtra  : Mejora la presentación de tablas creadas con `knitr::kable()`.  
# RColorBrewer: Proporciona paletas de colores personalizables.  
# knitr       : Genera informes dinámicos y reproducibles en R.  
# scales      : Herramientas para formatear ejes y escalas en gráficos.  
# ggtext      : Permite personalizar texto avanzado en gráficos de `ggplot2`.  
# showtext    : Permite usar fuentes externas en gráficos.  
# colorspace  : Herramientas para trabajar con colores de forma flexible.  
# paletteer   : Accede a diversas paletas de colores de diferentes paquetes.  
# sysfonts    : Administra y utiliza fuentes personalizadas en gráficos.  
# glue        : Facilita la creación de cadenas de texto dinámicas con interpolación.  
# dplyr       : Paquete de manipulación de datos dentro del `tidyverse`.


# ---------------------------- DATA LOADING ------------------------------------

# LEEMOS EL CHAT A TRAVÉS DEL TXT EXPORTADO DESDE LA APP
miChat <- rwa_read("WA_M.txt")
#rwa_read: Lee el archivo de texto exportado desde WhatsApp y lo convierte en un 
#data frame organizado.

head(miChat)

# ---------------------------- CLEANING AND WRANGLING --------------------------

# Extraer año, mes, día y hora en formato 24h
miChat <- miChat |> 
  mutate(
    AÑO = year(time),   # Extraer el año
    MES = month(time),  # Extraer el mes
    DÍA = day(time),    # Extraer el día
    HORA = hour(time)   # Extraer la hora en formato 24h
  )

#Eliminar mensajes sin autor:
miChat <- miChat |> 
  filter(!is.na(author))


# ---------------------------   MAKING THE CHARTS   ----------------------------

#                  Visualization parameters for all charts

sysfonts::font_add(
  family = "Font Awesome Brands",#descargar manualmente
  regular = here::here("docs", "fa-brands-400.ttf")
)

# Cargar las fuentes para el diseño del gráfico
font_add_google("Oswald", family = "title_font")
font_add_google("Barlow Condensed", family = "caption_font")
font_add_google("Kanit", family = "body_font")
showtext_auto()

# Colores y estilos
bg_col <- "white"
text_col <- "grey15" #color para el texto en gral
text_hil <- "grey15" #color para el texto resaltado


# Añadir íconos y usuarios
github <- "&#xf09b"  # Código del ícono de GitHub
github_username <- "R-Santi-C132"
xtwitter <- "&#xe61b"  # Código del ícono de Twitter (X)
xtwitter_username <- "@xxxxx_xxxxx"

#Glue:Ensambla cadenas de texto combinando texto y variables.
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome Brands\";'>{github};</span> <span style='color: {text_hil}'>{github_username}  </span>")

social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome Brands\";'>{xtwitter};</span> <span style='color: {text_hil}'>{xtwitter_username}</span>")
#<span style>: Usa HTML para formatear el texto. Aquí se especifica la fuente y el color.
# El texto final incluye el ícono de la red social y el nombre de usuario resaltado.

plot_caption <- paste0(#paste0: Une varias cadenas de texto en una sola.
  "**Data:** WA", 
  " |  **Code:** ", 
  social_caption_1, 
  " |  **Gráfico:** ", 
  social_caption_2
)

#Elimina variables temporales para liberar memoria y evitar conflictos posteriores.
rm(github, github_username, xtwitter, 
   xtwitter_username, social_caption_1, 
   social_caption_2) 

#_______________________________________________________________________________

# 1.- ¿Cuántos mensajes se enviaron en el periodo de tiempo (sept 23 - ene 2025? 

#________________________________   Heatmap  ___________________________________

#                              a) Data cleaning 

miChat_summary <- miChat |> 
  group_by(AÑO, MES) |> 
  summarise(num_textos = n(), .groups = "drop") |> 
  mutate(MES = factor(MES, levels = 1:12, labels = month.abb)) # Convertir MES a nombres abreviados


#                             b) Making the heatmap

heatmap_plot <- miChat_summary |> 
  ggplot(aes(x = MES, y = factor(AÑO), fill = num_textos)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_paletteer_c("grDevices::Temps", 
    direction = 1, name = "Número de mensajes",
    guide= guide_colorbar(
      barwidth=20, barheight=1.5, title.position="top",title.hjust=0.5
    )) +
  labs(
    title = "Heatmap: ¿Cuántos mensajes nos hemos enviado? \nSept 2023 - Ene 2025",
    subtitle = "Visualización de número de mensajes ",
    x = "Mes",
    y = "Año",
    caption = plot_caption
  ) +
  theme_minimal(base_family = "title_font") +
  theme(
    text = element_text(color = text_col),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 45, hjust = 0.5),
    legend.text = element_text(size = 45),
    axis.text.x = element_text(hjust = 1, size = 45),
    axis.text.y = element_text(size = 40),
    axis.title = element_text(size = 55),
    plot.title = element_text(
      family = "title_font", size = 65, face = "bold", 
      hjust = 0.5, vjust = -2, lineheight = 0.35),#lineheight controla el espaciado entrelinea
    plot.subtitle = element_text(size = 50, hjust = 0.5, vjust = -2.5),
    plot.caption = element_textbox(
      family = "caption_font", size = 45, hjust = 0.5, #color = text_col, 
      halign = 0.5)
  )

# Mostrar el gráfico
heatmap_plot

#                                c) Saving 
ggsave(
  filename ="heatmap_nmessage.png",
  plot = heatmap_plot,
  width = 210,    # Best Twitter Aspect Ratio = 5:4
  height = 297,   
  units = "mm",
  dpi = 300,
  bg = "white"
)

#_______________________________________________________________________________

#                   2.- ¿Quién ha enviado más mensajes? 

#________________________________ stacked_bar __________________________________


#                               a)Data cleaning  

#Procesar los datos por autor y fecha
miChat_summary2 <- miChat |> 
  mutate(
    AÑO = lubridate::year(time),                       # Extraer el año de la columna `time`.
    MES = lubridate::month(time, label = TRUE, abbr = TRUE)  # Extraer el mes con nombres abreviados.
  ) |> 
  group_by(AÑO, MES, author) |>                        # Agrupar por año, mes y autor.
  summarise(num_textos = n(), .groups = "drop")        # Contar los mensajes enviados por autor.

# Cambiar los nombres de los autores
miChat_summary2 <- miChat_summary2 |> 
  mutate(author = case_when(
    author == "Im a cyborg but that's ok" ~ "Yo",        
    author == "Mi mini Bióloga ᵐᵃᵍʸ ´･ᴗ･`" ~"Ella"  
  ))

#                                 b) Graph 

# Crear el gráfico de barras apiladas
stacked_bar_plot <- miChat_summary2 |> 
  ggplot(aes(x = MES, y = num_textos, fill = author)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.1) +
  #stat = "identity": Usa los datos como están (sin contar).
  #position = "stack": Apila las barras de los autores en cada mes.
  #color = "white": Bordes blancos para separar las barras.
  scale_fill_manual(
    values = c("Yo" = "#001a6e",   # Azul para el autor S
               "Ella" = "#8b80fd"),  # Naranja para el autor M
    name = "Emisor"              # Título de la leyenda
  )+
    labs(
    title = "¿Quién envía más mensajes? \nSept 2023 - Ene 2025",
    subtitle = "Comparación mensual y anual",
    x = "Mes",
    y = "Número de mensajes",
    caption = plot_caption
  ) +
  facet_wrap(~AÑO, ncol = 1) +#Divide el gráfico en facetas (una por año), 
  #con una columna por faceta.
  theme_minimal(base_family = "title_font") +
  theme(
    text = element_text(color = text_col),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 45, hjust = 0.5),
    legend.text = element_text(size = 45),
    axis.text.x = element_text(size = 55, hjust = 1),
    strip.text.x = element_text(size = 45, face = "bold", vjust = -1.5),
    axis.text.y = element_text(size = 45),
    strip.text.y = element_text(size = 14, face = "bold"),  # Modifica las etiquetas de las facetas
    axis.title = element_text(size = 55),
    plot.title = element_text(
      family = "title_font", size = 65, face = "bold", 
      hjust = 0.5, vjust = -2, lineheight = 0.35),
    plot.subtitle = element_text(size = 50, hjust = 0.5, vjust = -2.5),
    plot.caption = element_textbox(
      family = "caption_font", size = 45, hjust = 0.5, 
      halign = 0.5)
  )

#Visualizar el gráfico
stacked_bar_plot

#                                  c)Saving 
ggsave(
  filename ="stacked_bar_WHO.png",
  plot = stacked_bar_plot,
  width = 210,    # Best Twitter Aspect Ratio = 5:4
  height = 297,   
  units = "mm",
  dpi = 300,
  bg = "white"
)


#_______________________________________________________________________________

#                  3.- ¿Cuáles son las palabras más usadas? 


# ___________________________       Barplot     _____________________________ 
#                             Palabras en general 

#             a) Eliminar palabras bajo un criterio personal

remover_palabras <- c(stopwords(language = "pt"),
                      "multimedia", "message", "media", "omitted", "y", "la", "el", "en", "es", "si", "lo", "ya", "pero",
                      "esa", "los", "yo", "mi", "un", "con", "las", "omitido", "más", "eso", "al", "una", "del", "qué",
                      "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les", "pue", "ese", "son", "está",
                      "pues", "ahí", "sí", "ver", "estás", "algo", "vas", "ir", "voy", "creo", "fue", "solo", "ni",
                      "sólo", "nada", "aqui", "tú", "this", "was", "edited")

#                        b) Proceso y visualización
NWord_plot_pro <- miChat |>
  unnest_tokens(input = text, output = word) |>
  #descompone el texto en palabras individuales|col que contiene los datos|
  #nombre de la nueva col con las palabras
  filter(!word %in% remover_palabras) |>#Filtra las palabras de la columna word,
  #excluyendo las que aparecen en la lista remover_palabras
  count(word) |>#Cuenta el número de ocurrencias de cada palabra única en la columna word.
  top_n(20, n) |>
  arrange(desc(n)) |>
  ggplot(aes(x = reorder(word, n), y = n, fill = n, color = n)) +
  #reorderna las palabras según su frecuencia
  geom_col(width= 0.7, show.legend = FALSE, color= "white", linewidth=0.1)+
  scale_fill_paletteer_c("grDevices::Temps",direction = 1)+
  scale_color_paletteer_c("grDevices::Temps", direction = 1) +
  labs(
    title = "¿Cuáles son las palabras que más usamos?",
    subtitle = "Análisis de las palabras más recurrentes Sept 2023 - Ene 2025",
    x = "Palabras",
    y = "Número de veces que se utilizó",
    caption = plot_caption
  ) +
  coord_flip() +#Invierte los ejes, colocando las palabras en el eje vertical y 
  #las frecuencias en el horizontal.
  theme_minimal(base_family = "title_font") +
  theme(
    plot.background = element_rect(fill = bg_col, color = bg_col),
    text = element_text(color = text_col),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 40, color = text_col),
    axis.title = element_text(size = 40, face = "bold", color = text_col),
    plot.title = element_text(size = 65, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 50, hjust = 0.5),
    plot.caption = element_textbox(
      family = "caption_font", size = 35, hjust = 0.5, halign = 0.5, color = text_col
    )
  )

#                                Visualizar gráfico
NWord_plot_pro


#                                    c) Guardar
ggsave(
  filename ="words.png",
  plot = NWord_plot_pro,
  width = 210,    # Best Twitter Aspect Ratio = 5:4
  height = 297,   
  units = "mm",
  dpi = 300,
  bg = "white"
)

# ________________________  Palabras más usadas por autor  ______________________

#                               a) Data cleaning 
word_freq_per_user <- miChat |>
  unnest_tokens(input = text, output = word) |>
  filter(!word %in% remover_palabras) |>
  count(author, word, sort = TRUE) |>
  group_by(author) |>
  top_n(10, n) |>
  ungroup()

# Cambiar los nombres de los autores
word_freq_per_user <- word_freq_per_user |> 
  mutate(author = case_when(
    author == "Im a cyborg but that's ok" ~ "Yo",        # Cambiar "Im" a "Yo"
    author == "Mi mini Bióloga ᵐᵃᵍʸ ´･ᴗ･`" ~"Ella" 
  ))


#                             b) Making the graph 
word_freq_plot <- word_freq_per_user |>
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("Yo" = "#001a6e",   # Azul para el autor S
                               "Ella" = "#8b80fd")) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~author, scales = "free_y")+ # Dividir en paneles por autor
  labs(
    title = "¿Cuáles son las palabras que más usa cada uno?",
    subtitle = "Análisis de las palabras más frecuentes en los mensajes",
    x = "Palabras",
    y = "Frecuencia",
    caption = plot_caption
  ) +
  theme_minimal(base_family = "title_font") +
  theme(
    plot.background = element_rect(fill = bg_col, color = bg_col),
    text = element_text(color = text_col),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 40, color = text_col),
    axis.title.x = element_text(size = 45, face = "bold", color = text_col, margin = margin(t = 10)), # Más separación
    axis.title.y = element_text(size = 45, face = "bold", color = text_col),
    plot.title = element_text(size = 65, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 50, hjust = 0.5),
    strip.text = element_text(size = 40, face = "bold"), # Títulos de paneles
    plot.caption = element_textbox_simple(
      family = "caption_font", size = 35, hjust = 0.5, halign = 0.5, color = text_col,
      margin = margin(t = 10)
    ),
    plot.margin = margin(1, 1, 1, 1, "cm"))#ajusta los márgenes

# Mostrar el gráfico
word_freq_plot

#                                c) Guardar
ggsave(
  filename ="words_perperson.png",
  plot = word_freq_plot,
  width = 250,    # Best Twitter Aspect Ratio = 5:4
  height = 297,   
  units = "mm",
  dpi = 300,
  bg = "white"
)


