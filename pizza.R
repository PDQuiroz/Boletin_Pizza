# Shiny App.io
# Cargando Bibliotecas
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringr)
library(readr)
library(magrittr)
library(purrr)
library(tm)
library(tidyr)


# Cargando el Dataset
# Working directory para crear una lista
# setwd("/Users/pablo/Desktop/2021.02/01. Altiiro/05. Text Analytics/06. Boletines/Boletin_2 Pizza")

# Ocupando Filepaths para listar archivos csv
filePaths <- list.files("./datasets", "\\.csv$", full.names = TRUE)

# Testeando que funciones
read_csv(filePaths[1])

# ocupando purrr leemos y juntamos los archivos
my_data <- map_dfr(filePaths, read_csv)

colnames(my_data)

# Removiendo Extras
my_data <- my_data %>% select(-'ODSEW-ShBeI-QClCJf-QYOpke-title', -'ODSEW-ShBeI-QClCJf-QYOpke-VdSJob',
                              -'ODSEW-ShBeI-text 2', -'ODSEW-ShBeI-hWJfub-yHKmmc-tv6Bve-hidden', 
                              -'ODSEW-ShBeI-hWJfub-yHKmmc-NnAfwf', -'uEubGf', -'bC3Nkc'
                              ) %>% 
  drop_na(Comentario)

# Resultado Final
my_data

# Cargando AFINN para Analysis de Sentimiento
afinn <- read.csv("./lexico/afinn_es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

afinn <- rename(afinn, "word" = "Palabra")
afinn <- rename(afinn, "english_word" = "Word")
afinn

########################## Cambios de Valor en AFINN

# Descuento Estaba como -1
afinn$Puntuacion[afinn$word == "descuento"] <- 1






# Cargando StopWords en Castellano
stopwords_es <- stopwords(kind  = "es")

# Cargando StopWords en Inglés
data("stop_words")

# Convirtiendo a Tibble + Cambio de Nombre
stopwords_es <- as.data.frame(stopwords_es)
stopwords_es <- rename(stopwords_es, "word" = "stopwords_es")
stopwords_es$lexicon <- "snowball"

# Custom Stop Words
cust_stopwords <- data.frame(word = c("by", "google", "translate", "translated"))
cust_stopwords

# Palabras de Interés
interes_words <- data.frame(word = c("covid", "COVID", "Covid"))
interes_words

############## Corrigiendo Fecha
my_data <- rename(my_data,  fecha = Periodo)
unique(my_data$fecha)

# Cambiando Fechas a valores Numericos
my_data$fecha %<>%
  gsub("27 minutes ago", "0", .) %>% 
  gsub("51 minutes ago", "0", .) %>% 
  gsub("19 hours ago", "0", .) %>% 
  gsub("4 hours ago", "0", .) %>% 
  gsub("7 hours ago", "0", .) %>%
  gsub("a day ago", "0", .) %>% 
  gsub("2 days ago", "0", .) %>% 
  gsub("3 days ago", "0", .) %>% 
  gsub("4 days ago", "0", .) %>% 
  gsub("5 days ago", "0", .) %>% 
  gsub("6 days ago", "0", .) %>% 
  gsub("a week ago", "0", .) %>% 
  gsub("2 weeks ago", "0", .) %>% 
  gsub("3 weeks ago", "0", .) %>% 
  gsub("4 weeks ago", "0", .) %>% 
  gsub("a month ago", "1", .) %>% 
  gsub("2 months ago", "2", .) %>% 
  gsub("3 months ago", "3", .) %>% 
  gsub("4 months ago", "4", .) %>% 
  gsub("5 months ago", "5", .) %>% 
  gsub("6 months ago", "6", .) %>% 
  gsub("7 months ago", "7", .) %>% 
  gsub("8 months ago", "8", .) %>% 
  gsub("9 months ago", "9", .) %>% 
  gsub("10 months ago", "10", .) %>% 
  gsub("11 months ago", "11", .) %>% 
  gsub("a year ago", "12", .) %>% 
  gsub("2 years ago", "24", .) 

# Revisando el Resultado
unique(my_data$fecha)

# Convirtiendo a Numeros
my_data$fecha <- as.numeric(my_data$fecha)

# Revisando Resultado
sample(my_data$fecha, size = 10, replace = FALSE)



############### Palabras
my_data %>% 
  filter(str_detect(Comentario, regex("covid", ignore_case = T))) %>% 
  select(Comentario)



################ Tokens
# remove tokens_ "by", "google", "translated"
tokens <- my_data %>% 
    unnest_tokens(word, Comentario) %>% 
    anti_join(stopwords_es) %>% 
    anti_join(stop_words) %>% 
    anti_join(cust_stopwords)
tokens



cust_stopwords2 <- data.frame(word = c("si", "cerveza", "service", "rich", "attention",
                                       "food"))

# Top Tokens por Frecuencia
tokens %>% 
  count(word) %>% 
  anti_join(cust_stopwords) %>% 
  anti_join(cust_stopwords2) %>% 
  arrange(desc(n)) %>% 
  filter(n < 70 & n > 40) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% # para que salgan en orden
  ggplot(aes(x = word, y = n, fill = n)) + geom_col() +
  coord_flip() +
  xlab("Palabra") + ylab("Cantidad") + 
  theme_classic()


################# TF - IDF
# Tokens de valor por periodo
freq_tf_idf <- tokens %>% 
  count(word, fecha, sort =T) %>% 
  mutate(frequency = n/sum(n)) %>% 
  mutate(index = row_number()) %>% 
  bind_tf_idf(word, fecha,  n) 


freq_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  filter(tf_idf >= 0.0323) %>%
  mutate(word = reorder(word, tf_idf)) %>% # para que salgan en orden
  ggplot(aes(x = word, y = tf_idf, fill = n)) + geom_col() +
  coord_flip() +
  theme_classic()


freq_tf_idf %>% 
  arrange(desc(tf_idf))

###################### GGRAPH - NGRAMS
library(igraph)
library(ggraph)
library(gsubfn)

create_trigrams <- function(data){
  data %>% 
    unnest_tokens(bigram, Comentario, token = "ngrams", n = 3) %>% 
    filter(!is.na(bigram)) %>% 
    separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
    filter(!word1 %in% stopwords_es$word) %>%
    filter(!word2 %in% stopwords_es$word) %>% 
    filter(!word3 %in% stopwords_es$word) %>% 
    filter(!word1 %in% cust_stopwords$word) %>% 
    filter(!word2 %in% cust_stopwords$word) %>% 
    filter(!word3 %in% cust_stopwords$word) %>% 
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word) %>% 
    filter(!word3 %in% stop_words$word) %>% 
    count(word1, word2, word3, sort = TRUE)
  
}

trigram <- create_trigrams(my_data)

trigram %>% 
  filter(n >= 2)
# kiss my hass -> aperitivo del honesto mike

# Nube de palabras
create_ngram_cloud <- function(data, filters ){
  
  set.seed(1989)
  a <- grid::arrow(type = "closed", length = unit(.14, "inches"))
  
  data %>% 
    filter(n >= filters) %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr")+
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches'))  +
    geom_node_point(color = "lightblue", size = 2) +
    geom_node_text(aes(label=name), vjust =1, hjust=1) + 
    theme_void()
  
}

create_ngram_cloud(trigram, 2)



######### Analisis de Sentimiento

# Positive
tokens %>% inner_join(afinn) %>% 
  count(word, Puntuacion, sort = T) %>% 
  anti_join(cust_stopwords2) %>% 
  filter(Puntuacion >= 1 & Puntuacion <= 2) %>% 
  top_n(12) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(x = word, y = n, fill = n)) + geom_col() + coord_flip()+
  theme_bw()

negative_stop <- data.frame(word = c("mal"))
# Negative
tokens %>% inner_join(afinn) %>% 
  anti_join(negative_stop) %>% 
  count(word, Puntuacion, sort = T) %>% 
  anti_join(cust_stopwords2) %>% 
  filter(Puntuacion >= -3 & Puntuacion <= -1) %>% 
  top_n(12) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(x = word, y = n, fill = n)) + geom_col() + coord_flip()+
  theme_bw()

# Relación al servicio y tiempo de espera
tokens %>%  inner_join(afinn) %>%
  count(word, Puntuacion, fecha, sort = T) %>% 
  filter(Puntuacion < 0) %>% 
  arrange((fecha)) %>% 
  group_by(fecha) %>% 
  top_n(2) %>% 
  tail()

######################## Sentiment Plotly
library(plotly)

# Plotly Data

plotly_data <- tokens %>% 
    inner_join(afinn) %>% 
    group_by(fecha, Puntuacion) %>% 
    filter(fecha < 12) %>% 
    count(word, fecha, index = row_number(), Puntuacion) %>% 
    mutate(total_Puntuacion = sum(Puntuacion)) %>% 
    ungroup() %>% 
    group_by(fecha) %>% 
    mutate(total_n = sum(n)) %>% 
    select(fecha, word, Puntuacion, total_Puntuacion, total_n) %>% 
    arrange(desc(Puntuacion))
plotly_data

# Box Plotly Plot
box_plotly <- ggplot(plotly_data) +
  geom_hline(yintercept = 0, color = 'pink') + 
  geom_boxplot( aes(x = fecha, y = total_Puntuacion, group = fecha,
                    ),  width = 0.3) +
  geom_line(aes(x = fecha, y = total_n), colour = "darkBlue") +
  xlab("Meses Atrás") +
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "Comentarios por Mes",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*10, name="Valor Sentimiento")
  ) +
  labs(title="Sentimiento por Mes", 
       subtitle="Puntuación Afinn") +
  theme_bw()

box_plotly

# Column Plotly Plot
col_plotly <- ggplot(plotly_data) +
  geom_hline(yintercept = 0, color = 'pink') + 
  geom_col(aes(x = fecha, y = total_Puntuacion,  
                fill = Puntuacion,
               text = word), width = 0.3) +
  xlab("Meses Atrás") +
  ylab("Total Sentimiento")+
  labs(title="Sentimiento por Mes", 
       subtitle="Puntuación Afinn") +
  scale_y_continuous()+
  theme_bw()

col_plotly

ggplotly(col_plotly, tooltip = c("text", "Puntuacion")) 



############### Shiny App
#Shiny Widgets ------------------------------------------------------

library(shinyWidgets)
library(shinydashboard)
# shinyWidgetsGallery()


#Server---------------------------------------------------------------
library(shiny) # loading shiny
library(shinythemes)

# install.packages("bslib")
# library(bslib)

library(plotly)
library(markdown)

ui <- fluidPage(
  titlePanel("Análisis de Comentarios de Restaurantes de Pizza"),
  h5("Dplace - Julio, 2021"),
  br(),
  p("Hecho con datos extraídos de Google Reviews y Trip Advisor."),
  br(), br(),
  # shinythemes::themeSelector(),
  theme = shinythemes::shinytheme('yeti'),
  navbarPage("", 
             
             tabPanel("Flujo de Comentarios",
                      h3("Tri-Grams"),
                      p("Las palabras escritas en secuencia más veces seguidas"),
                      sliderInput(inputId = 'flujo', label = 'Frecuencia de Uso',
                                  min = 2, max = 5, value = 2, step = 1),
                      plotOutput(outputId = "ggraph"),
                      DT::DTOutput('tri_grams'),
                      br()
                      ), # TB Flujo Close
             
             tabPanel("Palabras de Interés",
                      
                      fluidRow(
                        
                                  column(6,
                                    h3("Palabras mas Frecuentes"),
                                    p("Los ", em("Tokens"), " con mayor repetición."),
                                    br(),
                                    sliderInput(inputId = 'cantidad', label = 'Cantidad de Palabras',
                                                min = 1, max = 20, value = 10, step = 1),
                                    plotOutput(outputId = "palabras_max"), br()),
                                    
                                  column(6,
                                    h3("Palabras por Sentimiento Objetivo"),
                                    p("Cada palabra tiene un valor. Ej. bueno = 2, pesimo = -3"),
                                    br(),
                                    sliderInput(inputId = 'sentiment', label = 'Filtro por Sentimiento',
                                                min = -3, max = 3, value = c(-2,2), step = 1),
                                    plotOutput(outputId = "palabras_sentiment"), br())
                                    
                      ), # Cerrar Fluid Row
                      
                      h3("Palabra por Frecuencia Inversa de Documento"),
                      p("Cada documento o comentario tiene palabras más comunes que otras. Este algoritmo nos nuestra las palabras de valor que se repiten menos pero que tiene más contenido que simplemente decir bueno."),
                      br(),
                      radioButtons(inputId = 'tf_idf', label = 'Filtrar Por Frecuencia',
                                  choices = c(0.019, 0.0235, 0.0323), selected = 0.0323),
                      plotOutput(outputId = "palabras_tf_idf"),
                      br()
                      ), #TB Palabras Close
             
             
             tabPanel("Comentarios y Sentimientos",
                      
                      h3("Total Comentarios y Valores por Mes"),
                      p("Cada palabra tiene un valor numérico. Por ejemplo, ", strong("Malo")," = -2  y  " ,strong("Bueno"), " = +2. Esto nos permite comprender el", em("sentimiento promedio"), "de cada mes."),
                      br(),
                      h4("Boxplot"),
                      p("Aca podemos ver todos los comentarios al igual que el valor agregado de los sentimientos de estos"),
                      br(),
                      plotOutput(outputId = "box_plot"),
                      
                      br(),
                      h4("Gráfico Interactivo"),
                      p("Revisa las palabras negativas o positivas de cada mes"),
                      plotlyOutput(outputId = "plotly_plot"),
                      br()
             ), # TB Comentario Close
             
             tabPanel("Buscar Comentarios",
                      h3("Buscar Comentarios por Palabras Claves"),
                      br(),
                      textInput(inputId = 'palabra', label = 'Elegir Palabra', value = "protocolo"),
                      DT::DTOutput('palabra_en_comentarios'),
                      br()
             ) # TB Comentario Close
            
             
            ) # Navbar Close
)# UI Close




server <- function(input, output, session){
  
  # Palabras mas Comunes
  output$palabras_max <- renderPlot({
    
    tokens %>% 
      count(word) %>% 
      anti_join(cust_stopwords) %>% 
      top_n(input$cantidad)%>% 
      arrange(desc(n)) %>% 
      mutate(word = reorder(word, n)) %>% # para que salgan en orden
      ggplot(aes(x = word, y = n, fill = n)) + geom_col() +
      coord_flip() +
      theme_classic()
    
    
  }) # Cerrando Palabras Comunes
  
  # Filtrando por Sentimiento
  output$palabras_sentiment <- renderPlot({
    
    tokens %>% inner_join(afinn) %>% 
      count(word, Puntuacion, sort = T) %>% 
      filter(Puntuacion >= input$sentiment & Puntuacion <= input$sentiment) %>% 
      top_n(10) %>% 
      mutate(word = reorder(word,n)) %>% 
      ggplot(aes(x = word, y = n, fill = n)) + geom_col() + coord_flip() +
      theme_classic()
    
    
  }) # Cerrando Sentimiento
  
  # Palabras de valor segun TF IDF
  output$palabras_tf_idf <- renderPlot({
    
    freq_tf_idf %>% 
      arrange(desc(tf_idf)) %>% 
      filter(tf_idf >= input$tf_idf) %>%
      mutate(word = reorder(word, tf_idf)) %>% # para que salgan en orden
      tail(10) %>% 
      ggplot(aes(x = word, y = tf_idf, fill = n)) + geom_col() +
      coord_flip() +
      theme_classic()
    
    
  }) # Cerrando TF IDF
  
  # Tabla de Palabras
  output$palabra_en_comentarios <- DT::renderDT({
    
    # Filtro por Palabra
    filter_word <- my_data %>% 
      filter(str_detect(Comentario, regex(input$palabra, ignore_case = T))) %>% 
      select(Comentario)
    
    # Unica manera de remover el searchbar
    DT::datatable(filter_word,options = list(dom = 'tp')) 
    
  }) # Cerrando palabra_en_comentarios

  
  # Tabla de Flujo
  output$tri_grams <- DT::renderDT({
    
    # Filtro por Cantidad
    trigrams <- trigram %>% 
      filter(n >= input$flujo)
    
    # Unica manera de remover el searchbar
    DT::datatable(trigrams, options = list(dom = 'ltp')) 
    
  }) # Cerrando palabra_en_comentarios
  
  
  # Ggraph
  output$ggraph <- renderPlot({
    
    create_ngram_cloud(trigram, input$flujo)
    
  }) # Cerrando ggraph
  
  
  # Box Plot
  output$box_plot <- renderPlot({
    
    box_plotly
    
  }) # Cerrando Box Plot
  
  # Plotly Plot
  # ocupar renderPlotly
  output$plotly_plot <- renderPlotly({
    
    ggplotly(col_plotly, tooltip = c("text", "Puntuacion")) 
    
    
  }) # Cerrando Plotly

} # Cerrando Server



shinyApp(ui = ui, server = server)

# install.packages('rsconnect')

 


















