#  Lab 2 - Dashboard Interativo Para AnÃ¡lise de Incidentes e Atos de ViolÃªncia

# Script de Construcao do Dashboard

# Pacotes
library(shiny)
library(shinydashboard)
library(scales)

# Frontend

# PÃ¡gina do dashboard
ui <- dashboardPage(
  # CabeÃ§alho
  dashboardHeader(title = "Dashboard Interativo Para AnÃ¡lise de Incidentes e Atos de ViolÃªncia"),
  
  # Barra lateral
  dashboardSidebar(selectInput("dataset", "Selecione a OpÃ§Ã£o:",
                               choices = c("Mortos Por Grupo Terrorista", 
                                           "RegiÃ£o do Ataque",
                                           "PaÃ­s do Ataque",
                                           "Tipo de Ataque", 
                                           "Tipo de Alvo do Ataque", 
                                           "Tipo de Arma", 
                                           "Nacionalidade Alvo")),
                   sliderInput("kills",
                               "Numero Maximo de Mortos:",
                               min = 0,
                               max = 20000, 
                               value = 40),
                   sliderInput("year",
                               "Ano:",
                               min = 1970,
                               max = 2017, 
                               value = 1970,#Valor que vai comecar a busca 
                               animate = TRUE,#Botão do play
                               sep = ""),
                   actionButton("update", "Atualizar o Dashboard")),
  
  # Corpo da pagina  #GRAFICOS UTILIZADOS 
  dashboardBody(fluidRow(column(width = 6,
                                box(plotOutput("bubblePlot", height = 350), width = NULL),
                                box(plotOutput("barChart", height = 350), width = NULL)),
                         column(width = 6, 
                                box(tableOutput("view"), width = NULL)))))


# Backend

# Funcão para organizacao dos dados
server <- function(input, output) {
  
  # Input dos dados
  datasetInput <- eventReactive(input$update, {switch(input$dataset,
                                                      "Mortos Por Grupo Terrorista" = GroupTable,
                                                      "RegiÃ£o do Ataque" = RegionTable,
                                                      "PaÃ­s do Ataque" = countryTable,
                                                      "Tipo de Ataque" = attacktypeTable,
                                                      "Tipo de Alvo do Ataque" = targtypeTable,
                                                      "Tipo de Arma" = WeaponTable,
                                                      "Nacionalidade Alvo" = natltyTable)},
                                ignoreNULL = FALSE)
  
  # Titulo
  TitleInput <- eventReactive(input$update, {switch(input$dataset,
                                                    "Mortos Por Grupo Terrorista" = "Grupo Terrorista",
                                                    "RegiÃ£o Onde Ocorreu o Ataque" = "RegiÃµes",
                                                    "PaÃ­s Onde Ocorreu o Ataque" = "PaÃ­ses",
                                                    "Tipo de Ataque" = "Tipo de Ataque",
                                                    "Tipo de Alvo do Ataque" = "Tipo de Alvo",
                                                    "Tipo de Arma" = "Tipo de Arma",
                                                    "Nacionalidade Alvo" = "Nacionalidade Alvo")},
                              ignoreNULL = FALSE)
  
  # Grafico de bolhas
  output$bubblePlot <- renderPlot({
    
    # Dataset
    dataset <- datasetInput()
    
    # TÃ­tulo
    Title <- TitleInput()
    
    # Limites
    xmin <- min(dataset$attack_count)
    xmax <- max(dataset$attack_count)
    ymin <- min(dataset$kills_total)
    ymax <- max(dataset$kills_total)
    
    # Dataset
    new_data1 <-dataset[!(dataset$year != input$year | dataset$kills_total < input$kills),]
    
    # CriaÃ§Ã£o do grÃ¡fico
    ggplot(new_data1, aes(x = attack_count, y = kills_total, color = Title, size = kills_mean)) +
      geom_point(alpha = 0.5) +
      scale_size(range = c(3, 12)) +
      scale_color_viridis_d() +
      scale_y_log10(limits = c(ymin + 1, ymax), labels = scales::comma) +
      scale_x_log10(limits = c(xmin, xmax), labels = scales::comma) +
      ylab("Total de Mortos (Escala de Log)") + 
      xlab("Total de Ataques (Escala de Log)") + 
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = .5),
        axis.title.x = element_text(face = "bold", size = 11, vjust = 0),
        axis.title.y = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(face = "bold", size = 10)
      )
  })
  
  # GrÃ¡fico de barras
  output$barChart <- renderPlot({
    
    # Dataset
    dataset <- datasetInput()
    
    # TÃ­tulo
    Title <- TitleInput()
    
    # Ajuste nos dados
    new_data2 <- dataset[!(dataset$kills_total < input$kills | dataset$year > input$year),]
    new_data2 <- new_data2 %>% group_by(year) %>% tally() %>% 
      arrange(year) %>% 
      mutate(color = if_else(year - 1 == lag(year), 
                             if_else(n > lag(n), "Increase", 
                                     if_else(n == lag(n), "Same", "Decrease")), "Increase"))
    new_data2$color[1] = "Increase"
    new_data2v2 <- dataset[!(dataset$kills_total < input$kills),]
    new_data2v2 <- new_data2v2 %>% group_by(year) %>% tally() 
    ymax <- max(new_data2v2$n)
    
    # CriaÃ§Ã£o do grÃ¡fico
    ggplot(new_data2, aes(x = year, y = n)) + geom_col(color = "#07415c", fill = "#5cbdb7", width = 1) +
      labs(title = Title) +
      ylab(paste(Title)) + 
      xlab("Year") +
      theme_bw() + 
      xlim(1969, 2018) +
      ylim(0, ymax) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = .5),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12, vjust = 3),
        axis.text = element_text(face = "bold")
      )
  })
  
  # Tabela
  output$view <- renderTable({
    
    # Dataset
    dataset <- datasetInput()
    
    # TÃ­tulo
    Title <- TitleInput()
    
    # Ajuste do dataset
    new_data3 <- dataset[!(dataset$year != input$year | dataset$kills_total < input$kills),]
    new_data3 <- new_data3 %>% select(1, attack_count, kills_total, kills_mean) %>% 
      arrange(kills_total, decreasing = TRUE) %>%
      mutate(attack_count = comma(attack_count), kills_total = comma(kills_total), kills_mean = comma(kills_mean))
    
    # Nomes das colunas
    colnames(new_data3)[1] <- Title
    colnames(new_data3)[2] <- "Numeros de Ataques"
    colnames(new_data3)[3] <- "Total de Mortos"
    colnames(new_data3)[4] <- "Media de Mortos"
    head(new_data3, n = nrow(new_data3))
  })
}

# ExecuÃ§Ã£o
shinyApp(ui, server)