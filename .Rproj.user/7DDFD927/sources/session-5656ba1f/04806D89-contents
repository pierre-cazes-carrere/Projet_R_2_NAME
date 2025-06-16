# app.R

library(shiny)
library(tidyverse)

# Charger et nettoyer les données
data <- read_csv("data/ObesityDataSet_raw_and_data_sinthetic.csv") %>%
  mutate(
    BMI = Weight / (Height^2),
    AgeGroup = case_when(
      Age < 18 ~ "Adolescent",
      Age < 30 ~ "Jeune adulte",
      Age < 45 ~ "Adulte",
      TRUE     ~ "Senior"
    ),
    across(c(Gender, family_history_with_overweight, FAVC, CAEC, SMOKE, SCC, CALC, MTRANS, NObeyesdad), as.factor)
  ) %>%
  filter(
    between(BMI, quantile(BMI, 0.01), quantile(BMI, 0.99)),
    between(Age, 14, 60),
    between(Height, 1.45, 2),
    between(Weight, 40, 170)
  ) %>%
  distinct()

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Exploration des facteurs liés à l'obésité"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Genre :", choices = unique(data$Gender)),
      selectInput("agegroup", "Tranche d'âge :", choices = unique(data$AgeGroup))
    ),
    mainPanel(
      h4("Répartition des classes d'obésité"),
      plotOutput("obesityDist"),
      h4("Habitudes alimentaires : grignotage (CAEC)"),
      plotOutput("snackPlot"),
      h4("Activité physique (FAF) par classe d'obésité"),
      plotOutput("activityPlot"),
      h4("Moyen de transport par classe d'obésité"),
      plotOutput("transportPlot")
    )
  )
)

# Serveur
server <- function(input, output) {
  
  filteredData <- reactive({
    data %>%
      filter(Gender == input$gender, AgeGroup == input$agegroup)
  })
  
  output$obesityDist <- renderPlot({
    ggplot(filteredData(), aes(x = NObeyesdad, fill = NObeyesdad)) +
      geom_bar() +
      labs(title = "Classes d'obésité", x = "Catégorie", y = "Nombre") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$snackPlot <- renderPlot({
    ggplot(filteredData(), aes(x = CAEC, fill = NObeyesdad)) +
      geom_bar(position = "dodge") +
      labs(title = "Grignotage entre les repas", x = "Fréquence", y = "Nombre")
  })
  
  output$activityPlot <- renderPlot({
    ggplot(filteredData(), aes(x = NObeyesdad, y = FAF, fill = NObeyesdad)) +
      geom_boxplot() +
      labs(title = "Activité physique par catégorie", y = "Temps d'activité physique") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$transportPlot <- renderPlot({
    ggplot(filteredData(), aes(x = MTRANS, fill = NObeyesdad)) +
      geom_bar(position = "dodge") +
      labs(title = "Transport utilisé selon la catégorie", x = "Mode de transport", y = "Nombre") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)


