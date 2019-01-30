library(shiny)
library(ggplot2)
library(lintr)
load("userData_shiny_ass3.Rdata")

ui <- fluidPage(
  titlePanel("Q-Score Comparison App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain", "Domain:",
                  choices = unique(userData$domain_name)),
      checkboxGroupInput("class", "Show Grades:",
                         c("Grade 3" = 3, "Grade 4" = 4,
                           "Grade 5" = 5, "Grade 6" = 6,
                           "Grade 7" = 7, "Grade 8" = 8),
                         selected = sort(unique(userData$grade, decreasing = F))
                         )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("All", plotOutput("plot_all")),
                  tabPanel("Female", value = "f", plotOutput("plot_f")),
                  tabPanel("Male", value = "m", plotOutput("plot_m"))
      )
    )
  )
)

server <- function(input, output) {
  new_data <- reactive({
    subset(userData, userData$domain_name == input$domain
           & userData$grade %in% input$class)
  })

  output$plot_all <- renderPlot({
    ggplot(data = new_data(), aes(x = as.factor(grade), y = q_score)) +
      geom_boxplot() +
      ylab("Quantile Score") +
      theme_minimal() +
      theme(axis.title.x = element_blank()) +
      scale_x_discrete(breaks = seq(3, 8, 1),
                         labels = c("Grade 3", "Grade 4", "Grade 5",
                                    "Grade 6", "Grade 7", "Grade 8"))
  })

  output$plot_f <- renderPlot({
    ggplot(data = subset(new_data(), gender == "f"),
           aes(x = as.factor(grade), y = q_score)) +
      geom_boxplot() +
      ylab("Quantile Score") +
      theme_minimal() +
      theme(axis.title.x = element_blank()) +
      scale_x_discrete(breaks = seq(3, 8, 1),
                       labels = c("Grade 3", "Grade 4", "Grade 5",
                                  "Grade 6", "Grade 7", "Grade 8"))
  })

  output$plot_m <- renderPlot({
    ggplot(data = subset(new_data(), gender == "m"),
           aes(x = as.factor(grade), y = q_score)) +
      geom_boxplot() +
      ylab("Quantile Score") +
      theme_minimal() +
      theme(axis.title.x = element_blank()) +
      scale_x_discrete(breaks = seq(3, 8, 1),
                       labels = c("Grade 3", "Grade 4",
                                  "Grade 5", "Grade 6",
                                  "Grade 7", "Grade 8"))
  })
}

shinyApp(ui = ui, server = server)