library(shiny)
library(figquest)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(textOutput("question_text"), width = 12)
  ),
  fluidRow(
    radioButtons(
      "preference", "Jouw voorkeur", inline = TRUE, choiceValues = 0:5,
      choiceNames = c(
        "zeker A", "eerder A", "misschien A", "misschien B", "eerder B",
        "zeker B"
      ),
      selected = character(0)
    ),
    actionButton("next", "volgende")
  ),
  fluidRow(
    column(plotOutput("plot_a", height = "600px"), width = 6),
    column(plotOutput("plot_b", height = "600px"), width = 6)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$question_text <- renderText(session$token)

  data <- reactiveValues(
    dataset = NULL,
    design = generate_design(
      design_effect = list(y_label = c("index", "change")),
      design_data = list(
        size = "strong", threshold = log(0.75), reference = "none", ci = "none",
        effect = "none"
      ),
      max_question = 10
    ),
    question = NULL
  )

  observeEvent(data$design, {
    if (is.null(data$design)) {
      return(NULL)
    }
    data$question <- 1
  })

  observeEvent(data$question, {
    if (is.null(data$question)) {
      return(NULL)
    }
    data$dataset <- generate_data(
      size = as.character(data$design$size[data$question]),
      threshold = data$design$threshold[data$question]
    )
    names_to <- paste0("name_", session$token)
    values_to <- paste0("value_", session$token)
    design_to <- paste0("design_", session$token)
    data$design[data$question, ] %>%
      pivot_longer(
        matches("_[ab]$"), names_to = names_to, values_to = values_to
      ) %>%
      extract(names_to, c(design_to, session$token), "(.*)_([ab])$") %>%
      pivot_wider(
        names_from = !!design_to, values_from = !!values_to
      ) -> data$design_ab
  })

  output$plot_a <- renderPlot({
    if (is.null(data$dataset)) {
      return(NULL)
    }
    selected <- data$design_ab[, session$token] == "a"
    create_figure(
      dataset = data$dataset, reference = data$design_ab$reference[selected],
      y_label = data$design_ab$y_label[selected],
      ci = data$design_ab$ci[selected], effect = data$design_ab$effect[selected]
    ) +
      ggtitle("A")
  })

  output$plot_b <- renderPlot({
    if (is.null(data$dataset)) {
      return(NULL)
    }
    selected <- data$design_ab[, session$token] == "b"
    create_figure(
      dataset = data$dataset, reference = data$design_ab$reference[selected],
      y_label = data$design_ab$y_label[selected],
      ci = data$design_ab$ci[selected], effect = data$design_ab$effect[selected]
    ) +
      ggtitle("B")
  })

}
# Run the application
shinyApp(ui = ui, server = server)
