library(shiny)
library(figquest)
library(dplyr)
library(tidyr)
library(ggplot2)

max_question <- 10

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(htmlOutput("question_text"), width = 12)
  ),
  fluidRow(
    radioButtons(
      "preference", "Jouw voorkeur", inline = TRUE, selected = character(0),
      choiceValues = 1:6 - mean(1:6),
      choiceNames = c(
        "zeker A", "eerder A", "misschien A", "misschien B", "eerder B",
        "zeker B"
      )
    ),
    actionButton("next_button", "volgende")
  ),
  fluidRow(
    column(plotOutput("plot_a", height = "600px"), width = 6),
    column(plotOutput("plot_b", height = "600px"), width = 6)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  data <- reactiveValues(
    answer = numeric(0),
    dataset = NULL,
    design = NULL,
    level = 1,
    level_question = c(
"Verkies je labels op de y as als een index of als een relatief verschil?
Bij een index vermenigvuldigen we de cijfers met een vast getal zodat de waarde
in het referentiejaar gelijk is aan 100.
Bij een relatief verschil geven we de procentuele wijziging ten opzichte van
de waarde in het referentiejaar.",
"Beide figuren zijn een andere manier van weergave van de dezelfde tijdreeks.
In dit geval verschillen ze in de manier waarop we referentielijnen weergeven.
We nemen jouw voorkeur uit eerder antwoorden mee.
Welke figuur kan je het makkelijkst interpreteren?",
"Beide figuren zijn een andere manier van weergave van de dezelfde tijdreeks.
In dit geval verschillen ze in de manier waarop we onzekerheid weergeven.
We nemen jouw voorkeur uit eerder antwoorden mee.
Welke figuur kan je het makkelijkst interpreteren?",
"Beide figuren zijn een andere manier van weergave van de dezelfde tijdreeks.
In dit geval verschillen ze in de manier waarop we de interpretatie van het
effect weergeven.
We nemen jouw voorkeur uit eerder antwoorden mee.
Welke figuur kan je het makkelijkst interpreteren?"
),
    preferred = list(),
    question = NULL
  )

  output$test <- renderText("test")

  output$question_text <- renderText(
    sprintf(
      "%s<br>deelvraag %i van %i", data$level_question[data$level],
      data$question, nrow(data$design)
    )
  )

  observeEvent(data$level, {
    if (data$level == 1) {
      data$design <- generate_design(
        design_effect = list(y_label = c("index", "change")),
        design_data = list(
          size = "strong", threshold = log(0.75), reference = "none",
          ci = "none", effect = "none"
        ),
        max_question = max_question
      )
      return(NULL)
    }
    if (data$level == 2) {
      data$design <- generate_design(
        design_effect = list(reference = c("none", "lines", "lines + text")),
        design_data = c(
          data$preferred,
          list(
            size = c("stable", "moderate", "strong"), ci = "none",
            threshold = log(c(0.9, 0.75)), effect = "none"
          )
        ),
        max_question = max_question
      )
      return(NULL)
    }
    if (data$level == 3) {
      data$design <- generate_design(
        design_effect = list(ci = c("none", "band", "gradient")),
        design_data = c(
          data$preferred,
          list(
            size = c("stable", "moderate", "strong", "potential"),
            threshold = log(c(0.9, 0.75)), effect = "none"
          )
        ),
        max_question = max_question
      )
      return(NULL)
    }
    if (data$level == 4) {
      if (data$preferred$ci == "none") {
        effect <- c("none", "symbol", "colour symbol")
      } else {
        effect <- c(
          "none", "symbol", "colour symbol", "colour ci", "symbol + colour ci"
        )
      }
      data$design <- generate_design(
        design_effect = list(effect = effect),
        design_data = c(
          data$preferred,
          list(
            size = c("stable", "moderate", "strong", "potential"),
            threshold = log(c(0.9, 0.75))
          )
        ),
        max_question = max_question
      )
      return(NULL)
    }
    output$plot_a <- renderPlot(NULL)
    output$plot_b <- renderPlot(NULL)
    output$question_text <- renderText(
      "<h1>Hartelijk bedankt om deel te nemen aan ons onderzoek.</h1>"
    )
    updateActionButton(session = session, inputId = "next_button", label = "")
    updateRadioButtons(
      session = session, inputId = "preference", choices = "", label = ""
    )
  })

  observeEvent(data$design, {
    if (is.null(data$design)) {
      return(NULL)
    }
    data$question <- 1
  })

  observeEvent(data$question, {
    if (is.null(data$question)) {
      output$question_text <- renderText(data$level_question[data$level])
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

  observeEvent(input$next_button, {
    if (data$level > length(data$level_question)) {
      return(NULL)
    }
    if (is.null(input$preference)) {
      output$question_text <- renderText(
        sprintf(
          "%s<br>deelvraag %i van %i\n%s", data$level_question[data$level],
          data$question, nrow(data$design),
          "<p style='color:red;'>Gelieve de vraag eerst te beantwoorden.</p>"
        )
      )
      return(NULL)
    }
    data$answer <- c(data$answer, as.numeric(input$preference))
    updateRadioButtons(
      session = session, inputId = "preference", selected = character(0)
    )
    output$question_text <- renderText(
      sprintf(
        "%s<br>deelvraag %i van %i", data$level_question[data$level],
        data$question, nrow(data$design)
      )
    )
    if (data$question == nrow(data$design)) {
      data$preferred <- c(
        data$preferred, calc_preferred(data$design, data$answer)
      )
      data$answer <- numeric(0)
      data$level <- data$level + 1
      data$question <- NULL
    } else {
      data$question <- data$question + 1
    }
  })
}

calc_preferred <- function(ds, answer) {
  ds %>%
    select(matches("_[ab]$")) %>%
    mutate(answer = answer) %>%
    pivot_longer(-.data$answer) %>%
    extract(.data$name, c("name", "plot"), "(.*)_([ab])$") %>%
    mutate(answer = ifelse(.data$plot == "a", -1, 1) * .data$answer) %>%
    group_by(.data$name, .data$value) %>%
    summarise(score = mean(.data$answer), .groups = "drop") %>%
    slice_max(.data$score, n = 1, with_ties = FALSE) -> pref
  setNames(pref$value, pref$name)
}

# Run the application
shinyApp(ui = ui, server = server)
