library(shiny)
library(figquest)
library(dplyr)
library(tidyr)
library(ggplot2)

max_question <- 10
max_interpretation <- 4

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    id = "hidden_tabs", type = "hidden",
    tabPanelBody(
      "introduction",
      fluidRow(column(h1("Inleiding"), width = 10, offset = 1)),
      fluidRow(
        column(
          sprintf(
"Het INBO publiceert een aantal natuurindicatoren waarbij we allerhande trends
grafisch weergeven.
Met deze vragenlijst willen we onderzoeken wat voor figuur hiervoor het meest
geschikt is.
We starten met onderstaande vragen over jou.
Daarmee willen we onderzoeken of er een verband is tussen jouw kennis en jouw
voorkeuren wat betreft figuren.
Vervolgens vragen we je om %i standaard figuren te interpreteren (multiple
choise).
Daarna geven we je in 4 stappen de mogelijkheid om de figuren te verbeteren.
Hiertoe krijg je telkens 2 figuren naast elkaar te zien die op basis van een
stijlkenmerk van elkaar verschillen.
De gegevens in beide figuren is identiek.
Voor elk van deze stappen tonen we maximum %i paren van figuren.
Tenslotte leggen we je terug %i figuren ter interpretatie voor.
Ditmaal in de stijl waar jouw voorkeur naar uitgaat.",
            max_interpretation, max_question, max_interpretation
          ),
          width = 10, offset = 1
        )
      ),
      fluidRow(
        column(h2("Een beetje achtergrond over jou"), width = 10, offset = 1)
      ),
      fluidRow(
        column(
          radioButtons(
            "math", "Hoe vertrouwd voel jij je met cijfers?", inline = TRUE,
            selected = character(0),
            choices = c(
              "niet vertrouwd", "weinig vertrouwd", "vertrouwd",
              "zeer vertrouwd"
            )
          ),
          width = 10, offset = 1
        ),
      ),
      fluidRow(
        column(
          radioButtons(
            "stats", "Hoe vertrouwd voel jij je met statistiek?", inline = TRUE,
            selected = character(0),
            choices = c(
              "niet vertrouwd", "weinig vertrouwd", "vertrouwd",
              "zeer vertrouwd"
            )
          ),
          width = 10, offset = 1
        )
      ),
      fluidRow(
        column(
          radioButtons(
            "colourblind", "Ben je kleurenblind?", inline = TRUE,
            selected = character(0),
            choices = c("ja", "nee", "wil liever niet vertellen")
          ),
          width = 10, offset = 1
        ),
        column(actionButton("next_intro", "volgende"), width = 5, offset = 1)
      ),
      fluidRow(column(htmlOutput("intro_problem"), width = 5, offset = 1))
    ),
    tabPanelBody(
      "comparison",
      fluidRow(
        column(htmlOutput("question_text"), width = 10, offset = 1)
      ),
      fluidRow(
        column(
          radioButtons(
            "preference", "Jouw voorkeur", inline = TRUE,
            selected = character(0), choiceValues = 1:6 - mean(1:6),
            choiceNames = c(
              "zeker A", "eerder A", "misschien A", "misschien B", "eerder B",
              "zeker B"
            )
          ),
          width = 10, offset = 1
        ),
        column(actionButton("next_button", "volgende"), width = 5, offset = 1)
      ),
      fluidRow(
        column(plotOutput("plot_a", height = "600px"), width = 5, offset = 1),
        column(plotOutput("plot_b", height = "600px"), width = 5, offset = 1)
      )
    ),
    tabPanelBody(
      "exam",
      fluidRow(
        column(htmlOutput("question_exam"), width = 10, offset = 1)
      ),
      fluidRow(
        column(
          radioButtons(
            "interpretation", "Jouw interpretatie", inline = TRUE,
            selected = character(0),
            choices = c("toename", "afname", "stabiel", "onzeker", "geen idee")
          ),
          width = 10, offset = 1
        ),
        column(actionButton("next_exam", "volgende"), width = 10, offset = 1)
      ),
      fluidRow(
        column(
          plotOutput("plot_exam", height = "600px"), width = 10, offset = 1
        )
      )
    ),
    tabPanelBody(
      "thanks",
      h1("Hartelijk dank om deel te nemen aan dit onderzoek.")
    )
  )
)

# nolint start: cyclocomp_linter.
server <- function(input, output, session) {

  data <- reactiveValues(
    answer = numeric(0),
    dataset = NULL,
    design = NULL,
    level = 1,
    level_question = c(
"",
"Hoe zou jij de toestand van %i (ter hoogte van verticale stippellijn)
interpreteren?",
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
Welke figuur kan je het makkelijkst interpreteren?",
"Hoe zou jij de toestand van %i (ter hoogte van verticale stippellijn)
interpreteren?"
),
    preferred = list(),
    question = NULL
  )
  output$question_text <- renderText(
    sprintf(
      "%s<br>deelvraag %i van %i", data$level_question[data$level],
      data$question, nrow(data$design)
    )
  )

  observeEvent(data$level, {
    updateTabsetPanel(
      session = session, inputId = "hidden_tabs", selected = "comparison"
    )
    if (data$level == 1) {
      updateTabsetPanel(
        session = session, inputId = "hidden_tabs", selected = "introduction"
      )
      return(NULL)
    }
    if (data$level == 2) {
      updateTabsetPanel(
        session = session, inputId = "hidden_tabs", selected = "exam"
      )
      data$design <- generate_design(
        design_effect = list(y_label = c("index", "change")),
        design_data = c(
          tail(data$preferred, -1),
          list(
            size = c("stable", "strong", "potential"), reference = "none",
            ci = "none", effect = "none", threshold = log(c(0.9, 0.75))
          )
        ),
        max_question = max_interpretation
      )
      return(NULL)
    }
    if (data$level == 3) {
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
    if (data$level == 4) {
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
    if (data$level == 5) {
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
    if (data$level == 6) {
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
    if (data$level == 7) {
      updateTabsetPanel(
        session = session, inputId = "hidden_tabs", selected = "exam"
      )
      data$design <- generate_design(
        design_effect = vapply(
          head(data$preferred, 1), FUN.VALUE = vector(mode = "list", 1),
          FUN = function(x) {
            list(rep(x, 2))
          }
        ),
        design_data = c(
          tail(data$preferred, -1),
          list(
            size = c("stable", "strong", "potential"),
            threshold = log(c(0.9, 0.75))
          )
        ),
        max_question = max_interpretation
      )
      return(NULL)
    }
    updateTabsetPanel(
      session = session, inputId = "hidden_tabs", selected = "thanks"
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
    if (data$level %in% c(2, 7) && data$question <= max_interpretation) {
      attr(data$dataset, "selected_year") <- sample(data$dataset$x, 1)
      isolate({
        question_text <- sprintf(
          data$level_question[data$level], attr(data$dataset, "selected_year")
        )
      })
      output$question_exam <- renderText(
        sprintf(
          "%s<br>deelvraag %i van %i", question_text, data$question,
          nrow(data$design)
        )
      )
    }
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

  output$plot_exam <- renderPlot({
    if (is.null(data$dataset)) {
      return(NULL)
    }
    selected <- data$design_ab[, session$token] == "a"
    create_figure(
      dataset = data$dataset, reference = data$design_ab$reference[selected],
      y_label = data$design_ab$y_label[selected],
      ci = data$design_ab$ci[selected], effect = data$design_ab$effect[selected]
    ) +
      geom_vline(xintercept = attr(data$dataset, "selected_year"), linetype = 2)
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

  observeEvent(input$next_intro, {
    if (
      is.null(input$math) || is.null(input$stats) || is.null(input$colourblind)
    ) {
      output$intro_problem <- renderText(
        "<p  style='color:red;'>Graag alle vragen beantwoorden</p>"
      )
      return(NULL)
    }
    data$level <- data$level + 1
  })

  observeEvent(input$next_exam, {
    if (!data$level %in% c(2, 7)) {
      return(NULL)
    }
    if (is.null(input$interpretation)) {
      isolate({
        question_text <- sprintf(
          data$level_question[data$level], attr(data$dataset, "selected_year")
        )
      })
      output$question_exam <- renderText(
        sprintf(
          "%s<br>deelvraag %i van %i\n%s",
          question_text, data$question, nrow(data$design),
          "<p style='color:red;'>Gelieve de vraag eerst te beantwoorden.</p>"
        )
      )
      return(NULL)
    }
    updateRadioButtons(
      session = session, inputId = "interpretation", selected = character(0)
    )
    if (data$question == nrow(data$design)) {
      data$level <- data$level + 1
      data$question <- NULL
    } else {
      data$question <- data$question + 1
    }
  })
}
# nolint end

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
