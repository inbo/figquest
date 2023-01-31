library(shiny)
library(figquest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(git2rdata)
library(INBOtheme)

max_question <- 10
max_interpretation <- 4

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$script('
        var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
    '),
    tags$link(rel = "stylesheet", type = "text/css", href = "inbo_report.css")
  ),
  tabsetPanel(
    id = "hidden_tabs", type = "hidden",
    tabPanelBody(
      "introduction",
      fluidRow(column(h1("Inleiding"), width = 10, offset = 1)),
      fluidRow(
        column(
  "Het INBO publiceert een aantal natuurindicatoren waarbij we allerhande trends
  grafisch weergeven.
  Deze trends worden uitgedrukt als een wijziging ten opzichte van een
  referentiejaar.
  In de voorbeelden die we geven is dit het jaar 2000.
  Met deze vragenlijst willen we onderzoeken wat voor figuur hiervoor het meest
  geschikt is.",
          class = "question", width = 10, offset = 1
        )
      ),
      fluidRow(
        column(
          sprintf(
"We starten met onderstaande vragen over u.
Daarmee willen we onderzoeken of er een verband is tussen uw kennis en uw
voorkeuren wat betreft figuren.
Vervolgens vragen we u om %i standaard figuren te interpreteren (multiple
choice).
Daarna geven we u in 4 stappen de mogelijkheid om de figuren te verbeteren.
Hiertoe krijgt u telkens 2 figuren naast elkaar te zien die op basis van een
stijlkenmerk van elkaar verschillen.
De gegevens in beide figuren zijn identiek.
Voor elk van deze stappen tonen we maximum %i paren van figuren.
Tenslotte leggen we u terug %i figuren ter interpretatie voor.
Ditmaal in de stijl waar uw voorkeur naar uitgaat.",
            max_interpretation, max_question, max_interpretation
          ),
          class = "question", width = 10, offset = 1
        )
      ),
      fluidRow(
        column(h2("Een beetje achtergrond over u"), width = 10, offset = 1)
      ),
      fluidRow(
        column(
          radioButtons(
            "math", "Hoe vertrouwd voelt u zicht met cijfers?", inline = TRUE,
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
            "stats", "Hoe vertrouwd voelt u zich met statistiek?",
            inline = TRUE, selected = character(0),
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
            "colourblind", "Bent u kleurenblind?", inline = TRUE,
            selected = character(0),
            choices = c("ja", "nee", "wil ik liever niet vertellen")
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
        column(
          h2("Makkelijkst te interpreteren figuur"), width = 10, offset = 1
        )
      ),
      fluidRow(
        column(
          htmlOutput("question_text", class = "question"), width = 10,
          offset = 1
        )
      ),
      fluidRow(
        column(
          radioButtons(
            "preference", "Uw voorkeur", inline = TRUE,
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
        column(
          h2("Interpreteren van een figuur"), width = 10, offset = 1
        )
      ),
      fluidRow(
        column(htmlOutput("question_exam"), width = 10, offset = 1)
      ),
      fluidRow(
        column(
          radioButtons(
            "interpretation", "Uw interpretatie", inline = TRUE,
            selected = character(0),
            choices = c(
              "toename t.o.v. 2000", "afname t.o.v. 2000",
              "stabiel t.o.v. 2000", "onzeker", "geen idee"
            )
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
      "intermediate",
      h2("We slaan de tussentijdse resultaten op.")
    ),
    tabPanelBody(
      "thanks",
      h1("Hartelijk dank om deel te nemen aan dit onderzoek."),
      "Voor vragen en opmerkingen kan u terecht bij thierry.onkelinx@inbo.be"
    )
  )
)

# nolint start: cyclocomp_linter.
server <- function(input, output, session) {

  root <- Sys.getenv("FIGQUEST_DATA", unset = ".")

  data <- reactiveValues(
    answer = numeric(0),
    dataset = NULL,
    design = NULL,
    fontsize = 20,
    interpretation = list(),
    level = 1,
    level_question = c(
"",
"Hoe zou u de toestand van %i (ter hoogte van verticale stippellijn)
interpreteren ten opzichte van het referentiejaar 2000?
<br><b>vraag 2 van 7</b>",
"Verkiest u labels op de y as als een index of als een relatief verschil?
Bij een index vermenigvuldigen we de cijfers met een vast getal zodat de waarde
in het referentiejaar gelijk is aan 100.
Bij een relatief verschil geven we de procentuele wijziging ten opzichte van
de waarde in het referentiejaar.
<br><b>vraag 3 van 7</b>",
"Beide figuren zijn een andere manier van weergave van dezelfde tijdreeks.
In dit geval verschillen ze in de manier waarop we referentielijnen weergeven.
We nemen uw voorkeur uit eerdere antwoorden mee.
Welke figuur kan u het makkelijkst interpreteren?
<br><b>vraag 4 van 7</b>",
"Beide figuren zijn een andere manier van weergave van de dezelfde tijdreeks.
In dit geval verschillen ze in de manier waarop we onzekerheid weergeven.
We nemen uw voorkeur uit eerdere antwoorden mee.
Welke figuur kan u het makkelijkst interpreteren?
<br><b>vraag 5 van 7</b>",
"Beide figuren zijn een andere manier van weergave van de dezelfde tijdreeks.
In dit geval verschillen ze in de manier waarop we de interpretatie van het
effect weergeven.
We nemen uw voorkeur uit eerdere antwoorden mee.
Welke figuur kan u het makkelijkst interpreteren?
<br><b>vraag 6 van 7</b>",
"Hoe zou u de toestand van %i (ter hoogte van verticale stippellijn)
interpreteren ten opzichte van het referentiejaar 2000?
<br><b>deel 7 van 7</b>"
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

  observeEvent(data$fontsize, {
    theme_set(theme_inbo(base_size = data$fontsize))
    update_geom_defaults(geom = "text", list(size = 3 * data$fontsize / 12))
    update_geom_defaults(geom = "line", list(size = 0.5 * data$fontsize / 12))
    update_geom_defaults(geom = "hline", list(size = 0.5 * data$fontsize / 12))
    update_geom_defaults(
      geom = "vline",
      list(size = 1 * data$fontsize / 12, colour = inbo_steun_donkerroos)
    )
  })

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
            ci = "none", effect = "none",
            threshold = round(log(c(0.9, 0.75)), 4)
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
          size = "strong", threshold = round(log(0.75), 4), reference = "none",
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
            threshold = round(log(c(0.9, 0.75)), 4), effect = "none"
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
            threshold = round(log(c(0.9, 0.75)), 4), effect = "none"
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
            size = c("moderate", "strong", "potential"),
            threshold = round(log(c(0.9, 0.75)), 4)
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
            threshold = round(log(c(0.9, 0.75)), 4)
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
      scale_points = data$fontsize / 12,
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
      scale_points = data$fontsize / 12,
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
      scale_points = data$fontsize / 12,
      highlight = attr(data$dataset, "selected_year"),
      ci = data$design_ab$ci[selected], effect = data$design_ab$effect[selected]
    )
  })

  observeEvent(data$fontsize, {
    if (data$level == 1 || is.null(data$dataset)) {
      return(NULL)
    }
    selected <- data$design_ab[, session$token] == "a"
    if (data$level %in% c(2, 7)) {
      output$plot_exam <- renderPlot({
        create_figure(
          dataset = data$dataset,
          reference = data$design_ab$reference[selected],
          y_label = data$design_ab$y_label[selected],
          scale_points = data$fontsize / 12,
          ci = data$design_ab$ci[selected],
          effect = data$design_ab$effect[selected]
        ) +
          geom_vline(
            xintercept = attr(data$dataset, "selected_year"), linetype = 2
          )
      })
      return(NULL)
    }
    output$plot_a <- renderPlot({
      create_figure(
        dataset = data$dataset, reference = data$design_ab$reference[selected],
        y_label = data$design_ab$y_label[selected],
        scale_points = data$fontsize / 12,
        ci = data$design_ab$ci[selected],
        effect = data$design_ab$effect[selected]
      ) +
        ggtitle("A")
    })
    selected <- data$design_ab[, session$token] == "b"
    output$plot_b <- renderPlot({
      create_figure(
        dataset = data$dataset, reference = data$design_ab$reference[selected],
        y_label = data$design_ab$y_label[selected],
        scale_points = data$fontsize / 12,
        ci = data$design_ab$ci[selected],
        effect = data$design_ab$effect[selected]
      ) +
        ggtitle("B")
    })
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
      updateTabsetPanel(
        session = session, inputId = "hidden_tabs", selected = "intermediate"
      )
      data$design |>
        mutate(
          answer = data$answer, session = session$token, timestamp = Sys.time(),
          width = input$dimension[1], height = input$dimension[2]
        ) |>
        select(-matches("_id$")) |>
        write_vc(
          sprintf("preference_%i_%s", data$level, session$token), root = root,
          sorting = c("session", "timestamp", "id")
        )
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
    updateTabsetPanel(
      session = session, inputId = "hidden_tabs", selected = "intermediate"
    )
    data.frame(
      session = session$token, timestamp = Sys.time(),
      math = factor(
        input$math,
        levels = c(
          "niet vertrouwd", "weinig vertrouwd", "vertrouwd", "zeer vertrouwd"
        ),
        labels = c("not", "low", "medium", "high")
      ),
      stats = factor(
        input$stats,
        levels = c(
          "niet vertrouwd", "weinig vertrouwd", "vertrouwd", "zeer vertrouwd"
        ),
        labels = c("not", "low", "medium", "high")
      ),
      colourblind = factor(
        input$colourblind,
        levels = c("ja", "nee", "wil ik liever niet vertellen"),
        labels = c("yes", "no", "no answer")
      ),
      width = input$dimension[1], height = input$dimension[2]
    ) |>
      write_vc(
        sprintf("intro_%s", session$token), sorting = c("session", "timestamp"),
        root = root
      )
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
    data.frame(
      session = session$token, timestamp = Sys.time(),
      threshold = attr(data$dataset, "threshold"),
      f_down = attr(data$dataset, "f_down") |>
        round(4),
      f_up = attr(data$dataset, "f_up") |>
        round(4),
      flip = attr(data$dataset, "flip") |>
        as.integer(),
      direction = attr(data$dataset, "direction") |>
        as.integer(),
      size = factor(
        data$design$size[data$question],
        levels = c("stable", "moderate", "strong", "potential")
      ),
      reference = factor(
        data$design$reference[data$question],
        levels = c("none", "lines", "lines + text")
      ),
      y_label = factor(
        data$design$y_label_a[data$question], levels = c("change", "index")
      ),
      ci = factor(
        data$design$ci[data$question], levels = c("none", "band", "gradient")
      ),
      effect = factor(
        data$design$effect[data$question],
        levels = c(
        "none", "symbol", "colour symbol", "colour ci", "symbol + colour ci"
      )),
      year = attr(data$dataset, "selected_year") |>
        as.integer(),
      correct = as.character(data$dataset$classification[
        data$dataset$x == attr(data$dataset, "selected_year")
      ]),
      answer = factor(
        input$interpretation,
        levels = c(
          "toename t.o.v. 2000", "afname t.o.v. 2000", "stabiel t.o.v. 2000",
          "onzeker", "geen idee"
        ),
        labels = c("+", "-", "~", "?", "x")
      )
    ) %>%
      list() %>%
      c(data$interpretation) -> data$interpretation
    updateRadioButtons(
      session = session, inputId = "interpretation", selected = character(0)
    )
    if (data$question == nrow(data$design)) {
      updateTabsetPanel(
        session = session, inputId = "hidden_tabs", selected = "intermediate"
      )
      bind_rows(data$interpretation) |>
        mutate(width = input$dimension[1], height = input$dimension[2]) |>
        write_vc(
          sprintf("exam_%s", session$token), root = root,
          sorting = c("session", "timestamp")
        )
      data$interpretation <- list()
      data$level <- data$level + 1
      data$question <- NULL
    } else {
      data$question <- data$question + 1
    }
  })

  observeEvent(input$font_small, {
    data$fontsize <- data$fontsize / 1.1
  })

  observeEvent(input$font_large, {
    data$fontsize <- data$fontsize * 1.1
  })
}
# nolint end

calc_preferred <- function(ds, answer) {
  ds %>%
    select(matches("_[ab]$")) %>%
    mutate(answer = answer) %>%
    pivot_longer(-"answer") %>%
    extract(.data$name, c("name", "plot"), "(.*)_([ab])$") %>%
    mutate(answer = ifelse(.data$plot == "a", -1, 1) * .data$answer) %>%
    group_by(.data$name, .data$value) %>%
    summarise(score = mean(.data$answer), .groups = "drop") %>%
    slice_max(.data$score, n = 1, with_ties = FALSE) -> pref
  setNames(pref$value, pref$name)
}

# Run the application
shinyApp(ui = ui, server = server)
