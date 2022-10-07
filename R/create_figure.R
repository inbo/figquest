#' Create a `ggplot2` figure
#' @param dataset the output of `generate_data()`.
#' @param reference Which reference to show.
#' Must be either `"lines + text"` (default), `"lines"` or `"none"`.
#' @param y_label Defines the style of the y axis labels
#' `"index"` generates labels as rescaled values where the reference is set to
#' 100.
#' `"change"` generates labels as the relative difference to the reference.
#' @param ci Defines the style of the confidence intervals.
#' `"none"` hides the confidence intervals.
#' `"band"` displays only the 90% confidence intervals.
#' `"gradient"` displays the 30%, 60% and 90% confidence intervals.
#' @param effect Defines the style of the classification.
#' `"none"` hides the classification.
#' `"symbol"` displays point with text symbols.
#' `"colour symbol"` displays point with text symbols with different colours.
#' `"colour ci"` colours the confidence intervals.
#' `"symbol + colour ci"` colours the confidence intervals and displays points
#' with text symbols`.
#' @param scale_points a scaling factor for point symbols.
#' Defaults to `1`.
#' @param highlight Which year to highlight with a vertical dashed line.
#' @export
#' @importFrom INBOtheme inbo_steun_blauw inbo_steun_donkerroos traffic_palette
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 aes annotate element_blank geom_hline geom_line
#' geom_point geom_ribbon geom_text geom_vline ggplot guides guide_legend
#' scale_colour_manual scale_fill_manual scale_x_continuous scale_y_log10 theme
#' unit
#' @importFrom rlang .data
#' @importFrom stats setNames
create_figure <- function(
  dataset = generate_data(size = "strong", threshold = log(0.75)),
  reference = c("none", "lines", "lines + text"),
  y_label = c("index", "change"), ci = c("none", "band", "gradient"),
  effect = c(
    "none", "symbol", "colour symbol", "colour ci", "symbol + colour ci"
  ),
  scale_points = 1, highlight = NULL
) {
  reference <- match.arg(reference)
  y_label <- match.arg(y_label)
  ci <- match.arg(ci)
  effect <- match.arg(effect)
  interpretation_code <- c(
    "++" = "sterke toename", "+" = "toename", "+~" = "matige toename",
    "~" = "stabiel", "-~" = "matige daling", "-" = "daling",
    "--" = "sterke daling", "?+" = "mogelijke toename",
    "?-" = "mogelijke daling", "?" = "onduidelijke trend"
  )
  c(rev(traffic_palette(7)), "grey65", "grey35", "grey50") %>%
    setNames(
      c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?")
    ) -> interpretation_gradient
  if (grepl("colour", effect)) {
    c(rev(traffic_palette(7)), "grey65", "grey35", "grey50") %>%
      setNames(
        c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?")
      ) -> interpretation_gradient
    interpretation_gradient[4] <- inbo_steun_blauw
  } else {
    rep(inbo_steun_blauw, 10) %>%
      setNames(
        c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?")
      ) -> interpretation_gradient
  }
  names(interpretation_gradient) <- interpretation_code

  dataset <- dataset %>%
    mutate(
      effect = factor(
        .data$classification, levels = names(interpretation_code),
        labels = interpretation_code
      )
    )
  p <- ggplot(dataset, aes(x = .data$x, y = .data$mu)) +
    scale_x_continuous(limits = c(2000, NA)) +
    theme(
      axis.title.x = element_blank()
    )
  if (reference != "none") {
    p <- p +
      geom_hline(yintercept = 1, linetype = 2, colour = inbo_steun_donkerroos) +
      geom_hline(
        yintercept = exp(c(-1, 1) * attr(dataset, "threshold")), linetype = 3,
        colour = inbo_steun_donkerroos
      )
    if (grepl("text", reference)) {
      p <- p +
        annotate(
          geom = "text", label = "referentie", x = max(dataset$x) + 2, y = 1,
          vjust = -0.2, hjust = 1, colour = inbo_steun_donkerroos
        ) +
        annotate(
          geom = "text", label = "belangrijke toename", x = max(dataset$x) + 2,
          y = exp(abs(attr(dataset, "threshold"))), vjust = -0.2, hjust = 1,
          colour = inbo_steun_donkerroos
        ) +
        annotate(
          geom = "text", label = "belangrijke afname", x = max(dataset$x) + 2,
          y = exp(-abs(attr(dataset, "threshold"))), vjust = -0.2, hjust = 1,
          colour = inbo_steun_donkerroos
        )
    }
  }
  if (y_label == "index") {
    p <- p +
      scale_y_log10(
        "index (2000 = 100)", labels = index_label, breaks = index_major_breaks,
        minor_breaks = index_minor_breaks
      )
  } else {
    p <- p +
      scale_y_log10(
        "wijziging t.o.v. 2000", labels = change_label,
        breaks = index_major_breaks, minor_breaks = index_minor_breaks
      )
  }
  if (ci %in% c("band", "gradient")) {
    if (grepl("colour ci", effect)) {
      ci_data <- generate_ci_data(dataset)
      p <- p +
        geom_ribbon(
          data = ci_data, alpha = 0.3,
          aes(
            ymin = .data$lcl_90, ymax = .data$ucl_90, fill = .data$effect,
            group = .data$set
          )
        ) +
        scale_fill_manual(
          "wijziging", values = interpretation_gradient, drop = FALSE,
          labels = wrap_long
        ) +
        guides(
          fill = guide_legend(
            override.aes = list(label = names(interpretation_code)), alpha = 1
          )
        ) +
        theme(legend.key.size = unit(24, units = "points"))
      if (ci == "gradient") {
        p <- p +
          geom_ribbon(
            data = ci_data, alpha = 0.3,
            aes(
              ymin = .data$lcl_60, ymax = .data$ucl_60, fill = .data$effect,
              group = .data$set
            )
          ) +
          geom_ribbon(
            data = ci_data, alpha = 0.3,
            aes(
              ymin = .data$lcl_30, ymax = .data$ucl_30, fill = .data$effect,
              group = .data$set
            )
          )
      }
    } else {
      p <- p +
        geom_ribbon(
          aes(ymin = .data$lcl_90, ymax = .data$ucl_90), alpha = 0.3
        )
      if (ci == "gradient") {
        p <- p +
          geom_ribbon(
            aes(ymin = .data$lcl_60, ymax = .data$ucl_60), alpha = 0.3
          ) +
          geom_ribbon(
            aes(ymin = .data$lcl_30, ymax = .data$ucl_30), alpha = 0.3
          )
      }
    }
  }
  p <- p + geom_line()
  if (!is.null(highlight)) {
    p <- p +
      geom_vline(xintercept = highlight, linetype = 2)
  }
  if (grepl("symbol", effect)) {
    if (grepl("colour ci", effect)) {
      p <- p +
        geom_point(size = 6 * scale_points, show.legend = TRUE)
    } else {
      p <- p +
        geom_point(aes(colour = effect), size = 6 * scale_points) +
        geom_text(
          aes(label = classification), colour = "white",
          size = 3 * scale_points, show.legend = TRUE
        ) +
        scale_colour_manual(
          values = interpretation_gradient, drop = FALSE,
          labels = wrap_long
        ) +
        guides(
          colour = guide_legend(
            override.aes = list(label = names(interpretation_code))
          )
        )
    }
    p <- p +
      geom_text(
        aes(label = classification), colour = "white", size = 3 * scale_points,
        show.legend = TRUE
      )
  }
  p
}

#' @importFrom dplyr %>% bind_rows distinct group_by inner_join lag mutate
#' select slice_max slice_min transmute ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils head
#' @importFrom rlang .data
generate_ci_data <- function(dataset) {
  dataset %>%
    mutate(
      set = lag(.data$classification, default = head(.data$classification, 1)),
      set = .data$set != .data$classification,
      set = cumsum(.data$set)
    ) -> sets
  sets %>%
    select(-.data$classification, -.data$effect) %>%
    group_by(.data$set) %>%
    slice_min(.data$x, n = 1) %>%
    ungroup() %>%
    mutate(set = .data$set - 1) %>%
    pivot_longer(-.data$set) -> next_set
  sets %>%
    select(-.data$classification, -.data$effect) %>%
    group_by(.data$set) %>%
    slice_max(.data$x) %>%
    ungroup() %>%
    pivot_longer(-.data$set) %>%
    inner_join(next_set, by = c("set", "name")) %>%
    transmute(
      .data$set, .data$name, value = (.data$value.x + .data$value.y) / 2
    ) %>%
    pivot_wider(names_from = .data$name, values_from = .data$value) -> mid
  sets %>%
    distinct(.data$set, .data$classification, .data$effect) %>%
    inner_join(mid, by = "set") %>%
    bind_rows(
      sets,
      sets %>%
        distinct(.data$set, .data$classification, .data$effect) %>%
        inner_join(
          mid %>%
            mutate(set = .data$set + 1),
          by = "set"
        )
    )
}

index_label <- function(x) {
  x * 100
}

change_label <- function(x) {
  precision <- max(ceiling(-log10(diff(range(x, na.rm = TRUE))) - 2), 0)
  sprintf(paste0("%+.", precision, "f%%"), (x - 1) * 100)
}

index_major_breaks <- function(x) {
  pretty(x, n = 5)
}

index_minor_breaks <- function(x) {
  pretty(x, n = 10)
}

#' @importFrom stringr str_wrap
wrap_long <- function(x) {
  str_wrap(x, width = 12)
}
