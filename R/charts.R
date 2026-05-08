# RF chart helpers — page 07 (Review & download).
#
# Builds the time-series and bar visualisations of state$rf_results$combined
# using ggplot2 + ggiraph, plus a custom HTML table renderer. Designed to
# pixel-match the editorial mockup: cream paper background, Forest & Ember
# palette, JetBrains Mono tick labels, no chart-junk, faded gridlines only.
#
# Public functions:
#   vp_chart_palette()         — named color vector for BASE + FIC1-6
#   theme_vp_chart()           — ggplot theme matching the design system
#   mgmt_short_label()         — vectorised MgmtID -> "FIC1 · <2'" etc.
#   render_rf_timeseries(df)   — ggplot/ggiraph time series
#   render_rf_bar(df, year)    — ggplot/ggiraph bar chart
#   render_rf_table_html(df)   — custom HTML table (negatives in red)


# Color map keyed by MgmtID. Diverging green → orange → red so low-intensity
# fire reads as benign and high-intensity reads as harmful, matching the
# ecological intuition for most habitat RFs. Pulls from the palette tokens
# in www/style.css where possible; the in-between peach / mid-orange shades
# are interpolated to fill the 6-step gradient cleanly.
vp_chart_palette <- function() {
  c(
    "BASE" = "#1F3225",   # --vp-forest-dark, dashed (faded baseline)
    "FIC1" = "#94B095",   # --vp-forest-light
    "FIC2" = "#E8B084",   # peach (interpolated)
    "FIC3" = "#E89556",   # warm orange (interpolated)
    "FIC4" = "#D26B30",   # mid orange (interpolated)
    "FIC5" = "#A8341E",   # --vp-danger
    "FIC6" = "#621810"    # darkened danger
  )
}


# Friendly short label for the table SCENARIO column.
# Pulls from mgmt_labels in R/ec_labels.R, then extracts the parenthetical
# flame-length range (e.g. "FIC1 · <2'"). Falls back to the raw mgmt_id if
# the label can't be parsed.
mgmt_short_label <- function(mgmt_id) {
  vapply(mgmt_id, function(id) {
    if (id == "BASE") return("Baseline (no fire)")
    row <- mgmt_labels[mgmt_labels$mgmt_id == id, ]
    if (nrow(row) == 0) return(id)
    # Extract content inside the first (...) group; "(<2 ft flame length)"
    m <- regmatches(row$label[1], regexpr("\\(([^)]+)\\)", row$label[1]))
    if (length(m) == 0) return(id)
    # Strip parens and " ft flame length", apostrophe for feet.
    inner <- gsub("^\\(|\\)$", "", m)
    inner <- gsub(" ft flame length", "'", inner)
    paste0(id, " · ", inner)
  }, character(1), USE.NAMES = FALSE)
}


# Custom ggplot theme. Cream background, mono tick labels, faded horizontal
# gridlines only, no axis lines, no panel border, legend at bottom in mono.
theme_vp_chart <- function(base_size = 11) {
  ggplot2::theme_minimal(base_family = "Poppins") +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "#FAF8F1", color = NA),
      panel.background   = ggplot2::element_rect(fill = "#FAF8F1", color = NA),
      panel.border       = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#E5DECD", linewidth = 0.3),
      axis.line          = ggplot2::element_blank(),
      axis.ticks         = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(family = "JetBrains Mono",
                                                 size = base_size,
                                                 color = "#8C9388"),
      axis.text.x        = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.text.y        = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.title.x       = ggplot2::element_text(family = "JetBrains Mono",
                                                 size = base_size - 1,
                                                 color = "#8C9388",
                                                 margin = ggplot2::margin(t = 14),
                                                 hjust = 0.5),
      axis.title.y       = ggplot2::element_text(family = "JetBrains Mono",
                                                 size = base_size - 1,
                                                 color = "#8C9388",
                                                 margin = ggplot2::margin(r = 14),
                                                 angle = 90,
                                                 hjust = 0.5),
      plot.title         = ggplot2::element_blank(),
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(family = "JetBrains Mono",
                                                 size  = 8.25,   # ~11px at 96dpi
                                                 color = "#525B4F",
                                                 face  = "bold"),
      legend.key         = ggplot2::element_blank(),
      legend.key.width   = ggplot2::unit(20, "pt"),
      legend.spacing.x   = ggplot2::unit(10, "pt"),
      legend.background  = ggplot2::element_rect(fill = "#FAF8F1", color = NA),
      legend.margin      = ggplot2::margin(t = 10),
      plot.margin        = ggplot2::margin(8, 16, 8, 8)
    )
}


# Time series: one line per MgmtID across rel.time. BASE is dashed; FIC1-6
# are solid lines with the diverging palette. Top-layer transparent rects
# per timestep deliver a plotly-style unified tooltip showing every series'
# value at that x-position (hover anywhere in a column triggers it).
render_rf_timeseries <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  pal      <- vp_chart_palette()
  mgmts    <- intersect(names(pal), unique(df$MgmtID))
  baseline <- "BASE" %in% mgmts
  fic_only <- setdiff(mgmts, "BASE")

  df <- df |>
    dplyr::filter(MgmtID %in% mgmts) |>
    dplyr::mutate(MgmtID = factor(MgmtID, levels = mgmts))

  x_breaks <- sort(unique(df$rel.time))

  # Half-step width between adjacent timesteps — used to size the hover rects
  # so the strips abut without gaps (matches plotly's continuous x-snap feel).
  step <- if (length(x_breaks) >= 2)
            min(diff(x_breaks)) / 2
          else 1

  # ymin/ymax for the hover rects. Pad to the visible range so rects span
  # the whole plotting area; the actual y scale auto-fits to the data.
  y_pad <- max(abs(df$median_combined_rf), na.rm = TRUE) * 1.1
  if (!is.finite(y_pad) || y_pad == 0) y_pad <- 1

  # Build per-timestep unified tooltip data. One row per rel.time; tooltip is
  # an HTML block listing every series at that x with a colored dot prefix.
  unified <- df |>
    dplyr::arrange(MgmtID, rel.time) |>
    dplyr::group_by(rel.time) |>
    dplyr::summarise(
      tooltip = paste0(
        "<div style='font-weight:600;margin-bottom:4px;'>t", rel.time[1], "</div>",
        paste0(
          sprintf(
            paste0("<div style='display:flex;align-items:center;gap:6px;'>",
                   "<span style='display:inline-block;width:8px;height:8px;",
                   "border-radius:1px;background:%s;'></span>",
                   "<span style='min-width:60px;'>%s</span>",
                   "<span style='margin-left:auto;font-weight:600;'>%s%.2f</span>",
                   "</div>"),
            pal[as.character(MgmtID)],
            ifelse(MgmtID == "BASE", "BASELINE", as.character(MgmtID)),
            ifelse(median_combined_rf > 0, "+", ""),
            median_combined_rf
          ),
          collapse = ""
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      xmin    = rel.time - step,
      xmax    = rel.time + step,
      ymin    = -y_pad,
      ymax    = y_pad,
      data_id = paste0("hover_", rel.time)
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = rel.time, y = median_combined_rf,
                                        color = MgmtID, group = MgmtID)) +
    # Reference line at y=0
    ggplot2::geom_hline(yintercept = 0, color = "#B8BDB3", linewidth = 0.4) +
    # Baseline rendered dashed (under the FIC lines)
    {if (baseline) ggplot2::geom_line(
      data = dplyr::filter(df, MgmtID == "BASE"),
      linetype = "dashed", linewidth = 0.9
    )} +
    # FIC lines
    ggplot2::geom_line(
      data = dplyr::filter(df, MgmtID %in% fic_only),
      linewidth = 1.4, lineend = "round"
    ) +
    # Top-layer hover rects — invisible, one per timestep. Catches the cursor
    # anywhere in the column and fires the unified HTML tooltip.
    ggiraph::geom_rect_interactive(
      data = unified,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   tooltip = tooltip, data_id = data_id),
      inherit.aes = FALSE,
      fill  = "white",
      alpha = 0.001     # near-zero so the rect registers hover events without showing
    ) +
    ggplot2::scale_color_manual(values = pal,
                                labels = function(x) toupper(ifelse(x == "BASE", "BASELINE", x))) +
    ggplot2::scale_x_continuous(breaks = x_breaks,
                                expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Relative year", y = "Weighted effect on HVRA") +
    theme_vp_chart() +
    ggplot2::guides(color = ggplot2::guide_legend(
      nrow = 1,
      override.aes = list(linewidth = 1.6)
    ))

  ggiraph::girafe(
    ggobj   = p,
    width_svg  = 9.5,
    height_svg = 4.6,
    options = list(
      ggiraph::opts_hover(css = ""),
      ggiraph::opts_tooltip(
        opacity = 0.96,
        offx    = 14,
        offy    = 14,
        css = paste(
          "background-color: #14180F;",
          "color: #FAF8F1;",
          "padding: 10px 12px;",
          "border-radius: 4px;",
          "font-family: 'JetBrains Mono', monospace;",
          "font-size: 11px;",
          "line-height: 1.55;",
          "min-width: 180px;",
          "box-shadow: 0 4px 12px rgba(20,24,15,0.25);"
        )
      ),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  )
}


# Bar: combined weighted RF at one rel.time year. Bars colored by MgmtID,
# value labels above (positive) or below (negative) each bar.
render_rf_bar <- function(df, year = 10) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  pal     <- vp_chart_palette()
  snap    <- df |>
    dplyr::filter(rel.time == year) |>
    dplyr::mutate(
      MgmtID    = factor(MgmtID, levels = intersect(names(pal), unique(MgmtID))),
      label_str = formatC(median_combined_rf, format = "f", digits = 2),
      vjust     = ifelse(median_combined_rf >= 0, -0.6, 1.6),
      tooltip   = sprintf("%s · t%d: %.2f", MgmtID, year, median_combined_rf),
      data_id   = MgmtID
    )

  if (nrow(snap) == 0) return(NULL)

  p <- ggplot2::ggplot(snap, ggplot2::aes(x = MgmtID, y = median_combined_rf,
                                          fill = MgmtID)) +
    ggplot2::geom_hline(yintercept = 0, color = "#B8BDB3", linewidth = 0.4) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tooltip, data_id = data_id),
      width = 0.6
    ) +
    ggplot2::geom_text(ggplot2::aes(label = label_str, vjust = vjust),
                       family = "JetBrains Mono", size = 3.4,
                       color = "#525B4F") +
    ggplot2::scale_fill_manual(values = pal,
                               labels = function(x) toupper(ifelse(x == "BASE", "BASELINE", x))) +
    ggplot2::scale_x_discrete(labels = function(x) ifelse(x == "BASE", "BASE", x)) +
    ggplot2::labs(x = NULL, y = "Weighted effect on HVRA") +
    theme_vp_chart() +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1,
      override.aes = list(linewidth = 0)
    ))

  ggiraph::girafe(
    ggobj      = p,
    width_svg  = 9.5,
    height_svg = 4.6,
    options    = list(
      ggiraph::opts_hover(css = "fill-opacity: 0.85;"),
      ggiraph::opts_tooltip(
        css = paste(
          "background-color: #14180F;",
          "color: #FAF8F1;",
          "padding: 6px 10px;",
          "border-radius: 3px;",
          "font-family: 'JetBrains Mono', monospace;",
          "font-size: 11px;"
        )
      ),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  )
}


# Custom HTML table for the RF values. Wide format: rows = scenarios,
# columns = T0..T30. Negatives rendered in danger color; baseline row in
# italic; mono throughout. Returns a tagList ready to drop into renderUI.
render_rf_table_html <- function(combined_df) {
  if (is.null(combined_df) || nrow(combined_df) == 0) {
    return(htmltools::tags$div(class = "rf-table-empty",
                               "No RF results yet."))
  }

  pal   <- vp_chart_palette()
  mgmts <- intersect(names(pal), unique(combined_df$MgmtID))
  years <- sort(unique(combined_df$rel.time))

  wide <- combined_df |>
    dplyr::filter(MgmtID %in% mgmts) |>
    tidyr::pivot_wider(names_from = rel.time, values_from = median_combined_rf) |>
    dplyr::mutate(MgmtID = factor(MgmtID, levels = mgmts)) |>
    dplyr::arrange(MgmtID)

  fmt_cell <- function(x) {
    if (is.na(x)) return(htmltools::tags$td(class = "rf-table-cell rf-table-na", "—"))
    cls <- if (x < 0) "rf-table-cell rf-table-neg"
           else if (x > 0) "rf-table-cell rf-table-pos"
           else "rf-table-cell rf-table-zero"
    htmltools::tags$td(class = cls, formatC(x, format = "f", digits = 2))
  }

  header <- htmltools::tags$thead(
    htmltools::tags$tr(
      htmltools::tags$th(class = "rf-table-th rf-table-th-scenario", "SCENARIO"),
      lapply(years, function(y) {
        htmltools::tags$th(class = "rf-table-th rf-table-th-num",
                           paste0("T", y))
      })
    )
  )

  rows <- lapply(seq_len(nrow(wide)), function(i) {
    row    <- wide[i, , drop = FALSE]
    mgmt   <- as.character(row$MgmtID)
    label  <- mgmt_short_label(mgmt)
    is_base <- mgmt == "BASE"
    row_cls <- paste("rf-table-row", if (is_base) "rf-table-row-baseline" else "")

    htmltools::tags$tr(class = row_cls,
      htmltools::tags$td(class = "rf-table-cell rf-table-cell-scenario", label),
      lapply(years, function(y) {
        col_name <- as.character(y)
        fmt_cell(row[[col_name]])
      })
    )
  })

  htmltools::tags$table(class = "rf-table",
    header,
    htmltools::tags$tbody(rows)
  )
}
