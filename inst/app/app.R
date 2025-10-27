# inst/app/app.R
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(gt)

# packaged data
germany <- asg4::germany_burden
eu      <- asg4::eu_eea_burden
meta    <- asg4::metadata_bhai

ui <- page_navbar(
  title = "Germany HAI Burden (2011–2012)",
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(tags$link(rel = "stylesheet", href = "www/style.css")),

  nav_panel(
    "Overview",

    # ---------- KPI row (value boxes are responsive + no scrollbars)
    layout_column_wrap(
      width = 1/3,
      value_box(title = textOutput("kpi_title_sum"), value = textOutput("kpi_sum"),
                p("Sum of DALYs per 100k (selected infections)")),
      value_box(title = "Difference vs other region", value = textOutput("kpi_diff"),
                p("Current − Other (same infections)")),
      value_box(title = "Top infection (share)", value = textOutput("kpi_top_name"),
                p(textOutput("kpi_top_share")))
    ),

    card(
      card_header("Selected infections: Germany vs EU/EEA (DALYs per 100k)"),
      card_body(plotOutput("compare_plot", height = "240px"))
    ),

    hr(),

    fluidRow(
      column(
        4,
        radioButtons("country", "Country", c("Germany", "EU/EEA"), inline = TRUE),
        checkboxGroupInput(
          "types", "Infection types",
          choices  = unique(germany$hai_type),
          selected = unique(germany$hai_type)
        ),
        helpText("Assumption: EU/EEA McCabe distribution is used for Germany (see vignette).")
      ),
      column(
        8,
        card(card_body(plotOutput("p_burden", height = "260px"))),
        gt_output("t_burden")
      )
    )
  ),

  nav_panel(
    "About",
    tags$p("Data: Zacher et al. (2019), Euro Surveill 24(46):1900135."),
    tags$p("DALYs via BHAI using ECDC PPS 2011–2012; packaged data only.")
  )
)

server <- function(input, output, session) {

  # ---------- Reactives
  sel_types <- reactive(input$types)

  cur_df <- reactive({
    if (input$country == "Germany") germany else eu
  })
  other_df <- reactive({
    if (input$country == "Germany") eu else germany
  })

  cur_sum <- reactive({
    cur_df() |>
      filter(hai_type %in% sel_types()) |>
      summarise(s = sum(dalys_per100k, na.rm = TRUE)) |>
      pull(s)
  })
  other_sum <- reactive({
    other_df() |>
      filter(hai_type %in% sel_types()) |>
      summarise(s = sum(dalys_per100k, na.rm = TRUE)) |>
      pull(s)
  })
  top_row <- reactive({
    cur_df() |>
      filter(hai_type %in% sel_types()) |>
      arrange(desc(dalys_per100k)) |>
      slice(1)
  })

  # ---------- KPI outputs
  output$kpi_title_sum <- renderText({
    paste0(input$country, " DALYs/100k (sum)")
  })
  output$kpi_sum  <- renderText({ round(cur_sum(), 1) })
  output$kpi_diff <- renderText({ sprintf("%+.1f", cur_sum() - other_sum()) })
  output$kpi_top_name <- renderText({
    if (nrow(top_row()) == 0) "—" else top_row()$hai_type
  })
  output$kpi_top_share <- renderText({
    if (nrow(top_row()) == 0) return("")
    share <- top_row()$dalys_per100k / max(cur_sum(), 1e-9)
    paste0(round(share * 100, 1), "% of selected total")
  })

  # ---------- Mini comparison plot
  output$compare_plot <- renderPlot({
    req(sel_types())
    g <- germany |> filter(hai_type %in% sel_types()) |> mutate(country = "Germany")
    e <- eu      |> filter(hai_type %in% sel_types()) |> mutate(country = "EU/EEA")
    combined <- bind_rows(g, e); req(nrow(combined) > 0)

    ggplot(combined, aes(hai_type, dalys_per100k, fill = country)) +
      geom_col(position = "dodge") +
      labs(x = NULL, y = "DALYs per 100k") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", plot.margin = margin(6, 6, 6, 6))
  })

  # ---------- Main plot
  output$p_burden <- renderPlot({
    req(sel_types())
    df <- cur_df() |> filter(hai_type %in% sel_types()); req(nrow(df) > 0)

    ggplot(df, aes(hai_type, dalys_per100k)) +
      geom_col() +
      labs(
        x = "Infection type", y = "DALYs per 100,000",
        title = paste0(input$country, ": DALYs per 100,000")
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(6, 6, 6, 6))
  })

  # ---------- Table with fixed schema
  table_df <- reactive({
    df <- cur_df() |> filter(hai_type %in% sel_types())
    needed <- c("hai_type", "cases", "deaths", "dalys", "dalys_per100k")
    for (nm in needed) if (!nm %in% names(df)) df[[nm]] <- NA_real_
    df[, needed, drop = FALSE]
  })

  output$t_burden <- render_gt({
    df <- table_df()
    note <- if (input$country == "EU/EEA")
      "EU/EEA values are per 100,000 population (Zacher et al., 2019)."
    else
      "Germany values are medians from the representative PPS sample."

    gt(df) |>
      cols_label(
        hai_type      = "Infection type",
        cases         = "Cases",
        deaths        = "Deaths",
        dalys         = "DALYs",
        dalys_per100k = "DALYs per 100k"
      ) |>
      fmt_number(columns = c(cases, deaths, dalys, dalys_per100k), decimals = 1) |>
      fmt_missing(everything(), missing_text = "—") |>
      tab_source_note(note)
  })
}

shinyApp(ui, server)
