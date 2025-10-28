# ASCII ONLY

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(gt)

# packaged data (no read.csv):
germany <- asg4::germany_burden
eu      <- asg4::eu_eea_burden
meta    <- asg4::metadata_bhai

# helper: long names for clarity in labels and tables
pretty_hai <- c(
  "HAP" = "Hospital-acquired pneumonia",
  "UTI" = "Urinary tract infection",
  "BSI" = "Primary bloodstream infection",
  "SSI" = "Surgical site infection",
  "CDI" = "Clostridioides difficile infection"
)

ui <- page_navbar(
  title = "Germany HAI Burden (2011-2012)",
  theme = bs_theme(bootswatch = "flatly"),
  # custom css for subtle polish
  tags$head(tags$link(rel = "stylesheet", href = "www/style.css")),

  nav_panel(
    "Overview",

    # KPI cards (reactive)
    layout_columns(
      col_widths = c(4,4,4),
      value_box(
        title = textOutput("kpi_title_sum"),
        value = textOutput("kpi_value_sum"),
        showcase = icon("bar-chart-line"),
        theme_color = "primary"
      ),
      value_box(
        title = "Difference vs other region",
        value = textOutput("kpi_value_diff"),
        showcase = icon("arrows-alt-h"),
        theme_color = "warning"
      ),
      value_box(
        title = "Top infection (share)",
        value = textOutput("kpi_value_top"),
        showcase = icon("award"),
        theme_color = "success"
      )
    ),

    # comparison bar plot (reactive to selections)
    card(
      card_header("Selected infections: Germany vs EU-EEA (DALYs per 100k)"),
      plotOutput("p_compare", height = 260)
    ),

    layout_sidebar(
      sidebar = sidebar(
        h4("Controls"),
        radioButtons(
          "country", "Country",
          c("Germany", "EU/EEA"), inline = TRUE
        ),
        checkboxGroupInput(
          "types", "Infection types",
          choices  = names(pretty_hai),
          selected = names(pretty_hai)
        ),
        hr(),
        h5("Field definitions"),
        tags$ul(
          tags$li(tags$b("hai_type:"), " infection category (HAP, UTI, BSI, SSI, CDI)"),
          tags$li(tags$b("cases:"), " estimated annual cases per 100k population"),
          tags$li(tags$b("deaths:"), " attributable deaths per 100k"),
          tags$li(tags$b("dalys:"), " disability-adjusted life years per 100k (absolute)"),
          tags$li(tags$b("dalys_per100k:"), " DALYs per 100k (used for charts)")
        ),
        hr(),
        h5("How to read the outputs"),
        tags$ul(
          tags$li("Bar heights show burden per 100k. Higher bars mean higher health loss."),
          tags$li("Use the checkboxes to focus on specific infection types."),
          tags$li("The KPIs summarise the selected infections only."),
          tags$li("A positive diff means the chosen region is higher than the other region.")
        ),
        hr(),
        em("Assumption: EU-EEA McCabe severity distribution is used for Germany (see vignette).")
      ),

      # right content: single region bar chart + data table
      layout_columns(
        col_widths = c(7,5),
        card(
          card_header(textOutput("single_title")),
          plotOutput("p_single", height = 320)
        ),
        card(
          card_header("Data table"),
          gt_output("t_burden")
        )
      )
    )
  ),

  nav_panel(
    "About",
    card(
      h4("Study context and packaged data"),
      p("Medians from Zacher et al. (2019) using ECDC PPS 2011-2012 and the BHAI approach.
        We package the Germany and EU-EEA DALY burden for five HAIs."),
      tags$ul(
        tags$li("Source: Zacher B. et al., Eurosurveillance 2019 (DOI: 10.2807/1560-7917.ES.2019.24.46.1900135)"),
        tags$li("Method: BHAI R package; PPS to annual incidence conversion per ECDC."),
        tags$li("Important assumption: EU-EEA McCabe distribution used for Germany.")
      ),
      h5("Metadata"),
      tableOutput("t_meta"),
      hr(),
      p("This app only uses data embedded in the package. No external files are read.")
    )
  )
)

server <- function(input, output, session) {

  # which dataset is current vs "other"
  current_data <- reactive({
    if (identical(input$country, "Germany")) germany else eu
  })
  other_data <- reactive({
    if (identical(input$country, "Germany")) eu else germany
  })

  # selected subset for all downstream outputs
  sel_current <- reactive({
    req(input$types)
    current_data() |> filter(hai_type %in% input$types)
  })
  sel_other <- reactive({
    req(input$types)
    other_data() |> filter(hai_type %in% input$types)
  })

  # KPIs -------------------------------------------------------------

  output$kpi_title_sum <- renderText({
    paste(input$country, "DALYs per 100k (sum)")
  })

  output$kpi_value_sum <- renderText({
    sprintf("%.1f", sum(sel_current()$dalys_per100k, na.rm = TRUE))
  })

  output$kpi_value_diff <- renderText({
    cur <- sum(sel_current()$dalys_per100k, na.rm = TRUE)
    oth <- sum(sel_other()$dalys_per100k, na.rm = TRUE)
    sprintf("%+.1f", cur - oth)
  })

  output$kpi_value_top <- renderText({
    df <- sel_current() |>
      mutate(share = dalys_per100k / sum(dalys_per100k)) |>
      arrange(desc(share)) |>
      slice(1)
    nm <- pretty_hai[df$hai_type]
    if (is.na(nm)) nm <- df$hai_type
    paste0(nm, "  (", sprintf("%.1f", df$share * 100), "%)")
  })

  # Comparison plot (two regions, filtered infections)
  output$p_compare <- renderPlot({
    req(input$types)
    g <- germany |> filter(hai_type %in% input$types) |>
      mutate(country = "Germany")
    e <- eu |> filter(hai_type %in% input$types) |>
      mutate(country = "EU/EEA")
    comb <- bind_rows(g, e)
    ggplot(comb, aes(hai_type, dalys_per100k, fill = country)) +
      geom_col(position = "dodge") +
      labs(x = "Infection type", y = "DALYs per 100,000", fill = "Region") +
      scale_x_discrete(labels = pretty_hai) +
      theme_minimal(base_size = 12)
  })

  # Single-region plot
  output$single_title <- renderText({
    paste(input$country, ": DALYs per 100,000")
  })

  output$p_single <- renderPlot({
    ggplot(sel_current(), aes(hai_type, dalys_per100k)) +
      geom_col() +
      labs(x = "Infection type", y = "DALYs per 100,000") +
      scale_x_discrete(labels = pretty_hai) +
      theme_minimal(base_size = 12)
  })

  # Table (uses gt, formatted; no non-ASCII)
  output$t_burden <- render_gt({
    df <- sel_current() |>
      mutate(hai_type = pretty_hai[hai_type]) |>
      select(hai_type, cases, deaths, dalys, dalys_per100k)
    gt(df, rowname_col = NULL) |>
      cols_label(
        hai_type = "Infection type",
        cases = "Cases",
        deaths = "Deaths",
        dalys = "DALYs",
        dalys_per100k = "DALYs per 100k"
      ) |>
      fmt_number(everything(), decimals = 1) |>
      fmt_missing(everything(), missing_text = "-") |>
      tab_footnote(footnote = "Values are per 100k population.")
  })

  # metadata table in About
  output$t_meta <- renderTable({
    as.data.frame(meta, stringsAsFactors = FALSE)
  })
}

shinyApp(ui, server)
