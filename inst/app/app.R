library(shiny); library(bslib); library(ggplot2); library(dplyr); library(gt)

germany <- asg4::germany_burden
eu     <- asg4::eu_eea_burden
meta   <- asg4::metadata_bhai

ui <- page_navbar(
  title = "Germany HAI Burden (2011–2012)",
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(tags$link(rel="stylesheet", href="www/style.css")),
  nav_panel(
    "Overview",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Germany DALYs/100k (sum)"),
        card_body(
          h2(textOutput("kpi_de")),
          p("Sum across HAP, UTI, BSI, SSI, CDI")
        )
      ),
      card(
        card_header("EU/EEA DALYs/100k (sum)"),
        card_body(
          h2(textOutput("kpi_eu")),
          p("Sum across HAP, UTI, BSI, SSI, CDI")
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        4,
        radioButtons("country","Country", c("Germany","EU/EEA"), inline = TRUE),
        checkboxGroupInput(
          "types","Infection types",
          choices = unique(germany$hai_type),
          selected = unique(germany$hai_type)
        )
      ),
      column(
        8,
        plotOutput("p_burden"),
        gt_output("t_burden")
      )
    )
  ),
  nav_panel(
    "About",
    tags$p("Medians from Zacher et al. (2019) using ECDC PPS 2011–2012 and BHAI."),
    tags$p("Assumption: EU/EEA McCabe distribution applied to Germany (see vignette & metadata).")
  )
)

server <- function(input, output, session) {

  # KPI cards
  output$kpi_de <- renderText({
    round(sum(germany$dalys_per100k, na.rm = TRUE), 1)
  })
  output$kpi_eu <- renderText({
    round(sum(eu$dalys_per100k, na.rm = TRUE), 1)
  })

  # Reactive dataset based on country
  cur_data <- reactive({
    if (input$country == "Germany") germany else eu
  })

  output$p_burden <- renderPlot({
    df <- cur_data() |> filter(hai_type %in% input$types)
    ggplot(df, aes(hai_type, dalys_per100k)) +
      geom_col() +
      labs(
        x = "Infection type", y = "DALYs per 100,000",
        title = paste0(input$country, ": DALYs per 100,000")
      ) +
      theme_minimal(base_size = 13)
  })

  output$t_burden <- render_gt({
    cur_data() |> filter(hai_type %in% input$types) |> gt()
  })
}

shinyApp(ui, server)
