if (!requireNamespace("asg4", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-apra0095")
}

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(gt)

# packaged data
germany <- asg4::germany_burden
eu      <- asg4::eu_eea_burden
meta    <- asg4::metadata_bhai

pretty_hai <- c(
  "HAP" = "Hospital-acquired pneumonia",
  "UTI" = "Urinary tract infection",
  "BSI" = "Primary bloodstream infection",
  "SSI" = "Surgical site infection",
  "CDI" = "Clostridioides difficile infection"
)

ui <- page_navbar(
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  title = "Germany HAI Burden (2011-2012)",
  theme = bs_theme(bootswatch = "flatly"),
  # load external css
  tags$head(htmltools::includeCSS("www/style.css")),

  nav_panel(
    "Overview",

    # KPI row
    div(
      class = "kpi-row nohover-kpi",
      layout_columns(
        breakpoint = "lg",
        col_widths = c(4, 4, 4),

        value_box(
          title    = textOutput("kpi_title_sum"),
          value    = textOutput("kpi_value_sum"),
          showcase = icon("list"),
          class    = "kpi kpi-slim"
        ),

        value_box(
          title    = "Difference vs other region",
          value    = textOutput("kpi_value_diff"),
          showcase = icon("arrows-alt-h"),
          class    = "kpi kpi-slim"
        ),

        # no trophy icon here
        value_box(
          title = "Top infection (share)",
          value = textOutput("kpi_value_top"),
          class = "kpi kpi-trophy"
        )
      )
    ),

    # comparison strip
    card(
      card_header("Selected infections: Germany vs EU/EEA (DALYs per 100k)"),
      plotOutput("p_compare", height = 340)
    ),

    # fixed sidebar
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        width = 300,

        h5("Country"),
        radioButtons("country", NULL, c("Germany", "EU/EEA"), inline = TRUE),

        h5("Infection types"),
        checkboxGroupInput(
          "types", NULL,
          choices  = names(pretty_hai),
          selected = names(pretty_hai)
        ),

        hr(),
        h5("Field definitions"),
        tags$ul(
          tags$li(tags$b("hai_type:"), " HAP, UTI, BSI, SSI, CDI"),
          tags$li(tags$b("cases:"), " estimated annual cases per 100k"),
          tags$li(tags$b("deaths:"), " attributable deaths per 100k"),
          tags$li(tags$b("dalys:"), " DALYs (absolute)"),
          tags$li(tags$b("dalys_per100k:"), " DALYs per 100k (for charts)")
        ),
        hr(),
        h5("How to read the outputs"),
        tags$ul(
          tags$li("Higher bars mean higher health loss."),
          tags$li("Checkboxes filter infection types."),
          tags$li("KPIs summarise the selected types only."),
          tags$li("Positive diff: chosen region > other region.")
        ),
        hr(),
        tags$em("Assumption: EU/EEA McCabe distribution is used for Germany (see vignette).")
      ),
      collapsible = FALSE,
      layout_columns(
        breakpoint = "lg",
        col_widths = c(8, 4),

        card(
          card_header(textOutput("single_title")),
          plotOutput("p_single", height = 360)
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
      p("Medians from Zacher et al. (2019) using ECDC PPS 2011-2012 and the BHAI approach."),
      tags$ul(
        tags$li("Source: Zacher et al., Eurosurveillance 2019 (DOI: 10.2807/1560-7917.ES.2019.24.46.1900135)"),
        tags$li("Method: BHAI package; PPS to annual incidence conversion per ECDC."),
        tags$li("Key assumption: EU/EEA McCabe distribution used for Germany.")
      ),
      h5("Metadata"),
      tableOutput("t_meta"),
      hr(),
      p("This app only uses data embedded in the package. No external files are read.")
    )
  )
)

server <- function(input, output, session) {


  updateCheckboxGroupInput(
    session, "types",
    choices  = names(pretty_hai),
    selected = names(pretty_hai)
  )

  current_data <- reactive({
    if (identical(input$country, "Germany")) germany else eu
  })
  other_data <- reactive({
    if (identical(input$country, "Germany")) eu else germany
  })

  sel_current <- reactive({
    req(input$types)
    current_data() %>% filter(hai_type %in% input$types)
  })
  sel_other <- reactive({
    req(input$types)
    other_data() %>% filter(hai_type %in% input$types)
  })

  # KPIs
  output$kpi_title_sum <- renderText({
    paste(input$country, "DALYs per 100k (sum)")
  })
  output$kpi_value_sum <- renderText({
    sprintf("%.1f", sum(sel_current()$dalys_per100k, na.rm = TRUE))
  })
  output$kpi_value_diff <- renderText({
    cur <- sum(sel_current()$dalys_per100k, na.rm = TRUE)
    oth <- sum(sel_other()$dalys_per100k,   na.rm = TRUE)
    sprintf("%+.1f", cur - oth)
  })
  output$kpi_value_top <- renderText({
    df <- sel_current()
    tot <- sum(df$dalys_per100k, na.rm = TRUE)
    if (!is.finite(tot) || tot <= 0 || nrow(df) == 0) return("No data")
    top <- df %>%
      mutate(share = dalys_per100k / tot) %>%
      arrange(desc(share)) %>%
      slice(1)
    nm <- pretty_hai[top$hai_type]
    if (is.na(nm)) nm <- top$hai_type
    paste0(nm, " (", sprintf("%.1f", top$share * 100), "%)")
  })

  # comparison chart
  output$p_compare <- renderPlot({
    req(input$types)
    g <- germany %>% filter(hai_type %in% input$types) %>% mutate(country = "Germany")
    e <- eu       %>% filter(hai_type %in% input$types) %>% mutate(country = "EU/EEA")
    comb <- bind_rows(g, e)
    ggplot(comb, aes(hai_type, dalys_per100k, fill = country)) +
      geom_col(position = "dodge") +
      labs(x = "Infection type", y = "DALYs per 100,000", fill = "Region") +
      scale_x_discrete(labels = pretty_hai) +
      theme_minimal(base_size = 12)
  })

  # single-region chart
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

  # table
  output$t_burden <- render_gt({
    df <- sel_current() %>%
      mutate(hai_type = pretty_hai[hai_type]) %>%
      select(hai_type, cases, deaths, dalys, dalys_per100k)

    gt(df) %>%
      cols_label(
        hai_type      = "Infection type",
        cases         = "Cases",
        deaths        = "Deaths",
        dalys         = "DALYs",
        dalys_per100k = "DALYs per 100k"
      ) %>%
      fmt_number(c(cases, deaths, dalys, dalys_per100k), decimals = 1) %>%
      sub_missing(everything(), missing_text = "-") %>%
      tab_footnote(footnote = "Values are per 100k population.")
  })

  output$t_meta <- renderTable({
    as.data.frame(meta, stringsAsFactors = FALSE)
  })
}

shinyApp(ui, server)


