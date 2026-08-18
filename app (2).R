library(shiny)
library(DT)
library(plotly)
library(tidyverse)

# ============================================================
# Load data
# ============================================================
download_private_rds <- function(repo_id, filepath) {
  url     <- paste0("https://huggingface.co/datasets/", repo_id, "/resolve/main/", filepath)
  api_key <- Sys.getenv("AnonData")
  if (api_key == "") stop("API key is not set.")

  response <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    timeout(60)
  )

  if (httr::status_code(response) == 200) {
    tmp <- tempfile(fileext = ".rds")
    writeBin(httr::content(response, "raw"), tmp)
    readRDS(tmp)
  } else {
    stop(paste("Failed to download:", filepath, "| Status:", httr::status_code(response)))
  }
}
pitcher_pitch_summary <- download_private_rds("adamfontana/AnonData", "shiny_app_data.rds")

player_map <- pitcher_pitch_summary %>%
  distinct(Pitcher) %>%
  arrange(Pitcher) %>%
  mutate(AnonymousPitcher = paste0("Player ", row_number()))

pitcher_pitch_summary <- pitcher_pitch_summary %>%
  left_join(player_map, by = "Pitcher") %>%
  mutate(Pitcher = AnonymousPitcher) %>%
  select(-AnonymousPitcher)
raw_feature_cols <- c("RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate",
                      "RelHeight", "RelSide", "Extension",
                      "VertApprAngle", "HorzApprAngle",
                      "SpinEff_proxy", "SpinAxis", "ZoneTime")

feature_labels <- c(
  RelSpeed = "Velocity",
  InducedVertBreak = "Vert Break (IVB)",
  HorzBreak = "Horz Break",
  SpinRate = "Spin Rate",
  RelHeight = "Release Height",
  RelSide = "Release Side",
  Extension = "Extension",
  VertApprAngle = "Vert Approach Angle",
  HorzApprAngle = "Horz Approach Angle",
  SpinEff_proxy = "Spin Efficiency",
  SpinAxis = "Spin Axis",
  ZoneTime = "Zone Time"
)

pctile_cols <- paste0(raw_feature_cols, "_pctile")

# Filter to Bristol Blues 2026 for display, while percentiles above were
# already computed against the full league before this filter was applied
blues_2026 <- pitcher_pitch_summary %>%
  filter(PitcherTeam == "BRI_B", Year == 2026)

pitchers <- blues_2026 %>% pull(Pitcher) %>% unique() %>% sort()

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel("Bristol Blues Stuff+ Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pitcher", "Pitcher", choices = pitchers),
      hr(),
      helpText("Click a row in the table to see the pitch's",
               "percentile profile in the radar chart."),
      helpText("Percentiles are calculated relative to same-handed",
               "pitchers throwing the same pitch type across the full league",
               "(min. 10 pitches).")
    ),
    
    mainPanel(
      h4("Pitch Arsenal"),
      DTOutput("pitch_table"),
      br(),
      plotlyOutput("radar_chart", height = "550px")
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  pitcher_data <- reactive({
    req(input$pitcher)
    blues_2026 %>%
      filter(Pitcher == input$pitcher) %>%
      arrange(desc(n))
  })
  
  output$pitch_table <- renderDT({
    pitcher_data() %>%
      transmute(
        `Pitch Type`,
        `# Pitches` = n,
        `Stuff+` = round(StuffPlus, 1)
      ) %>%
      datatable(selection = "single", rownames = FALSE,
                options = list(dom = "t", paging = FALSE)) %>%
      formatStyle("Stuff+", fontWeight = "bold")
  })
  
  observeEvent(pitcher_data(), {
    dataTableProxy("pitch_table") %>% selectRows(1)
  })
  
  output$radar_chart <- renderPlotly({
    req(input$pitch_table_rows_selected)
    
    row <- pitcher_data()[input$pitch_table_rows_selected, ]
    
    pctile_vals <- as.numeric(row[pctile_cols])
    labels <- feature_labels[raw_feature_cols]
    
    if (all(is.na(pctile_vals))) {
      return(
        plotly_empty(type = "scatter", mode = "markers") %>%
          layout(title = "Not enough league data for this pitch type (n < 10) to compute percentiles")
      )
    }
    
    plot_ly(
      type = "scatterpolar",
      r = c(pctile_vals, pctile_vals[1]),
      theta = c(labels, labels[1]),
      fill = "toself",
      name = paste(row$Pitcher, "-", row$`Pitch Type`)
    ) %>%
      add_trace(
        r = rep(50, length(labels) + 1),
        theta = c(labels, labels[1]),
        fill = "none",
        mode = "lines",
        line = list(dash = "dot", color = "gray"),
        name = "League Average"
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 100)),
          domain = list(x = c(0.1, 0.9), y = c(0, 0.85))
        ),
        title = list(
          text = paste0(row$Pitcher, " — ", row$`Pitch Type`,
                        " (Stuff+: ", round(row$StuffPlus, 1), ")"),
          y = 0.98
        ),
        margin = list(t = 80, b = 40)
      )
  })
}

shinyApp(ui, server)
