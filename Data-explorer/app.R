#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

library(tidyverse)

df <- read_rds("df_init.rds")

theme_set(theme_light())

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty", font_scale = 1.3),

  # Application title
  titlePanel("Agriculture and Livestock Management Data Explorer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectizeInput("parish_in",
        "Select parish",
        choices = unique(df$parish_match),
        selected = "borgunda",
        multiple = T
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Data description",
          h4("Data from:"),
          h6("Household statistics - BISOS N. Agriculture and Livestock Management - Summary prepared by Statistics Sweden"),
          a(href = "https://www.scb.se/hitta-statistik/sok/Index?Subject=allsubjects&Series=BISOS+N+Jordbruk+-+Hush%C3%A5llningss%C3%A4llskapen+%C3%A5rsber%C3%A4ttelser+1865-1911&From=1910&To=1911&Sort=relevance&Query=&Page=1&Tab=older&Exact=false", "Link to SCB"),
          p("Example of table output:"),
          imageOutput("tab")
        ),
        tabPanel(
          "Farm ownership",
          plotlyOutput("distPlot"),
          plotlyOutput("demographics")
        ),
        tabPanel(
          "Crops",
          fluidRow(
            column(
              5,
              h4("Seeds"),
              plotlyOutput("seed_plot")
            ),
            column(
              5,
              offset = 1,
              h4("Harvest"),
              plotlyOutput("harvest_plot")
            )
          ),
          fluidRow(
            column(
              12,
              h4("Relationship between seed and harvest volume (same year)"),
              plotlyOutput("crop_relation")
            )
          )
        ),
        tabPanel(
          "Animals",
          h4("Animals"),
          plotlyOutput("animals_plot")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df_temp <- reactive({
    df %>%
      filter(
        parish_match == input$parish_in,
      )
  })

  output$distPlot <- renderPlotly({
    req(!is.na(input$parish_in)) # this fixes the messy red writing.

    p <- df_temp() %>%
      filter(group == "ownership") %>%
      mutate(
        type = case_when(
          str_detect(eng_trans_corrected, "owned") ~ "Owned farms",
          TRUE ~ "Used farms"
        ),
        eng_trans_corrected = str_squish(str_remove(eng_trans_corrected, ".*:")),
        eng_trans_corrected = fct_inorder(eng_trans_corrected)
      ) %>%
      ggplot(aes(year, value, fill = eng_trans_corrected)) +
      geom_col() +
      facet_wrap(~type, scales = "free_y", nrow = 2) +
      scale_fill_brewer(palette = "Paired") +
      labs(
        x = NULL,
        y = "Number of farms"
      )

    ggplotly(p)
  })

  output$demographics <- renderPlotly({
    p <- df_temp() %>%
      filter(group == "demographics") %>%
      ggplot(aes(year, value, colour = eng_trans_corrected)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::number_format()) +
      labs(
        x = "Year",
        y = NULL,
        colour = "Statistic"
      )

    ggplotly(p)
  })

  plot_crops <- function(product) {
    df_temp() %>%
      filter(group %in% product) %>%
      mutate(eng_trans_corrected = str_squish(str_remove(eng_trans_corrected, "Seed for|Harvest for"))) %>%
      ggplot(aes(year, value, colour = eng_trans_corrected)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::number_format()) +
      labs(colour = "Crop")
  }

  plot_animals <- function() {
    df_temp() %>%
      filter(group == "animals") %>%
      ggplot(aes(year, value, colour = eng_trans_corrected)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::number_format()) +
      labs(
        x = "Year",
        colour = "Animals",
        y = "Number of animals"
      )
  }


  output$seed_plot <- renderPlotly({
    p <- plot_crops(c("seed")) +
      labs(
        x = "Year",
        y = "Volume of seed in cubic feet"
      )

    plotly::ggplotly(p)
  })

  output$harvest_plot <- renderPlotly({
    p <- plot_crops(c("harvest")) +
      labs(
        x = "Year",
        y = "Volume of harvest in cubic feet"
      )

    plotly::ggplotly(p)
  })

  output$crop_relation <- renderPlotly({
    p <- df_temp() %>%
      filter(group %in% c("seed", "harvest")) %>%
      mutate(eng_trans_corrected = str_squish(str_remove(eng_trans_corrected, "Seed for|Harvest for"))) %>%
      select(value, year, eng_trans_corrected, group) %>%
      arrange(year, eng_trans_corrected) %>%
      filter(!is.na(value)) %>%
      pivot_wider(names_from = group, values_from = value) %>%
      filter(
        !is.na(seed),
        !is.na(harvest)
      ) %>%
      ggplot(aes(seed, harvest)) +
      geom_smooth(method = "lm") +
      geom_point(aes(colour = eng_trans_corrected)) +
      # facet_wrap(~eng_trans_corrected, scales = "free", nrow = 3) +
      scale_x_continuous(labels = scales::number_format()) +
      scale_y_continuous(labels = scales::number_format()) +
      theme(legend.position = "right") +
      labs(
        # title = "Relationship between volume of seed and harvest",
        x = "Volume of seed in cubic feet",
        y = "Volume of harvest in cubic feet",
        colour = "Crop"
      )

    ggplotly(p)
  })

  output$animals_plot <- renderPlotly({
    p <- plot_animals()

    plotly::ggplotly(p)
  })

  output$tab <- renderImage(
    {
      list(
        src = "1872_43.jpg",
        width = "100%",
        height = "800px"
      )
    },
    deleteFile = FALSE
  )
}

# Run the application
shinyApp(ui = ui, server = server)
