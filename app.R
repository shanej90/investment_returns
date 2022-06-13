#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#pakcages---------------------------------
library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)
library(plotly)
library(scales)
library(shiny)
library(tidyr)

###########################
#UI
############################

ui <- fluidPage(
  
  #app title-----------------------------
  headerPanel("Investment return calculator"),
  
  #sidebar---------------------------------
  sidebarPanel(
    
    #start year9
    numericInput(
      "start_yr",
      "Start year",
      value = year(Sys.Date()),
      min = year(Sys.Date()),
      max = year(Sys.Date()) + 10
    ),
    
    #end year
    numericInput(
      "end_yr",
      "End year",
      value = year(Sys.Date()) + 25,
      min = year(Sys.Date()) + 11,
      max = year(Sys.Date()) + 50
    ),
    
    #input - annual amount invested
    numericInput(
      "amount_invested",
      "Investment made each year-begin (GBP)",
      value = 1000,
      min = 0,
      max = 100000,
      step = 1
    ),
    
    #input - annual return
    numericInput(
      "annual_rtn",
      "Annual return (%)",
      value = 5,
      min = 0,
      max = 15,
      step = 0.5
    ),
    
    #input - inflation
    numericInput(
      "inflation",
      "Annual inflation (%)",
      value = 2,
      min = -1,
      max = 10,
      step = 0.5
    ),
    
    #download
    downloadButton("downloadData", "Download raw data")
    
  ),
  
  #main panel
  mainPanel(
    
    #plot
    fluidRow(
      plotlyOutput(outputId = "data_plot"))
  ),
  
  #table
  fluidRow(
    dataTableOutput(outputId = "tbl")
  )
  
)

##########################
#SERVER
##########################

server <- function(input, output) {
  
  #functions to prepare data------------------------------------------------------
  
  #list of years
  year_list <- c(year(Sys.Date()):(year(Sys.Date()) + 50))
  
  #list of used years
  used_years <- reactive({
    c(input$start_yr:input$end_yr)
  })
  
  #generate data----------------------------------------------------------------
  
  #generate vector of data for each year
  generate_data <- function(yr) {
    
    #revised yrs
    new_yrs <- c(yr:(max(used_years()) - 1))
    
    #vector
    vector <- rep(
      input$amount_invested,
      length(new_yrs)
      )
    
    #real growth
    rg <- input$annual_rtn - input$inflation
    
    #multiuply each vector value by growth rate and compound
    for (i in 1:length(vector)){
      
      vector[i] <- vector[i] * ((1 + (rg / 100))^i)
      
    }
    
    #data frame
    df <- data.frame("year" = new_yrs, "value" = vector)
    
    
  }
  
  #list of all returned data
  invested_df <- reactive({
    lapply(
      used_years()[1:(length(used_years()) - 1)],
      generate_data
      ) |>
      bind_rows() |>
      group_by(year) |>
      summarise(value = sum(value)) |>
      ungroup() 
  })
  
  #plot the data---------------------------------
  
  output$data_plot <- renderPlotly({
    
    validate(
      need(input$start_yr <= year(Sys.Date() + 10),
      paste0("Start year must be between ", year(Sys.Date()), " and ", year(Sys.Date()) + 10, ".")
      ),
      need(input$start_yr < input$end_yr, "Start year must be before end year."),
      need(
        input$end_yr <= (year(Sys.Date()) + 50),
        paste0("Start year must be no more than ", year(Sys.Date()) + 50, ".")
        )
    )
    
    ggplotly(
      ggplot(
        invested_df(),
        aes(
          x = year,
          y = value,
          group = 1
          )
        ) +
        #geoms
        geom_line(
          size = 1.5,
          aes(
            text = paste(
              "Year: ", year,
              "<br>Portfolio value GBP: ", comma(round(value, 0))
            )
          )
          ) +
        #scales
        scale_x_continuous(name = "Year") +
        scale_y_continuous(name  = "Value of investments at end-of-year, GBP", labels = comma) +
        #theme
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          axis.title = element_text(face = "bold")
        ),
      tooltip = "text"
    )
    
  })
  
  #table showing return over time
  output$tbl <- renderDataTable({
    
    #add detail
    df <- invested_df() |>
      transmute(
        year,
        total_invested = input$amount_invested * row_number(),
        value,
        pct_growth = 100 * (value - (input$amount_invested * row_number())) / (input$amount_invested * row_number())
        )
    
    #DT object
    df |>
      datatable(
        rownames = F,
        colnames = c("Year", "Total invested", "End-of-year value", "% growth on amount invested"),
        filter = "top",
        options = list(bPaginate = T, pageLength = 15)
      ) |>
      formatCurrency(c(2, 3), currency = "£", digits = 0) |>
      formatCurrency(4, currency = "", digits = 0)
    
  })
  
  #download---------------------------------
  
  #input
  datasetInput <- reactive({
    invested_df() |>
      transmute(
        year,
        total_invested = input$amount_invested * row_number(),
        value,
        pct_growth = 100 * (value - (input$amount_invested * row_number())) / (input$amount_invested * row_number())
      )
  })
  
  #download data output
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("investment_calculations.csv")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = F)
    }
  )
  
  
  
}


# Run the application ------------------------------------------------------------------
shinyApp(ui = ui, server = server)

