install.packages("ggplot2")
install.packages("plotly")
install.packages("dbplyr")
install.packages("ggmap")
install.packages("maps")
install.packages("viridis")
install.packages("showtextdb")

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(ggplot2)
library(glue)
library(countrycode)
source('klinh.r')
source('Ngocmai.R')
source('232.R')
source('tghd.r')
library(terra)
library(raster)



pacman::p_load(tidyverse, rvest, xml2,
               janitor, lubridate, ggtext, ggrepel, extrafont, scales, ggalt, zoo,
               countrycode, gghighlight, ggimage, glue, grid, png, leaflet)

pacman::p_loaded()

ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    "OECD app",
    tabPanel("GDP",
             sidebarPanel(
               sliderInput("year_map", "Select year:", min = min(gdp$year), max = max(gdp$year), value = min(gdp$year), step = 1),
               selectInput("country_gpd", "Select countries:", choices = unique(gdp$country), multiple = TRUE),
               sliderInput("start_year", "Start year:", min = min(gdp$year), max = max(gdp$year), value = min(gdp$year), step = 1),
               sliderInput("end_year", "End year:", min = min(gdp$year), max = max(gdp$year), value = max(gdp$year), step = 1)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("GDP",
                          plotOutput(outputId = "gdp_treemap1"),
                 ),
                 tabPanel("ShareGDP",
                          plotlyOutput(outputId = "gdpShare")
                 ),
                 tabPanel("Population",
                          plotlyOutput(outputId = "world_map")
    
                 ),
                 tabPanel("GDP Growth ",
                          plotlyOutput(outputId = "gdp_growth_chart")
                 )
               )
             )
    ),
    tabPanel("Employee rate",
             sidebarPanel(
               selectizeInput("country_emp", "Select countries:", choices = unique(gradpc$country), multiple = TRUE),
          
               sliderInput("year_emp", "Select year:", min = min(gradpc$year), max = max(gradpc$year), value = min(gradpc$year), step = 1),
            
          
               sliderInput("year_bar", "Select Year for Bar Plot", min = 2010, max = max(gradpc$year), value = 2023),
               sliderInput("year_scatter", "Select Year for Scatter Plot", min = 2010, max = max(gradpc$year), value = 2010),
              
               numericInput("startYearInput", "Start Year", value = 2010),
               numericInput("endYearInput", "End Year", value = 2020),
               radioButtons("sexInput", "Sex", choices = c("Male", "Female", "Total"), selected = "Total")
             ),
             mainPanel(
               
               h1('EMPLOYMENT RATE'),
               
               tabsetPanel(
                 tabPanel("Emp1",
                          plotlyOutput(outputId = "rate_emp"),
                 ),
                 tabPanel("Emp2",
                          plotlyOutput(outputId = "employmentPlot")
                 ),
                 tabPanel("Emp3",
                          plotlyOutput(outputId = "emp_country")
                 )
               )
             )
    ),
    tabPanel("Better life index",
             sidebarPanel(
               selectInput(
                 inputId = "chartTypes1",
                 label = "Explore:",
                 choices = c("Housing", "Wealth", "Jobs", "Education", "Environment", 
                             "Engagement", "Health", "Safety", "Work.life.balance", "Community", "Life.satisfaction"),
                 selected = "Housing"
               )
             ),
             mainPanel(
               
               h1('BETTER LIFE INDEX'),
               
               tabsetPanel(
                 tabPanel("Factors",
                          plotlyOutput("better_index")
                 ),
                 
                 tabPanel("Table",
                          DT::dataTableOutput("table_better")
                 )
               )
             )
    ),
    tabPanel("MEI", sidebarLayout(
      sidebarPanel(
        selectInput("country3", "Select countries:", choices = unique(df_long$Country)),
        dateRangeInput("dateRange", "Select times:", start = min(df_long$Month), end = max(df_long$Month)),
        actionButton("submit", "Update")
      ),
      mainPanel(

        tabsetPanel(
          id = "tabPanel",
          tabPanel("Dữ liệu", DTOutput("dataTable")),
          tabPanel("Đồ thị",
                   tabsetPanel(
                     id = "graphTabPanel",
                     tabPanel("Dygraph", dygraphOutput("dygraphPlot")),
                     tabPanel("Tỉ giá hối đoái Trung Bình", plotlyOutput("summaryPlot1")),
                     tabPanel("Biến Động Trung Bình theo Tháng", plotlyOutput("summaryPlot2")),
                     tabPanel("Tỉ Giá Hối Đoái South Africa", plotlyOutput("individualPlot")),
                     tabPanel("Xếp Hạng", plotlyOutput("rankingPlot")),
                     tabPanel("Biểu Đồ Tỷ Giá Hối Đoái", plotOutput("ratePlot"))
                   )
          )
        )
      )
    )
    ),
    tabPanel("ICT",
             titlePanel("Phân tích ICT theo từng quốc gia"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("nation", "Select countries:", choices = unique(df$Country)),
                 actionButton("update", "Update")
               ),
               mainPanel(
                 plotOutput("avg_value"),
                 plotOutput("data_chart"),
                 plotOutput("pieChart"),
                 plotOutput("histogram"),
                 plotlyOutput("business_chart"),
                 DT::dataTableOutput("data_table")
               )
             )
    ),
    tabPanel("Data",
             sidebarPanel(
               tags$h3("Input:"),
               selectInput("download", "Choose a dataset:",
                           choices = c("Employment Rate", "Population",
                                       "Better Index","ICT","MEI","GDP","Working-Age-Polution(>15)",
                                       "Growth GPD"
                                       )),
               downloadButton("downloadData", "Download")
             ),
             mainPanel(
               tableOutput("table")
             )
    ),
    tabPanel("OECD Data",
             sidebarPanel(
               textInput("dataset", "Dataset code:", ""),
               textInput("filter", "Filter:", ""),
               dateInput("start_time", "Start time:", ""),
               dateInput("end_time", "End time:", ""),
               numericInput("last_n_observations", "Last n observations:", value = NULL),
               actionButton("download_btn", "Download Data")
             ),
             mainPanel(
               dataTableOutput("table_oced")
             )
    )
  )
)


# Define server function  
server <- function(input, output) {
  
  # Tải dữ liệu từ file RData
  load("data.RData")
  load("data2.RData")

  
  # Create GDP treemap plot
  output$gdp_treemap1 <- renderPlot({
    create_gdp_treemap_1(gdpData, "Open Sans")})
  
  # Create GDP share plot
  output$gdpShare <- renderPlotly({
    year <- input$year_map
    plot_gdp_share(gdp, year)})
  
  # Create employment rate plot
  output$employmentPlot <- renderPlotly({
    countries <- input$country_emp
    create_employment_rate_plot(input$year_bar, input$year_scatter,countries)})
  
  # Create GDP treemap plot
  output$world_map <- renderPlotly({
    years <- input$year_map
    create_world_map(pop, years)
  })
  
  output$gdp_growth_chart <- renderPlotly({
    gdp_growth_chart <- create_gdpGrowth(gdp, input$country_gpd, input$start_year, input$end_year)
    
    # Return the plotly chart
    gdp_growth_chart
  })
  
  output$rate_emp <- renderPlotly({
    years <- input$year_emp
    countries <- input$country_emp
    rate_plot(gradpc, years,countries)
  })
  
  output$emp_country <- renderPlotly({
    
    employment_rate_plot <- function(data, countries, start_year, end_year, Sex) {
      filtered_data <- data %>%
        filter(country %in% countries, year >= start_year, year <= end_year, sex == Sex)
      
      filtered_data$year <- as.character(filtered_data$year)
      
      p <- plot_ly(filtered_data, x = ~year, y = ~emp, color = ~country,
                   type = 'scatter', mode = 'lines+markers') %>%
        layout(
          title = "Employment Rate of OECD Countries",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Employment Rate"),
          legend = list(orientation = "h")
        )
      
      ggplotly(p)
    }
    countries <- input$country_emp
    start_year <- input$startYearInput
    end_year <- input$endYearInput
    sex <- switch(input$sexInput,
                  "Male" = "M",
                  "Female" = "F",
                  "Total" = "_T")
    
    employment_rate_plot(gradpc, countries, start_year, end_year, sex)
  })
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$download,
           "GDP" = gdp,"Employment Rate" = emp,"Population" = pop, 
           "Better Index" = df_new, "ICT" = ict,"MEI" = df_long, 
           "Working-Age-Polution(>15)" = wap, "Growth GPD" = gdp_growth 
          )
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    })
  
  # KHANHLINH
  # Biểu đồ histogram
  output$histogram <- renderPlot({
    ict_country <- ict[ict$Country == input$nation, ]
    hist(ict_country$Value, main = paste("Tần suất của chỉ số ICT -", input$nation), xlab = "Giá trị", ylab = "Tần suất", col = "blue", density = 25, angle = 60, border = "black")
  })
  
  # Biểu đồ giá trị trung bình
  output$avg_value <- renderPlot({
    ict_avg <- ict %>%
      group_by(Country) %>%
      summarize(avg_value = mean(Value, na.rm = TRUE))
    ggplot(ict_avg, aes(x = Country, y = avg_value)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Giá trị trung bình của biến Value theo quốc gia",
           x = "Quốc gia",
           y = "Giá trị trung bình") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data table
  output$data_table <- DT::renderDT({  
    ict_country <- ict[ict$Country == input$nation, ]
    ict_country
  })
  
  # Biểu đồ pie chart
  output$pieChart <- renderPlot({
    ict_country <- ict[ict$Country == input$nation, ]
    summary_table <- table(ict_country$BRKD)
    ict_summary <- as.data.frame(summary_table)
    colnames(ict_summary) <- c("Business Category", "Count")
    
    pie(ict_summary$Count, labels = ict_summary$`Business Category`, col = rainbow(length(ict_summary$Count)))
  })
  
  # Biểu đồ cột thể hiện tầm quan trọng của chỉ số ICT cho các quốc gia
  output$data_chart <- renderPlot({
    ggplot(avg_ict, aes(x = reorder(Country, avg_ict_value), y = avg_ict_value)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(title = "Tầm quan trọng của ICT",
           x = "Quốc gia",
           y = "Giá trị trung bình") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # Biểu đồ tương tác giữa các loại doanh nghiệp theo từng quốc gia
  output$business_chart <- renderPlotly({
    ict_country <- ict[ict$Country == input$nation, ]
    business_counts <- table(ict_country$BRKD)
    business_data <- data.frame(Business_Category = names(business_counts), Count = as.numeric(business_counts))
    
    plot_ly(business_data, x = ~Business_Category, y = ~Count, type = 'bar', 
            marker = list(color = rainbow(length(business_counts)))) %>%
      layout(title = paste("Phân tích Doanh nghiệp tại", input$nation),
             xaxis = list(title = "Loại Doanh nghiệp"),
             yaxis = list(title = "Số Lượng"))
  })
  
  #NGOCMAI
  # Render the selected chart
  # Hàm để vẽ biểu đồ dynamic world map tương tác
  output$better_index <- renderPlotly({
    draw_world_map(df_new, input$chartTypes1)
  })
  
  # Data table
  output$table_better <- DT::renderDT({  
    df_new
  })
  
  #THUHA
  data_filtered <- reactive({
    req(input$submit)  # Đảm bảo rằng nút submit đã được nhấn
    df_long %>%
      filter(Country == input$country3,
             Month >= input$dateRange[1],
             Month <= input$dateRange[2])
  })
  
  output$dygraphPlot <- renderDygraph({
    dygraph(combined_xts, main = "Price, RSI và MACD") %>%
      dyRangeSelector() %>%
      dyAxis("y", label = "Giá") %>%
      dySeries("Price", label = "Giá", axis = "y") %>%
      dySeries("rsi", label = "RSI") %>%
      dySeries("macd", label = "MACD") %>%
      dyOptions(stackedGraph = TRUE)
  })
  
  output$summaryPlot1 <- renderPlotly({
    plot_ly(data = summary_by_country, x = ~Mean, y = ~Country, type = "bar", marker = list(color = "steelblue")) %>%
      layout(yaxis = list(title = "Quốc Gia"), xaxis = list(title = "Giá Trị Trung Bình")) %>%
      layout(title = "Tổng Quan Tỉ Giá Hối Đoái Trung Bình Theo Quốc Gia")
  })
  
  output$summaryPlot2 <- renderPlotly({
    plot_ly(data = summary_by_month, x = ~Month, y = ~SD, type = "scatter", mode = "lines+markers",
            line = list(color = "blue"), marker = list(color = "red", size = 3)) %>%
      layout(title = "Biến động trung bình theo tháng",
             xaxis = list(title = "Tháng"), yaxis = list(title = "Độ lệch chuẩn"),
             showlegend = FALSE)
  })
  
  output$individualPlot <- renderPlotly({
    plot_ly(df_long %>%
              filter(Country == quoc_gia_bien_dong_nhat),
            x = ~Month, y = ~Value, type = "scatter", mode = "lines", line = list(color = "red")) %>%
      layout(title = paste("Biến Động Tỉ Giá Hối Đoái của", quoc_gia_bien_dong_nhat),
             xaxis = list(title = "Tháng"), yaxis = list(title = "Giá Trị"))
  })
  
  output$rankingPlot <- renderPlotly({
    plot_ly(summary_by_country, x = ~reorder(Country, SD), y = ~SD, type = "bar") %>%
      layout(title = "Xếp Hạng Biến Động Tỉ Giá Hối Đoái Theo Quốc Gia",
             xaxis = list(title = "Quốc Gia"), yaxis = list(title = "Độ Lệch Chuẩn"),
             showlegend = FALSE)
  })
  
  output$ratePlot <- renderPlot({
    req(data_filtered())
    ggplot(data_filtered(), aes(x = Month, y = Value)) +
      geom_line() +
      labs(title = paste("Biểu Đồ Tỷ Giá Hối Đoái Của", input$country3),
           x = "Tháng",
           y = "Tỷ Giá Hối Đoái")
  })
  
  output$dataTable <- renderDT({
    req(data_filtered())
    datatable(data_filtered(), options = list(pageLength = 10))
  })
  
  observeEvent(input$download_btn, {
    dataset <- input$dataset
    filter <- input$filter
    start_time <- input$start_time
    end_time <- input$end_time
    last_n_observations <- input$last_n_observations
    
    # Download data
    data <- get_data(dataset, filter, start_time, end_time, last_n_observations)
    
    # Show data
    output$table_oced <- renderDataTable(data)
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)

