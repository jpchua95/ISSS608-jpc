pacman::p_load(shinydashboard, tidyverse, plotly, sf, tmap, GGally, ggstatsplot, ggmosaic)

dengue_daily <- read_csv("data/dengue_daily_en.csv")
dengue_daily_sf <- st_as_sf(dengue_daily, 
                            coords = c("X_coord", "Y_coord"), 
                            crs = 3824)


print(class(dengue_daily_sf))

ui <- dashboardPage(
    dashboardHeader(title = "Taiwan Dengue Cases Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            menuItem("Association Test", tabName = "association_test", icon = icon("flask")),
            menuItem("Time Series Analysis", tabName = "association_test", icon = icon("line-chart"))
        )
    ),
    dashboardBody(
        tags$style(HTML("
            #chi_square_result {
            font-size: 20px;
            }
                        ")),
        tabItems(
            tabItem(
                tabName = "overview",
                tabsetPanel(
                    type = "tabs",
                    tabPanel("Overview of Dengue Cases in Taiwan",
                        fluidRow(
                            column(6, box(selectInput(inputId = "tw_map_variable",
                                                       label = "Select a variable:",
                                                       choices = c("Age Group" = "Age_Group",
                                                                   "County" = "Residential_County_City",
                                                                   "Imported Case" = "Imported_Case",
                                                                   "Serotype" = "Serotype"),
                                                       selected = "Age Group"),
                                          selectInput(inputId = "tw_map_gender",
                                                      label = "Select Gender:",
                                                      choices = c("All", 
                                                                  "Male" = "M", 
                                                                  "Female" = "F"),
                                                      selected = "All"),
                                          sliderInput(inputId = "tw_map_variable_year",
                                                       label = "Select Year:",
                                                       min = min(dengue_daily_sf$Onset_Year),
                                                       max = max(dengue_daily_sf$Onset_Year)-1,
                                                       value = max(dengue_daily_sf$Onset_Year),
                                                       step = 1,
                                                       animate = TRUE,
                                                       sep = ""),
                                          sliderInput(inputId = "tw_map_variable_epiweek",
                                                       label = "Select Week Range:",
                                                       min = min(dengue_daily_sf$Onset_Epiweek),
                                                       max = max(dengue_daily_sf$Onset_Epiweek),
                                                       value = c(min(dengue_daily_sf$Onset_Epiweek), max(dengue_daily_sf$Onset_Epiweek)),
                                                       step = 1,
                                                       animate = TRUE,
                                                       sep = "")
                                           )
                                   ),
                            column(6, tmapOutput("tw_map"))
                            )
                        ),
                    tabPanel("Dengue Cases Over the Years",
                        fluidRow(
                            column(6, box(selectInput(inputId = "chart_variable",
                                                       label = "Select a variable:",
                                                       choices = c("Age Group" = "Age_Group",
                                                                   "Gender",
                                                                   "County" = "Residential_County_City",
                                                                   "Imported Case" = "Imported_Case",
                                                                   "Serotype" = "Serotype"),
                                                       selected = "Age Group"),
                                           selectInput(inputId = "chart_type",
                                                       label = "Chart Type:",
                                                       choices = c("Bar",
                                                                   "Line"),
                                                       selected = "Bar")
                                           )
                                    ),
                             column(6, plotOutput("chart_visualisation"))
                            )
                        )
                    )
                ),
            tabItem(
                tabName = "association_test",
                h2("Overview of Dengue Cases in Taiwan"),
                fluidRow(
                    column(12, box(selectInput(inputId = "association_test_variable_1",
                                             label = "Variable 1:",
                                             choices = c("Age Group" = "Age_Group",
                                                         "Gender" = "Gender",
                                                         "County" = "Residential_County_City",
                                                         "Imported Case" = "Imported_Case"),
                                             selected = "Age Group"),
                                 )
                           ),
                    column(12, box(selectInput(inputId = "association_test_variable_2",
                                    label = "Variable 2:",
                                    choices = c("Age Group" = "Age_Group",
                                                "Gender" = "Gender",
                                                "County" = "Residential_County_City",
                                                "Imported Case" = "Imported_Case"),
                                    selected = "Imported_Case"),
                                  )
                    )
                ),
                textOutput("chi_square_result"),
                plotOutput("association_test_plot")
                
            )
        )
    )
)


server <- function(input, output) {

    output$tw_map <- renderTmap({
        
        aggregated  <- dengue_daily_sf %>%
            filter(Onset_Year == input$tw_map_variable_year) %>%
            filter(Onset_Epiweek >= input$tw_map_variable_epiweek[1] & 
                       Onset_Epiweek <= input$tw_map_variable_epiweek[2])
        
        if (input$tw_map_gender != "All") {
            aggregated <- aggregated %>%
                filter(Gender == input$tw_map_gender)
        }
        
        dengue_aggregated <- aggregated %>%
            #group_by(Onset_Year, Onset_Epiweek, Age_Group, Gender, Residential_County_City,
            #         Imported_Case, Serotype, MOI_Residential_County_Code, geometry) %>%
            group_by(Onset_Year, Onset_Epiweek, Gender, .data[[input$tw_map_variable]], geometry) %>%
            summarize(Count = n(), .groups = "drop")

        tm_shape(dengue_aggregated) + 
            tm_bubbles(fill = input$tw_map_variable,
                      # size = "Count",
                       col = "black",
                       lwd = 1)
        })
    
    output$chart_visualisation <- renderPlot({
        group_by_chart_variable <- dengue_daily %>%
            group_by(!!sym(input$chart_variable), Onset_Year) %>%
            summarize(Count = n(), .groups = "drop")
        
        {
            if (input$chart_type == "Bar") {
                ggplot(group_by_chart_variable, aes(x = Onset_Year, y = Count, fill = factor(.data[[input$chart_variable]]))) +
                    geom_bar(stat = "identity") +
                    labs(title = paste("Dengue Cases by", input$chart_variable, "Across Years"),
                         x = "Year",
                         y = "Count of Cases",
                         fill = input$chart_variable)
            } else if (input$chart_type == "Line") {
                ggplot(group_by_chart_variable, aes(x = Onset_Year, y = Count, color = factor(.data[[input$chart_variable]]))) +
                    geom_line() +
                    labs(title = paste("Dengue Cases by", input$chart_variable, "Across Years"),
                         x = "Year",
                         y = "Count of Cases",
                         color = input$chart_variable)
            }
            }
    })
    
    
    output$chi_square_result <- renderText({
        req(input$association_test_variable_1, input$association_test_variable_2)
        
        table_association_test <- table(dengue_daily[[input$association_test_variable_1]], 
                                        dengue_daily[[input$association_test_variable_2]])
        
        chi_square_test <- chisq.test(table_association_test)
        
        paste("Chi-Square Value: ", chi_square_test$statistic, " | ",
              "p-value: ", chi_square_test$p.value)
    })
    
    output$association_test_plot <- renderPlot({
        
        
        ggbarstats(data = dengue_daily, 
                   x = input$association_test_variable_1, 
                   y = input$association_test_variable_2)
    })
    
    
}


shinyApp(ui = ui, server = server)
