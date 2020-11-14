# load necessary packages
library(tidyverse)
library(rbokeh)   # interactive map plotting
library(shiny)

# read in the data
data0 <- readr::read_rds("ncaa_roster.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # add title
    titlePanel(
        "NCAA Men's Volleyball - Division I-II Historical Rosters"
        
    ), # close titlePanel
    
    sidebarLayout(
        
        # add a sidebarPanel to house an update button and filters
        sidebarPanel(
            width = 2,
            
            # create the update button
            actionButton(inputId = "update",
                         label = "Update"),
            
            # add some white space between the bottom of the button and the first filter
            br(),
            br(),
            
            # selector field for year
            selectizeInput(
                
                # inputId to refer back to in the server function below
                inputId = "year",
                
                # label as seen when the app is run
                label = "Year",
                
                # define available choices to filter by
                choices = unique(
                    data0 %>%
                        dplyr::arrange(desc(year)) %>%
                        dplyr::pull(year)),
                selected = 2021,
                
                # allow multiple years to be selected
                multiple = TRUE
                
            ), # close selectizeInput
            
            # selector field for school
            selectizeInput(
                inputId = "school",
                label = "School",
                choices = unique(
                    data0 %>%
                        dplyr::arrange(school) %>%
                        dplyr::pull(school)),
                multiple = TRUE
                
            ), # close selectizeInput
            
            # selector field for All-American status
            selectizeInput(
                inputId = "all_american",
                label = "All-American",
                choices = unique(
                    data0 %>%
                        dplyr::arrange(all_american) %>%
                        dplyr::pull(all_american)),
                multiple = TRUE
                
            ), # close selectizeInput
            
            # selector field for All-Conference status
            selectizeInput(
                inputId = "all_conference",
                label = "All-Conference",
                choices = unique(
                    data0 %>%
                        dplyr::arrange(all_conference) %>%
                        dplyr::pull(all_conference)),
                multiple = TRUE
                
            ), # close selectizeInput
            
            # selector field for home state
            selectizeInput(
                inputId = "state",
                label = "Home State",
                choices = unique(
                    data0 %>%
                        dplyr::arrange(state) %>%
                        dplyr::pull(state)),
                multiple = TRUE
                
            ), # close selectizeInput
            
            # selector field for home country
            selectizeInput(
                inputId = "country",
                label = "Home Country",
                choices = unique(
                    data0 %>%
                        dplyr::arrange(country) %>%
                        dplyr::pull(country)),
                multiple = TRUE
                
            ), # close selectizeInput
            
        ), # close sidebarPanel
        
        # define the main panel (where the map will go)
        mainPanel(
            width = 10,
            
            # call the map from the server function below and define map size
            rbokehOutput(
                outputId = "map",
                width = 1000,
                height = 1000
                
            ) # close rbokehOutput
        ) # close mainPanel
    ) # close sidebarLayout
) # close fluidPage

# Define server logic required to draw map
server <- function(input, output) {
    
    # define map output
    output$map <- renderRbokeh({
        
        # trigger update via update action button
        input$update
        
        # define map figure
        rbokeh::figure(
            width = 1440,
            height = 900) %>%
            ly_map("world",col = "gray") %>%
            ly_points(long, lat, 
                      data = data0 %>%
                          
                          # filter by given inputs from ui function above
                          # isolate() prevents the output from reloading until the
                          # update actionbutton is triggered
                          dplyr::filter(year %in% isolate(input$year) |
                                            
                                            # is.null to call all data with no filters
                                            is.null(isolate(input$year))) %>%
                          dplyr::filter(school %in% isolate(input$school) |
                                            is.null(isolate(input$school))) %>%
                          dplyr::filter(all_american %in% isolate(input$all_american) |
                                            is.null(isolate(input$all_american))) %>%
                          dplyr::filter(all_conference %in% isolate(input$all_conference) |
                                            is.null(isolate(input$all_conference))) %>%
                          dplyr::filter(state %in% isolate(input$state) |
                                            is.null(isolate(input$state))) %>%
                          dplyr::filter(country %in% isolate(input$country) |
                                            is.null(isolate(input$country))) %>%
                          
                          # group and summarise to prevent duplicate names over multiple years
                          dplyr::group_by(name,hometown,state,country,school,lat,long) %>%
                          dplyr::summarise(.groups = "drop"),
                      hover = c(name, school, hometown, state, country)) %>%
            x_axis(label = "", grid = F, visible = F) %>%
            y_axis(label = "", grid = F, visible = F)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)