#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(rsconnect)
#Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")

dat <- read_csv2("data_ulule_2019_managed.csv")

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(theme = "final_graph.css", #1o
        navbarPage("Navigate the website : ", #2o
            tabPanel("About",
                     #titlePanel("Included Content"),
                     mainPanel(
                         includeHTML("www/acc.html")
                     ))
            ,tabPanel("Nb", #3o

                # Application title
                titlePanel(h1("Number of campaigns",align="center")),

                # Sidebar with a slider input for number of bins
                sidebarLayout( #4o
                    sidebarPanel( #5o
                        sliderInput("year", #6o
                                    "Year of interest : ",
                                    min = 2011,
                                    max = 2019,
                                    value = 2015), #6f
                        radioButtons("choix_categorie", "Choose your category", choices = c("All",unique(na.omit(dat$category))))
                    ), #5f

        # Show a plot of the generated distribution
        mainPanel( #5o
            plotOutput("distPlot"),
            br(),
            plotOutput("distPlot_gen_year"),
            br(),
            plotOutput("distPlot_gen_month"),
            br()
        ) #5f end of main panel
    ) #4f end of sidebar layout
        ) #end of first tab 3f
        ,tabPanel("%", #
        
            # Application title
            titlePanel("Percentage of completed campaigns"),
            
            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel( #5o 6o
                    sliderInput( #7o
                        "year_percent",
                        "Year of interest : ",
                        min = 2011,
                        max = 2019,
                        value = 2015
                    ), #7f
                    radioButtons("choix_categorie_perc", "Choose your category", choices = c("All",unique(na.omit(dat$category))))
        ), #6f
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlotPercent"),
            br(),
            plotOutput("distPlotPercent_Year"),
            br(),
            plotOutput("distPlotPercent_Month")
            ,
            br()
            ) #end of main
        ) # end of sidebar layout
    )#end of second tab 
    
    ,tabPanel("Avg", #
              
              # Application title
              titlePanel("Average amount raised by completed campaigns"),
              
              # Sidebar with a slider input for number of bins
              sidebarLayout(
                  sidebarPanel( #5o 6o
                      sliderInput( #7o
                          "year_avg",
                          "Year of interest : ",
                          min = 2011,
                          max = 2019,
                          value = 2015
                      ), #7f
                      radioButtons("choix_categorie_avg", "Choose your category", choices = c("All",unique(na.omit(dat$category))))
                  ), #6f
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                      plotOutput("distPlotAvg"),
                      br(),
                      plotOutput("distPlotAvg_Year"),
                      br(),
                      plotOutput("distPlotAvg_Month"),
                      br()
                  ) #end of main
              ) # end of sidebar layout
    )#end of third tab 
    
    
    
    
        ) #end of navbar page 
    )#end of fluid page 
 ) #end of shinyUI
 
