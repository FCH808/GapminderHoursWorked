library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)
library(Hmisc)
library(plyr)
library(reshape2)

countries<- fread("hours_per_week.csv", header = TRUE, select = 1)
setnames(countries,"Working hours per week", "Country")

shinyUI(fluidPage(theme = "bootstrap.css",
  
  titlePanel("Hours worked per Week per Country"),
  
  sidebarPanel(

    conditionalPanel(
      condition = "input.theTabs != 'decadeTab'",
      
      selectInput('country', 'Countries to Track', c(None='.', countries$Country), 
                  selected = c("Switzerland", "United States", "Japan", "United Kingdom", "France"),
                  multiple = TRUE)
    ),
    
    conditionalPanel(
      condition = "input.theTabs == 'mediansTab'",
      
      selectInput('quart', 'Add Quartile Range', c(None = '.', "Yes")),
      
      sliderInput("slider_min", "First quantile percentage", 
                  min = 0, 
                  max = 1.00, 
                  value = .25, 
                  step = .01),
      sliderInput("slider_max", "Second quantile percentage", 
                  min = 0, 
                  max = 1.00, 
                  value = .75, 
                  step = .01),
      br(),
      checkboxInput('minmax', "Include min/max bars for each year"), 
      br(),
      
      selectInput('country_text', 'Show average hours per country each year', c(None='.', "Yes", "Just points" )),
      
      selectInput('world_text', 'Show median of all countries each year', c(None='.', "Yes", "Just points" ))
    ),
    
    conditionalPanel(
      condition = "input.theTabs != 'summaryTab'",
    selectInput('themes', "Plot Theme", c(None = '.', "Tufte", "Economist", "Solarized", "Midnight", "Stata", "Stephen Few", "Wall Street Journal"), selected = "Solarized"),
    
    br(),
    
    # Opens slidify html help document in new tab
    a('Help (Opens new tab)',href='http://fch808.github.io/GapminderHoursWorkedSlidify/#1', target="_blank"),
    
    br(),
    a(href = "https://gist.github.com/FCH808/eaa18c8095984b923462", "Source code (Opens new tab)", target="_blank")
  )),
  
    mainPanel(
      tabsetPanel(id ="theTabs",           
                  tabPanel("Medians Distributions", plotOutput('mediansPlot'), 
                           value = "mediansTab"),
                  tabPanel("Boxplots", plotOutput('boxplot'),
                           value = "boxplotsTab"),
                  tabPanel("Decades", plotOutput('decadePlot'),
                           value = "decadeTab"),
                  tabPanel("Summary",tableOutput("data_table"),tableOutput("data_table2"),
                           value = "summaryTab")
    )
  )
  
))

