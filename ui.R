#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinylive)
library(httpuv)
library(dplyr)
library(lubridate)
library(shinydashboard)
#library(rdrop2)
#library(ggbio)
library(tidyverse)
library(knitr)
library(ggplot2)
library(shiny.router)
library(ggrepel)
library(hrbrthemes)
#hrbrthemes::import_roboto_condensed()
library(ggridges)
library(forcats)
library(geomtextpath)
library(gghighlight)
library(sf)
library(ggExtra)
library(GGally)
library(viridis)
library(plotly)
library(gapminder)
library(readr)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title = "The Cost of College"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gender Pay Differentials", tabName = "tab1", icon = icon("dashboard")),
      menuItem("SAT Score Distribution", tabName = "tab2", icon = icon("chart-bar")),
      menuItem("Tuition Cost By Region", tabName = "tab3", icon = icon("table")),
      menuItem("Income v Student Debt", tabName = "tab6", icon = icon("table")),
      menuItem("UnderEmployment by Category", tabName = "tab7", icon = icon("chart-bar")),
      menuItem("Wage Growth with Education", tabName = "tab9", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1
      tabItem(
        tabName = "tab1",
        
        # Show a plot of the generated distribution
        
        fluidRow(
          
          box(
            width = 3,
            
            selectInput("variable", "Degree Obtained:",
                        c("Associate's" = "Associate's Degree",
                          "Bachelor's" = "Bachelor's Degree",
                          "Master's" = "Master's Degree",
                          "Doctorate" = "Doctoral Degree")),
            "The plot shown displays the average different in pay for females vs and non-females in pay as reported both one year after degree conferral and 4 years after degree conferral. For all education groups, males tend to outearn females in this metric as reported on the Department of Education's School Report Card Dataset. \n\n Data from https://collegescorecard.ed.gov/data/ Institution-Level Data"
           
          ),
          
          
          box(plotOutput("distPlot"), plotOutput("mfearnings"), width = 9, status = "primary")
          
        )
      ),
      
      # Tab 2
      tabItem(
        tabName = "tab2",
        titlePanel("Average SAT For Each Year "),
        fluidRow(
          box(
            title = "Average Distribution of SAT Scores By School Acceptance Rate",
            width = 3,
            sliderInput("satscore", "Acceptance Rate", min = 0, max = 100, value = 50, step = 10, 
                        animate = TRUE, ticks = TRUE, post = "%"),
            
            "The ridgeline plot shows the distribution of SAT scores at United States schools. The slider allows you to manipulate distribution and look at average SAT scores of schools with similar acceptance rates.
            As the acceptance rate gets lower, the variance in SAT scores grows significantly smaller and the most common scores appear to be much higher.
            For scores ranging around 50%, the variance in scores appears to be the highest, and schools with acceptance rates closer to 100%, SAT scores have a large variance, with median values trending lower.
            \n\n Data from https://collegescorecard.ed.gov/data/ Institution-Level Data"
            
          ),
          box(plotOutput("SAT"), width = 9, status = "primary"))
      ),
      # Tab 3
      tabItem(
        tabName = "tab3",
        titlePanel("School Tuition Cost Per Region"),
        fluidRow(
          box(title = "What are The Most Affordable Regions of the U.S. To Get A Degree?", width = 3, 
              "The price of college is a constantly Rising Expense for the average American. In Different Regions of the 
              country, the average cost of school differs greatly. According to the graph shown, The majority of schools costing over $65,000
              exist in the Mideastern states (Delaware, Washington D.C., Maryland, New Jersey, New York and Pennslyvania), New England 
              (Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, Vermont), and Far West states.
              which are (California Nevada Oregon and Washington). \n\n Data from https://collegescorecard.ed.gov/data/ Institution-Level Data"),
          box(plotOutput("tuition"), width = 9, status = "primary")
        )
      ),
     
      # Tab 6
      # tabItem(
      #   tabName = "tab6",
      #   titlePanel("Post Graduate Earnings By Gender and by Highest Degree Earned"),
      #   fluidRow(plotOutput("mfearnings"))
      # ),
      
      tabItem(
        tabName = "tab6",
        titlePanel("Family Income Compared to Average School Cost"),
        fluidRow(
          box(
            title = "How Does the Median Household Income of Students Compare to School Costs?",
            width = 3,
            radioButtons("reg", "Choose a Region", c("New England" = 1, 
                                                     "Mid East" = 2, 
                                                     "Great Lakes" = 3, 
                                                     "Plains" = 4, 
                                                     "Southeast" = 5, 
                                                     "Southwest" = 6, 
                                                     "Rocky Mountains" = 7,
                                                     "Far West" = 8,
                                                     "Outlaying Areas" = 9)),
            
            "The ridgeline plot shows the distribution of SAT scores at United States schools. The slider allows you to manipulate distribution and look at average SAT scores of schools with similar acceptance rates.
            As the acceptance rate gets lower, the variance in SAT scores grows significantly smaller and the most common scores appear to be much higher.
            For scores ranging around 50%, the variance in scores appears to be the highest, and schools with acceptance rates closer to 100%, SAT scores have a large variance, with median values trending lower.
            "
            
          ),
          box(plotOutput("incomesizemhhi"), width = 9, status = "primary"))
      ),
      
      
      
      tabItem(
        tabName = "tab7",
        titlePanel("Underemployed Population By Category Over Time"),
        fluidRow(
          
          box(
            width = 3,
            selectInput("degree", "Select an Education Level:",
                        c("All Educational Levels" = "All",
                          "Less Than a High School Degree" = "Less.than.HS",
                          "High School Degree" = "High.school",
                          "Attended Some College" = "Some.college",
                          "Bachelor's Degree" = "Bachelor.s.degree",
                          "Advanced Degree" = "Advanced.degree")),
            selectInput("gender", "Select a Gender:",
                        c("All Genders" = "All",
                          "Women" = "Women",
                          "Men" = "Men")),
            selectInput("race", "Select a Race:",
                        c("All Races" = "All",
                          "Black" = "Black",
                          "White" = "White",
                          "Hispanic" = "Hispanic")),
            
            selectInput("age", "Select an Age Group:",
                        c("All Ages" = "All",
                          "16-24" = "16.24",
                          "25-54" = "25.54",
                          "55-64" = "55.64",
                          "65 and over" = "65.")),
            'According to the Economic Policy Institute, "Underemployment is the share of
            the labor force that either 1) is unemployed, 2) is working part time but wants 
            and is available to work full time (an “involuntary” part timer), or 3) wants and
            is available to work and has looked for work in the last year but has given up actively
            seeking work in the last four weeks (“marginally attached” worker)." When comparing members
            of mixed demographic groups, the rate of under employment is influenced most drastically by
            education level. While those with only some high school experience were underemployed at a rate of more than 50% at times, 
            those with advanced degrees were amoung the least underemployed over time. \n\n Data from https://www.epi.org/data/#?subject=underemp'
            #sliderInput("bins",
            #            "Number of bins:",
            #           min = 1,
            #           max = 50,
            #           value = 30)
          ),
          box(plotOutput("underemp"), width = 9, status = "primary")
        )
      ),

      tabItem(
        tabName = "tab9",
        titlePanel("Family Income Compared to Average School Cost"),
        fluidRow(
          box(
            title = "How has a college Education Effected salary Over Time?",
            width = 3,
            checkboxInput("checkbox", "Adjust for Inflation", value = FALSE),
            "The graph shown uses data from the U.S. Economic Policy Institute and reflects the 
           average difference in salary between individuals who have a bachelor's degree compared 
           to those who have only completed a high school diploma in the past 35 years. While both 
           graphs show a material widening difference between wages for the college educated when compared to those with only a
           high school diploma, when accounting for inflation each year, it is also apparent that the value of the average American's pay has gone down
           regardless of education level.In order to adjust this Visualization for Inflation, I untilized the Bureau of Labor Statistics 
           Yearly Average Consumer Price Index Statistic to calculate the inflation rate and adjust the started average salary to it's equivalence in 2023"
            
          ),
          box(plotOutput("collegedollar"), width = 9, status = "primary"))
      )
    )
  )
))
