library(shiny)
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(lubridate)
#library(rdrop2)
library(shinydashboard)
library(shinylive)
library(httpuv)
library(tidyverse)
library(knitr)
library(ggplot2)
library(shiny.router)
library(ggrepel)
library(hrbrthemes)
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

#collegedollar <- drop_na(read_csv("EPI_College_wage_premium.csv"))
#stack_bar <- read_csv("stack_bar.csv")
#long_dat <- read_csv("long_dat.csv")
#underemp_ts <- read_csv("underemp_ts.csv")
#scat_dat <- read_csv("scat_dat.csv")
#coll_bar <- read_csv("coll_bar.csv")
#fos_interest <- read_csv("fos_interest.csv")
#long_dat <- read_csv("long_dat.csv")
#my <- read_csv("my.csv")

stack_bar <- read_csv("stack_bar.csv")
long_dat <- read_csv("long_dat.csv")
underemp_ts <- read_csv("underemp_ts.csv")
scat_dat <- read_csv("scat_dat.csv")
coll_bar <- read_csv("coll_bar.csv")
#long_dat <- read_csv("long_dat.csv")
fos_interest <- read_csv("fos_interest.csv")
my <- read_csv("my.csv")

source("ui.R")
source("server.R")


# Run the application 
shinyApp(ui = ui, server = server)
