#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
#library(rdrop2)
library(shinylive)
library(httpuv)
library(lubridate)
library(shinydashboard)
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
library(httr)
library(shiny)

#collegedollar <- drop_na(read_csv("EPI_College_wage_premium.csv"))
stack_bar <- read_csv("stack_bar.csv")
long_dat <- read_csv("long_dat.csv")
underemp_ts <- read_csv("underemp_ts.csv")
scat_dat <- read_csv("scat_dat.csv")
coll_bar <- read_csv("coll_bar.csv")
#long_dat <- read_csv("long_dat.csv")
fos_interest <- read_csv("fos_interest.csv")
my <- read_csv("my.csv")


#ggplot(coll_bar, aes(fill=degree, x=Date, y=dollars))+geom_bar(position="dodge", stat="identity")

# Define server logic required to draw a histogram
function(input, output, session) {

  
  
  output$distPlot <- renderPlot({
    fos_int_sub <- reactive ({
    
      filter(fos_interest, (CREDDESC == input$variable) & EARN_MALE_WNE_MDN_1YR<= 125000 & EARN_NOMALE_WNE_MDN_1YR <= 125000 & EARN_MALE_WNE_MDN_4YR<= 125000 & EARN_NOMALE_WNE_MDN_4YR <= 125000)
    })
    #fos_int_sub <- subset(fos_interest, (CREDDESC == input$variable) & EARN_MALE_WNE_MDN_1YR<= 125000 & EARN_NOMALE_WNE_MDN_1YR <= 125000 & EARN_MALE_WNE_MDN_4YR<= 125000 & EARN_NOMALE_WNE_MDN_4YR <= 125000)
  
    ggplot(fos_int_sub(), aes(x=x) ) +
      # Top
      geom_density( aes(x = EARN_MALE_WNE_MDN_1YR,y = after_stat(count)), color="darkslategray3", linewidth=.8) +
      geom_density( aes(x = EARN_NOMALE_WNE_MDN_1YR,y = after_stat(count)), color="deeppink1", linewidth = .8) +
      geom_label( aes(x=100000, y=0.2, label="Median Earnings \nAfter 1 year"), color="#69b3a2")  + annotate("text", x = c(15000, 60000), y = .3, label = c("Female", "Male"), color = c("deeppink1", "darkslategray3"))+
      # Bottom
      geom_density( aes(x = EARN_NOMALE_WNE_MDN_4YR,y = -after_stat(count)), color="deeppink", alpha=.35, linewidth=.8) +
      geom_label( aes(x=100000, y=-0.2, label="Median Earnings \nAfter 4 years"), color="#69b3a2") +
      geom_density( aes(x = EARN_MALE_WNE_MDN_4YR,y = -after_stat(count)), color="darkslategray3", alpha=.5, linewidth =.8)+ annotate("text", x = c(15000, 65000), y =- .25, label = c("Female", "Male"), color = c("deeppink1", "darkslategray3")) + ggtitle("Earned Income by Graduates After 1 Year vs. After 4 Years by Gender") + geom_hline(yintercept = 0) +
      theme(plot.title = element_text(hjust = 0.5))+ xlab("Earnings")+ylab("Density")
  })
  output$SAT <- renderPlot({ 
   # mine <- reactive({
      
   #   filter(my, ADM_RATE_ROUND == input$satscore)
    #})
     
    ggplot(filter(my, ADM_RATE_ROUND == input$satscore), aes(y=good_lab, x=Score,  fill=good_lab)) +
      geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
      theme_ridges()+
      theme(legend.position="none",
            panel.spacing = unit(0.1, "lines"),
            strip.text.x = element_text(size = 4)) + ylab("Score Density")+ ylab("Score Distribution")+ggtitle("Distribution of SAT Scores For American College Programs")
  })

  output$tuition <- renderPlot({
    ggplot(stack_bar, aes(group=port_inst_dat.REGION, y = value, x=fact_tuition, fill = factor(port_inst_dat.REGION))) + 
      geom_bar(position="fill", stat="identity") + theme(axis.text.x = element_text(angle = 45, vjust = -0.5, hjust=.1)) +  scale_fill_manual(values = c("darkolivegreen", "antiquewhite3", "aquamarine", "bisque", "cadetblue3", "darkseagreen","cornflowerblue"
                                                                                 , "chartreuse3", "burlywood3", "brown"), name = "Region", labels = c("U.S. Service Schools", "New England", 
                                                                                                                                                      "Mid East", "Great Lakes", "Plains", 
                                                                                                                                                      "Southeast", "Southwest", "Rocky Mountains",
                                                                                                                                                      "Far West","Outlaying Areas"))+ 
      xlab("Cost of Yearly School Tuition")+ ylab(
        "Proportion of Schools in Each Category")+ ggtitle("Yearly Tuition Cost Based on Region")                                                        
  })
  output$mfearnings <- renderPlot({
    ggplot(long_dat, aes(y=Earnings
                         , x=CREDDESC, fill = Gender)) + geom_boxplot() +labs(y="4 Year Post-grad Earnings", x="Highest Degree Earned")+ 
      scale_fill_manual(labels=c("Male","Female"), guide = guide_legend(reverse=TRUE), values=c("lightblue", "pink"))  + ggtitle("The Effect of Gender on Average Earned \nIncome 4 Year After Graduation") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
     
    
    output$underemp <- renderPlot({
    pop = "All"
    desired <-reactive({
    
    if (input$age != "All" & input$gender != "All" & input$race != "All")
    {paste(input$race, input$gender, input$age, sep = ".")}
    else if (input$degree != "All" & input$gender != "All" & input$race != "All")
    {paste(input$race, input$gender, input$degree, sep = ".")}
    
    else if (input$age != "All" & input$gender != "All") 
    {paste(input$gender, input$age, sep = ".")}
    else if (input$degree != "All" & input$gender != "All")
    {paste(input$gender, input$degree, sep = ".")}
    else if (input$age != "All" & input$race != "All")
    {paste(input$race, input$age, sep = ".")}
    else if (input$degree != "All" & input$age == "All" & input$gender == "All" & input$race != "All")
    {paste(input$race, input$degree, sep = ".")}
    else if (input$gender != "All" & input$race != "All")
    {paste(input$race, input$gender, sep = ".")}
    else if (input$degree != "All")
    {input$degree}
    else if (input$age != "All")
    {paste("X", input$age, sep = "")}
    else if (input$gender != "All")
    {input$gender}
    else if (input$race != "All")
    {input$race}
    else {"All"}
    
    #input$degree == "All" & input$age == "All" & input$gender == "All" & input$race == "All
    
    })
    
    
    ggplot(underemp_ts, aes(x= new_dates,y=points,group = condition, colour = condition), linewidth = 2)+
      geom_line() +
      theme(legend.position="none")+xlab("Time") + ylab("Percent of the Population Under Employed")+ggtitle("Percent of People Underemployed Over Time")+
      gghighlight((condition == desired() | condition == pop), max_highlight = 5L, unhighlighted_params = list(linewidth = 1))
  })
    
  output$incomeearnings <- renderPlot({
    ggplot(scat_dat, aes(x=MEDIAN_HH_INC, y=COSTT4_A,group=REGION,color=REGION, size=UGDS)) +
      geom_point() +
      ggtitle("How Does Median Household Income Compare to Average School Cost?") +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +xlab("Median Household Income For Each Institution") + ylab("Overall Cost of Student Attendance")
    # ggMarginal(p, type = "boxplot")
  })
  output$incomesizemhhi <- renderPlot ({
  #weoh <- reactive({
    
   # filter(scat_dat, REGION == input$reg)
  #})
  
  
  
    p <- ggplot(filter(scat_dat, REGION == input$reg), aes(x=MEDIAN_HH_INC, y=COSTT4_A,group=REGION,color=REGION, size=UGDS)) +
      geom_point() +
      ggtitle("How Does Median Household Income Compare to Average School Cost?") +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +xlab("Median Household Income For Each Institution") + ylab("Overall Cost of Student Attendance") 
    ggMarginal(p, type = "boxplot")
    
  })
  

  output$collegedollar <- renderPlot ({
    #checkboxInput("checkbox", "Adjust for Inflation", value = TRUE)
    if(input$checkbox){
      ggplot(coll_bar, aes(fill=degree, x=Date, y=dollars*inflation1))+geom_bar(position="dodge", stat="identity")+scale_fill_manual(values = c("darkolivegreen", "darkseagreen"), name = "Degree Level", labels = c("Bachelor's Degree", "High School Degree")) + xlab("Year")+ylab("Average Hourly Wage")
      }else{
      ggplot(coll_bar, aes(fill=degree, x=Date, y=dollars))+geom_bar(position="dodge", stat="identity")+scale_fill_manual(values = c("darkolivegreen", "darkseagreen"), name = "Degree Level", labels = c("Bachelor's Degree", "High School Degree"))+ xlab("Year")+ylab("Average Hourly Wage")
        }
  })

}


