#Mekias Kebede
#12/10/2023
#INFO201 Final Project - Shiny App
#I worked on this project all on my own without a team. 
#Final Project Shiny App

library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("Final Project.R") 

poverty2 <- read.csv("pip_dataset.csv")
companies <- c("Google", "Microsoft", "Apple")
grouped_pov <- group_by(poverty2, country)
pov3 <- summarize(grouped_pov, avg_gini = mean(gini), yr = year, country = country, .groups = "keep")

# UI
ui <- fluidPage(
  titlePanel("Examining Big Tech Effect on Poverty"),
  br(),
  p("The influence of big tech on our society and world is a complex and multifaceted discussion, 
    encompassing both positive and negative impacts. And while it may be that the overall
    net impact of technology has been positive, one of the evergrowing negative aspects is its contribution
    to the wealth inequality to our world. According to many economists a small collection of some of the
    largest technology companies in the world have risen to incredible financial success make billions of 
    dollars but in the process ravaging the econmoic success of those outside the tech space."),
  br(),  
    p("Technology companies like Google, Facebook, Microsoft, Apple, IBM, Amazon, etc have played a significant 
    role in exacerbating many social, economic, and political inequities; one of the largest being increasing 
    the evergrowing wealth disparity among citizens in the United states, as well as all over the world. The 
    exorbinat amount of money technology and information based companies make have raised critisms by many 
    economists to be increasing the wealth gap and furthering poverty. These inequalities can be listed as:
    Income Disparities, Gentrification, Stock Market Influence, Tax Avoidance, Monopoly Power"),
  br(),
  p("My analysis first began with trying to see if wealth inequality really still is a problem in united
    states and across the world. In addition to this I wanted to visualize the progression of income
    inequality throughout time for each country including the United States. In order to do this things 
    I isolated 2 variables that quantify my question perfectly; time, and the gini coefficient"),
  br(),
  #HTML styling 
  tags$style(HTML("
    h2 {
            background-color: #ccd5d8;
            color: Black;
            }")),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(
                    inputId = "country",
                    label = "Choose a country", choices = pov3$country
                  ), 
                  br(),
                  p(paste("
                          The Gini coefficient, or Gini index, is a measure of statistical dispersion used to 
                          represent the income or wealth distribution within a population. It quantifies the degree 
                          of inequality in a distribution, providing a numerical representation of the gap between 
                          the rich and the poor in a given society. The Gini coefficient is expressed as a value 
                          between 0 and 1, where 0 represents perfect equality (everyone has the same income or wealth), 
                          and 1 represents perfect inequality (one individual or group possesses all the income or 
                          wealth, while everyone else has none). The average gini coefficient of the world being 0.67!
                          And as you can see with any country choosen, if you scroll down to the United States, you'll see
                          a linear progression of the gini coefficient"))
                ),
                
                mainPanel(
                  h3("Gini Index Over Time By Country"),
                  plotlyOutput(outputId = "scatter")
                )
  ),
  titlePanel("Top Big Tech Performers"),
  p("I isolated 3 of the top technology companies in the United States; Google, Microsoft, and Apple for simplicity at showing their
    financial performing statistics from 2000 to 2022. The goal of this graphic was too see how the average stock price highs of these
    top tech performers have progressed throughout time. Clearly its is evident in in each company that there is a significant increase
    in stock price throughout the time of the existance of these companies."), 
  selectInput(
    inputId = "company",
    label = "Google, Microsoft, or Apple?", choices = companies
  ),
  mainPanel(
    plotlyOutput(outputId = "bar") 
  ), 
  titlePanel("Why is this important?"), 
  br(),
  p("Between the years 2000 and 2022, two of the largest hits on the economic prosperiety of citizens in the United States and around 
    the world occured; the 2008 financial crisis and the COVID-19 pandemic. Two catestrophic economic disasters within a two decade
    time span that negativly affected the livlyhood of humans all over the world, but had a net positive impact on many technology 
    companies throughout the same timeline. This is shown in the data I have analyzed and visualized through the average 
    non zero gini coefficient for every country including the United States increasing and or remaining constant at high economic
    inequality figures all while stock prices of the three largest technology companies have increased drastically! To put it
    simply this is not right! Adressing wealth inequality is more important than ever in our society in order to maintain social, 
    economic, and political stability."),
  br(),
)
# Server
server <- function(input, output) {
  output$scatter <- renderPlotly({
    p <- ggplot(data = filter(poverty2, poverty2$country == input$country)) + geom_point(mapping = aes(x = year, y = gini))
    return(p)
  })
  output$bar <- renderPlotly({
    if(input$company == "Google"){
      p_goog <- ggplot(data = df3_pov) + geom_bar(mapping = aes(x = yr, y = avg_High.google), stat = 'identity')
      return(p_goog)
    } else if(input$company == "Apple"){
      p_appl <- ggplot(data = df3_pov) + geom_bar(mapping = aes(x = yr, y = avg_High.apple), stat = 'identity')
      return(p_appl)
    } else if(input$company == "Microsoft"){
      p_micro <- ggplot(data = df3_pov) + geom_bar(mapping = aes(x = yr, y = avg_High.microsoft), stat = 'identity')
      return(p_micro)
    }
  })
}
shinyApp(ui = ui, server = server)