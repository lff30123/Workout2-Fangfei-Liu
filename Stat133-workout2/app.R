#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(ggplot2)
library(dplyr)

# Define UI for the application 

ui <- fluidPage(
  titlePanel("Saving-Investing Modalities"),
  fluidRow(column(4,
                  sliderInput("amount", "Initial Amount",
                              min = 0, 
                              max = 100000, 
                              step = 500,
                              value = 1000,
                              pre = "$")
  ),
  column(4,
         sliderInput("rate", "Return Rate (in %)",
                     min = 0, 
                     max = 20, 
                     step = 0.1,
                     value = 5)
  ),
  column(4,
         sliderInput("years", "Years",
                     min = 0, 
                     max = 50, 
                     step = 1,
                     value = 20)
  )),
  
  fluidRow(
    column(4,
           sliderInput("contrib", "Annual Contribution",
                       min = 0, 
                       max = 50000,
                       step = 500,
                       value = 2000,
                       pre = "$")
    ),
    column(4,
           sliderInput("growth", "Growth Rate (in %)",
                       min = 0, 
                       max = 20, 
                       step = 0.1,
                       value = 2)
    ),
    column(4,
           selectInput("facet", "Facet?", 
                       choices = list("Yes", "No"), 
                       selected = "No")
    )
  ),
  
  
  mainPanel(
    titlePanel("Timelines"),
    plotOutput("timelines", width = 600, height = 300),
    titlePanel("Balances"),
    verbatimTextOutput("balances")
  )
)


# Define server logic required to draw a graph

server <- function(input, output) {
  
  future_value <- function(amount,rate,years){end_return = amount*(1+rate)^years
  return(end_return)}
  
  annuity <- function(contrib,rate,years) {result <- contrib*((1+rate)^years-1)/rate
  return(result)}
  
  growing_annuity <- function(contrib,rate,growth,years){result <- contrib*((1+rate)^years-(1+growth)^years)/(rate-growth)
  return(result) }
  
  modalities <-  reactive({
    
    balance1 <- c()
    for (i in 0:input$years) {
      balance1 <- c(balance1 , future_value(input$amount,input$rate/100,i))}

  balance2 <- c()
    for (i in 0:input$years) {new_balance1 <- annuity(input$contrib,input$rate/100,i) + future_value(input$amount,input$rate/100,i)
    balance2 <- c(balance2 , new_balance1)}
  
  balance3 <- c()
    for (i in 0:input$years) {new_balance2 <- growing_annuity(input$contrib,input$rate/100,input$growth/100,i) + future_value(input$amount, input$rate/100,i)
    balance3 <- c(balance3 , new_balance2)}
  
  modalities <- data.frame(years = (0:input$years),no_contrib=balance1,fixed_contrib=balance2,growing_contrib=balance3)
  
  modalities
  })

# Fill in the spot we created for a plot
#Facet or not 
output$timelines <- renderPlot({
  dat1 <- melt(modalities(),id.vars = "years")
  
if (input$facet == "No") {
  ggplot(dat1, aes(x = years, y = value, color = variable)) +
    geom_line(size = 0.5) +
    geom_point(size = 0.5) +
    labs(title = "Three Modes of Investing", x = "Year", y = "Value", color = "Modality")

} else {
  # Facetting
  ggplot(dat1, aes(x = years, y = value,color=variable))+
    geom_line(size = 0.5) +
    geom_point(size = 0.5) +
    geom_area(aes(fill= variable), alpha = 0.5) +
    facet_wrap(~variable) +
    labs(title = "Three Modes of Investing", x = "Year", y = "Value")
}
  })



# Table output
output$balances <- renderPrint({
  modalities()
})}




# Run the application 
shinyApp(ui = ui, server = server)

