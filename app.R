#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(forecast)

png("abc", type="cairo")

setwd("D:/MyWork/IIMB_BDA/AnomalyDetection")

df_clean_merged <-read.csv("./df_clean_merged.csv")
dec_df_ts<-msts(df_clean_merged$metricvalue,seasonal.periods = c(288,2016))

decom <- decompose(dec_df_ts)

random <-decom$random

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 3,
                        max = 6,
                        step = 0.5,
                        value = 3),
            checkboxInput("somevalue", "Some value", FALSE),
            verbatimTextOutput("value")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    min_val <- reactive({
        min = mean(random, na.rm = T) - input$bins*sd(random, na.rm = T)
        
    })
    
    max_val<-reactive({
        max = mean(random, na.rm = T) + input$bins*sd(random, na.rm = T)
        
    })
    

    
    anomaly_df <-reactive({
        max <-max_val()
        min <- min_val()
        print(min)
        print(max)
        print(input$bins)
        position = data.frame(id=seq(1, length(random)), value=random)
        
        anomalyH = position[position$value > max, ]
        anomalyH = anomalyH[!is.na(anomalyH$value), ]
        anomalyL = position[position$value < min, ]
        # anomalyL = anomalyL[!is.na(anomalyL$value)]
        anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                             value=c(anomalyH$value, anomalyL$value))
        
        real = data.frame(id=seq(1, length(df_clean_merged$metricvalue)), value=df_clean_merged$metricvalue)
        realAnomaly = real[anomaly$id, ]
        
    })

    output$distPlot <- renderPlot({
    
        realAnomaly<-anomaly_df()
        
        plot(as.ts(df_clean_merged$metricvalue),type = 'l',col='blue')
        points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
