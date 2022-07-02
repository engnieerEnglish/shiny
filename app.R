
library(shiny)
library(dplyr)
library(plotly)
library(DT)

ui <- fluidPage(

    titlePanel("Study record"),

    sidebarLayout(
        sidebarPanel(
            uiOutput("uiBeginDay"),
            uiOutput("uiEndDay")
        ),
        mainPanel(
            plotlyOutput("studyRecord"),
            dataTableOutput("recordTable")
        )
    )
)

server <- function(input, output) {

    globalData = read.csv("studyRecord.csv",
                          header = TRUE, stringsAsFactors = FALSE)
    globalData$day = as.Date(globalData$day)
    
    output$uiBeginDay = renderUI({
        selectInput("beginDay", label = "Begin Day",
                    choices = globalData$day)
    })
    output$uiEndDay = renderUI({
      selectInput("endDay", label = "End Day",
                  choices = globalData$day)
    })
    output$studyRecord <- renderPlotly({
        isFirstLoad = is.null(input$beginDay) || is.null(input$endDay) ||
          input$beginDay == "" || input$endDay == ""
        if (isFirstLoad) {
          selectedDay = globalData$day
        } else {
          selectedDay = globalData[
            between(globalData$day, as.Date(input$beginDay), as.Date(input$endDay)), "day"]
        }
        plot_ly(source = "record", x = selectedDay,
                y = globalData[globalData$day %in% selectedDay, "minute"], type = "scatter", mode = "line",
                color = globalData[globalData$day %in% selectedDay, "content"], colors = c("blue", "red")) %>%
            layout(title = "record", xaxis = list(title = "day"),
                   yaxis = list(title = "minute"))
    })
    output$recordTable = renderDataTable({
        clickData = event_data("plotly_click", source = "record")
        if (is.null(clickData)) {
            return()
        }
        # legendOrder = sort(unique(globalData$content))
        # legendIndex = 0:(length(legendOrder) - 1)
        # legendTable = data.frame(index = legendIndex,
        #                          legend = legendOrder,
        #                          stringsAsFactors = FALSE)
        # clickLegend = legendTable[legendTable$index == clickData$curve,
        #                           "legend"]
        # DT::datatable(filter(globalData, day == clickData$x &
        #                          content == clickLegend))
        DT::datatable(filter(globalData, day == as.Date(clickData$x)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
