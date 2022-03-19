## app.R ##
library(shinydashboard)
# library(magrittr)
library(bupaR)
library("edeaR")
library("eventdataR")
library("processmapR")
library("processmonitR")
# install.packages("xesreadR")
library("petrinetR")
source("./demo-data-preparation.R", local=TRUE)
eventlog <- timeline
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Process map", tabName = "process", icon = icon("angle-double-right")),
      #menuItem("Precedence", tabName = "precedence", icon = icon("bullseye")),

      menuItem("Documents", tabName = "documents", icon = icon("dashboard")),

      menuItem("Channels", tabName = "channels", icon = icon("th")),
      menuItem("Traces", tabName = "traces", icon = icon("braille")),
     # menuItem("Activity", tabName = "activity", icon = icon("th")),
      menuItem("Rework", tabName = "rework", icon = icon("th")),
      menuItem("Performance", tabName = "performance", icon = icon("th"))

    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "process",
              fluidRow(
                box(DiagrammeR::grVizOutput("process_plot1"), width = "100%")
                )
              ,
              fluidRow(

                box(
                  title = "Controls",
                  sliderInput("process_map_filter", "Filter trace frequency:", 1, 100, 100)
                )
              )
      ),

      # performance -----
      tabItem(tabName = "performance",
           fluidRow(
                tabBox(title="Processing time",selected="Activity",
                       tabPanel("Log", plotOutput("processing_time_log")),
                       tabPanel("Case", plotOutput("processing_time_case")),
                       tabPanel("Activity", plotOutput("processing_time_activity")),
                       tabPanel("Resource", plotOutput("processing_time_resource"))
                       , width="100)%")
              ),
          fluidRow(
            tabBox(title="Throughput time",selected='Case',
              tabPanel("Log", plotOutput("throughput_time_log")),
              tabPanel("Case", plotOutput("throughput_time_case"))
              , width="100)%")
            ),
          fluidRow(
            tabBox(title="Idle time",selected ="Resource",
                   tabPanel("Log", plotOutput("idle_time_log")),
                   tabPanel("Case", plotOutput("idle_time_case")),
                   tabPanel("Resource",plotOutput("idle_time_resource"))
                   , width="100)%")
          )



        )
      ,
   # channels ----
      tabItem(tabName = "channels",
              fluidRow(
                box(DiagrammeR::grVizOutput("channels_plot1"), width = "100%")
              )
              ,
              fluidRow(

                # box(
                #   title = "Controls",
                #   sliderInput("process_map_filter", "Filter trace frequency:", 1, 100, 100)
                # )
              )
      ),

      # rework -----
      tabItem(tabName = "rework",
              fluidRow(
                tabBox(title="Rework",
                       tabPanel("Selfloops", plotOutput("selfloops_matrix")),
                       tabPanel("Repetitions", plotOutput("repetitions_matrix"))
                    , width="100)%")
              )


      ),


      # activity -----
      tabItem(tabName = "documents",
              fluidRow(
                tabBox(title="Documents",
                       tabPanel("Frequency", plotOutput("activity_frequency")),
                     tabPanel("Precedence", plotOutput("precedence"))
                       , width="100)%")
              )

      ),

      # traces -----
      tabItem(tabName = "traces",
              fluidRow(
                box(plotOutput("traces_plot"),width = "100%")
              )
              ,
              fluidRow(

                box(
                  title = "Controls",
                  sliderInput("traces_filter", "Filter traces coverage:", 1, 100, 50)
                )
              )
    )



    #           tabPanel("Processing time",
    #                    tabPanel("Log", plotOutput("processing_time_log")),
    #                     tabPanel("Case", plotOutput("processing_time_case")),
    #                     tabPanel("Activity", plotOutput("processing_time_activity")),
    #                     tabPanel("Resource", plotOutput("processing_time_resource")))),
    # tabPanel("Idle time", navlistPanel(well = F, widths = c(2,
    #                                                         9), tabPanel("Log", plotOutput("idle_time_log")),
    #                                    tabPanel("Case", plotOutput("idle_time_case")), tabPanel("Resource",
    #                                                                                             plotOutput("idle_time_resource"))))), selectizeInput("units",
    #                                                                                                                                                  "Time units:", choices = c("min", "hours", "days", "weeks"),
    )
  )
)


server <- function(input, output) {

  output$process_plot1 <- DiagrammeR::renderGrViz({
    sub <- timeline %>% filter_trace_frequency(percentage=input$process_map_filter/100)
    sub %>% process_map()
  })


  output$channels_plot1 <- DiagrammeR::renderGrViz({
    timeline %>% resource_map()
  })

  output$throughput_time_log <- renderPlot({
    eventlog %>% throughput_time("log", units = input$units) %>%
      plot()
  })
  output$throughput_time_case <- renderPlot({
    eventlog %>% throughput_time("case", units = input$units) %>%
      plot()
  })
  output$processing_time_log <- renderPlot({
    eventlog %>% processing_time("log", units = input$units) %>%
      plot()
  })
  output$processing_time_case <- renderPlot({
    eventlog %>% processing_time("case", units = input$units) %>%
      plot()
  })
  output$processing_time_activity <- renderPlot({
    eventlog %>% processing_time("activity", units = input$units) %>%
      plot()
  })
  output$processing_time_resource <- renderPlot({
    eventlog %>% processing_time("resource", units = input$units) %>%
      plot()
  })
  output$idle_time_log <- renderPlot({
    eventlog %>% idle_time("log", units = input$units) %>%
      plot()
  })
  output$idle_time_case <- renderPlot({
    eventlog %>% idle_time("case", units = input$units) %>%
      plot()
  })
  output$idle_time_resource <- renderPlot({
    eventlog %>% idle_time("resource", units = input$units) %>%
      plot()
  })
  output$selfloops_matrix <- renderPlot({
    eventlog %>% redo_selfloops_referral_matrix() %>%
      plot()
  })
  output$repetitions_matrix <- renderPlot({
    eventlog %>% redo_repetitions_referral_matrix() %>%
      plot()
  })

  output$activity_frequency <- renderPlot({
    eventlog %>% activity_frequency("activity") %>% plot()
  })
  output$activity_presence <- renderPlot({
    eventlog %>% activity_presence() %>% plot()
  })

  output$precedence <- renderPlot({
    timeline %>% precedence_matrix() %>% plot
  })

  output$traces_plot <- renderPlot(
    timeline %>% trace_explorer(coverage=input$traces_filter/100)
  )
}

shinyApp(ui, server)
