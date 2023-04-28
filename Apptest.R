library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(highcharter)

dmin <- "2018-07-01"
dmax <- "2022-12-01"

ui <- dashboardPage(   
    dashboardHeader(title = " "),
    dashboardSidebar(
    width = 300,
        sliderInput("dateRange","Date range:",
                    min = as.Date(dmin),
                    max = as.Date(dmax),
                    value=as.Date(c(dmin, dmax)),
                    step = 3,
                    timeFormat="%b %Y")
        ),
        
    dashboardBody(
        tags$head(tags$style(HTML(
            '.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }

    '))),
        
        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Compare ACSF with Retail Trade (original series) </span>\');
      })
      ')),
        tabBox(height = "740px", width = "850px",
               tabPanel("RT v ACSF ($m)",
                        fluidRow(
                        column(12,highchartOutput("hcontainer1",height = "720px", width = "1000px")))),
               tabPanel("ACSF as % of RT",
                        fluidRow(
                            column(12,highchartOutput("hcontainer2",height = "720px", width = "1000px")))),
               tabPanel("Table", DT::dataTableOutput("table")))
            )
    )


