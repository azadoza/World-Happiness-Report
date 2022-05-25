library(shiny)
library(shinydashboard)
library(DT)
ui <- dashboardPage(
    dashboardHeader(),
    title = NULL,
    skin = c("black"),
    dashboardSidebar(
        width = 0
    ),
    dashboardBody(
        box(
            status = "primary",
            title = "COVID-19 Deaths per 100k Population and Initial Index of Exposure in Every Country,\n Separated by Region", 
            height = "595",
            solidHeader = T,
            plotOutput(
                "trace_plot", 
                click = "plot_click"),
                verbatimTextOutput("info"),
            ),
        box( 
            title = "Data Table", 
            status = "primary", 
            height = "700",
            width = "6",
            solidHeader = T, 
            div(
                DT::dataTableOutput("trace_table",
                                    width = 580)
            ))
    ))
server <- function(input, output) 
{ 
    #Plot for Trace Explorer
    output$trace_plot <- renderPlot({
        regionCountryDeath %>% 
            ggplot(
                aes(
                    x=factor(Region),
                    y=Deaths_per_100k, 
                    color = Exposure)
            ) +
            geom_beeswarm(size=7, alpha=.7, cex = .75)+
            scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))+
            labs(title = "",
                 x= "Region",
                 y = "Deaths per 100k")+
            theme(text = element_text(size = 15),
                  legend.position = "bottom",
                  legend.title = element_text(size=14),
                  legend.text = element_text(size = 12),
                  legend.key.width= unit(2, 'cm'))+
            scale_color_viridis(option = "F", begin = 0, end = .8, direction = -1)
    })
    output$trace_table <- renderDataTable({
        regionCountryDeath
    })
    output$info <- renderPrint({
        # With ggplot2, no need to tell it what the x and y variables are.
        # threshold: set max distance, in pixels
        # maxpoints: maximum number of rows to return
        # addDist: add column with distance, in pixels
        nearPoints(regionCountryDeath, input$plot_click, xvar = factor("Region"),
                   yvar="Deaths_per_100k")
    })
}
shinyApp(ui, server)