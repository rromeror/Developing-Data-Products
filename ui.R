library(shiny)
shinyUI(pageWithSidebar(
        
        headerPanel("Internet tariff calculator"),
        sidebarPanel(
                numericInput('Internet_speed', 'Max Internet speed [Mbps]', 20, min = 3, max = 50, step = 1),
                sliderInput('Mean', 'Mean bit rate [Mbps]',value = 10, min = 0, max = 50, step = 0.0025,)
                
        ),
        mainPanel(
                
                h4('Results - Charge'),
                verbatimTextOutput("results1"),
                h4('Results - Internet connection statistics'),
                verbatimTextOutput("results2"),
                plotOutput('my_charge'),
                p(em("Documentation:",a("Project",href="Help.html")))
        )
))


