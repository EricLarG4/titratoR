
library(shiny)
library(shinydashboardPlus)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("TitratoR"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3('Solution titrante'),
            sliderInput(inputId = 'conc.titrant',
                        label = 'Concentration (M)',
                        value = 0.1,
                        min = 0,
                        max = 2,
                        step = 0.025),
            hr(),
            h3('Solution titrée'),
            sliderInput(inputId = 'conc.titrated',
                        label = 'Concentration (M)',
                        value = 0.05,
                        min = 0,
                        max = 2,
                        step = 0.025),
            sliderInput(inputId = 'vol.titrated',
                        label = 'Volume (mL)',
                        value = 25,
                        min = 0,
                        max = 250,
                        step = 5),
            fluidRow(
                column(6,
                       switchInput(inputId = 'ab',
                                   label = 'A/B ?',
                                   onLabel = 'Acide',
                                   offLabel = 'Base',
                                   onStatus = 'danger',
                                   offStatus = 'info',
                                   size = 'normal')
                ),
                column(6,
                       switchInput(inputId = 'strength',
                                   label = 'Force ?',
                                   onLabel = 'Fort',
                                   offLabel = 'Faible',
                                   onStatus = 'danger',
                                   offStatus = 'info',
                                   size = 'normal')
                )
            ),
            sliderInput(inputId = 'pka',
                        label = 'pKa',
                        value = 7,
                        min = 0,
                        max = 14,
                        step = 0.01),
            hr(),
            h3('Expérience'),
            sliderInput(inputId = 'burette',
                        label = 'Volume total ajouté (mL)',
                        value = 25,
                        min = 5,
                        max = 250,
                        step = 5),
            sliderInput(inputId = 'grad',
                        label = 'Volume goutte (mL)',
                        value = 0.5,
                        min = 0.1,
                        max = 5,
                        step = 0.1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            DT::DTOutput('reac'),
            plotOutput("p.ph"),
            plotOutput("p.n")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #libraries----
    library(tidyverse)
    library(ggthemes)
    library(ggsci)
    
    #funtions----
    source("ph_functions.R")
    
    
    #Input----
    
    strength <- reactive({
        if(!isTRUE(input$strength)){
            return('weak')
        } else {return('strong')}
    })
    
    ab <- reactive({
        if(isTRUE(input$ab)){
            return('acid')
        } else {return('base')}
    })
    
    #Calculations----
    
    reac <- reactive({
        reac.table(conc.titrant = input$conc.titrant,
                   conc.titrated = input$conc.titrated, vol.titrated = input$vol.titrated, 
                   ab = ab(), strength = strength(), pka = input$pka,
                   burette = input$burette, grad = input$grad)
    }) 
    
    output$reac <- DT::renderDT(server = FALSE, {reac()})
    
    #Plots----
    p.ph <- reactive({
        ggplot(data = reac(), aes(x = vol.titrant, y = pH, colour = pH)) +
            geom_point() +
            theme_pander() +
            theme(legend.position = 'none') 
    }) 
    
    p.n <-reactive({
        reac() %>%
            pivot_longer(cols = c(n.titrant, n.titrant.total, n.titrated, n.conj),
                         names_to = "species",
                         values_to = "n") %>%
            ggplot(aes(x = vol.titrant, y = n, color = species)) +
            geom_point() +
            theme_pander() +
            scale_colour_d3(labels = c('Conjuguée (A/B faibles)', 'Titrante', 'Titrante (total ajoutée)', 'Titrée')) +
            labs(colour = 'Espèces') 
    }) 
    
    
    output$p.ph <- renderPlot({
        p.ph()
    })
    
    output$p.n <- renderPlot({
        p.n()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
