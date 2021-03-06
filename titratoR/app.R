
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
    
    #Title
    dashboardHeader(title = "titratoR"),
    
    # Sidebar
    sidebar_fullCollapse = TRUE,
    dashboardSidebar(
        column(12,h3('Solution titrante')),
        sliderInput(inputId = 'conc.titrant',
                    label = 'Concentration (M)',
                    value = 0.1,
                    min = 0,
                    max = 2,
                    step = 0.025),
        hr(),
        column(12,h3('Solution titrée')),
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
            column(12,
                   switchInput(inputId = 'ab',
                               label = 'A/B ?',
                               onLabel = 'Acide',
                               offLabel = 'Base',
                               onStatus = 'danger',
                               offStatus = 'info',
                               size = 'normal')
            ),
            column(12,
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
        column(12,hr()),
        column(12,h3('Expérience')),
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
    # Body
    dashboardBody(
        fluidRow(
            column(12,
                   boxPlus(id = 'table',
                           title = 'Table',
                           collapsible = T,
                           collapsed = F,
                           width = 12,
                           DT::DTOutput('reac')   
                   )
            )
        ),
        fluidRow(
            column(12,
                   boxPlus(id = 'pH',
                           title = 'pH',
                           collapsible = T,
                           collapsed = F,
                           width = 6,
                           plotOutput("p.ph")
                   ),
                   boxPlus(id = 'n',
                           title = 'Quantités de matière',
                           collapsible = T,
                           collapsed = F,
                           width = 6,
                           plotOutput("p.n"))
            )
        )
    )
)



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
    
    output$reac <- DT::renderDT(server = FALSE, {
        reac() %>%
            select(-8) %>%
            magrittr::set_colnames(c('V titrant (mL)', 'V eq (mL)', 'V total (mL)',
                                     'n titrant total (mmol)', 'n titrant (mmol)', 'n titré (mmol)', 'n conjugué (mmol)',
                                     'pH')) %>%
            DT::datatable(
                extensions = c('Buttons', 'Responsive', 'Scroller'),
                rownames = F,
                escape = F, #need to be false to get hyperlink of DOI parsed
                filter = 'top',
                autoHideNavigation = T,
                options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = F,
                    pageLength = 25,
                    autoWidth = F,
                    dom = 'Bfrtip', #button position
                    buttons = c('copy', 'csv', 'excel', 'colvis') #buttons
                ) 
            )%>%
            formatRound(c(1:8),
                        digits = 2)
    })
    
    #Plots----
    p.ph <- reactive({
        ggplot(data = reac(), aes(x = vol.titrant, y = pH, colour = pH)) +
            geom_point(size = 2.5) +
            scale_color_gradient(low = 'tomato', high = 'steelblue') +
            geom_vline(xintercept = unique(reac()$vol.eq),
                       linetype = 'dashed',
                       color = 'forest green',
                       show.legend = FALSE) +
            theme_pander() +
            theme(legend.position = 'none') +
            xlab('Volume titrant (mL)')
    }) 
    
    p.n <-reactive({
        reac() %>%
            pivot_longer(cols = c(n.titrant, n.titrant.total, n.titrated, n.conj),
                         names_to = "species",
                         values_to = "n") %>%
            ggplot(aes(x = vol.titrant, y = n, color = species)) +
            geom_point(size = 2.5) +
            theme_pander() +
            theme(legend.position = 'bottom') +
            scale_colour_d3(labels = c('Conjuguée (A/B faibles)', 'Titrante', 'Titrante (total ajouté)', 'Titrée')) +
            labs(colour = 'Espèces') +
            xlab('Volume titrant (mL)') +
            ylab('Quantité de matière (mmol)')
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
