ui = bootstrapPage(fluidPage(
  titlePanel('Survival Assessment for Second Primary NSCLC'),
  # sidebarLayout(sidebarPanel(uiOutput('manySliders'),
  sidebarLayout(sidebarPanel(
    fluidRow(
      column(6, uiOutput('manySliders1'),offset = 0),
      column(6, uiOutput('manySliders2'),offset = 0)
    ),
    actionButton('add', 'Predict'),
    br(), br(),
    helpText('Press Quit to exit the App'),
    actionButton('quit', 'Quit'),
    br(), br(),
    helpText('Designed by Junmin Zhu, 2024')
  ),
  mainPanel(
    # tableOutput("inputDataTable"),
    # tableOutput("inputDataTable2"),
    tabPanel('Information',
             h4("Personalized Survival Prediction")),
    tabPanel('Survival plot', plotOutput('plot')),
    tabPanel('Information',
             h4("Local Interpretability Analysis"),
             p("Red indicates an increased chance of survival, 
             while green indicates a decreased chance of survival.")
    ),
    tabPanel('Explain plot', plotOutput('plot2'))
  ),
  )
)
)