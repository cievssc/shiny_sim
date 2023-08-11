 tabItem(tabName = 'geral',
    h2('Panorama geral'),
   fluidRow( 
   column(2,
   tags$div(class = 'ferramenta',
    radioButtons("periodo", label = h4("Período do ano:"),
    choices = list("Verão" = 1, "Outono" = 2, "Inverno" = 3, 'Primavera' = 4),
    selected = 1), #end radiobuttons
    selectInput("variavelI", label = h4("Variável:"),
    #ir adicionando nas análises...
    choices = list( #"Temperatura Máxima" = 2,
                   'Médias' = list(
                   "Temperatura Média" = 3, 
                   "Precipitação Acumulada Média" = 4, 
                   'Umidade do solo' = 5),
                    'Eventos extremos' = list(
                    'Ocorrência de ventos' = 6,
                    'Chuvas (percentil 99)' = 7)
                   ),
    selected = 4), #end radiobuttons
    sliderInput("decimal", "Transparência:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
    hr(),
    
     radioButtons("modeloI", label = h4("Modelo:"),
       choices = list("RCP 4.5" = 2, "RCP 8.5" = 3),
       selected = 2),
    hr(),  
   
   # checkboxInput("checkbox", label = "Valores em diferença", value = FALSE), 
   #             tags$span( style = "display: 'inline'",
    #     actionButton('modal_report','Sobre os dados.')
   #      ),
                p('Fonte: GIZ/INPE (2019)')
   ) #enddiv
   ), #column
  column(10,
   leafletOutput(outputId = 'map', width = '100%',height="500px"),
   fluidRow(
   column(6,
   #h3('Período 1981-2010', style = 'float: left'),
   shinydashboard::valueBox("Cenário Base", "1981-2010", icon = icon("hourglass"), color = 'yellow', width = NULL)),
    column(6,
   valueBoxOutput('cenario_txt', width = NULL)
   )
   )
   #h3('Saída do Modelo', style = 'float: right'))
            
  )#end column
  
  ) #endrow
      
     ) #end tabitem