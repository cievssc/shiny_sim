 tabItem(tabName = 'estiagem',
    h2('Estiagem'),
     
    verbatimTextOutput('textoteste'), 
   fluidRow( 
   column(4,
    box(title = "Diferenças cenários",
        closable = F,
        collapsible = T,
        width = 12,
        height = "500px",
        solidHeader = FALSE,
        fluidRow(
        column(6,
     #selectInput('municipio_estiagem', label = 'Município:', choices = municipiopoly$Municipio, 
     #selected = municipiopoly$Municipio[1], multiple = F),
     selectInput('variavel_estiagem', label = 'Indicador:', 
     choices = c('Temperatura' = 1,'Precipitação' = 2, 'Umidade do solo' = 3))),
     column(6,
     selectInput("periodo_estiagem", label = "Período do ano:",
    choices = list("Verão" = 1, "Outono" = 2, "Inverno" = 3, 'Primavera' = 4),
    selected = 1))
    ), #endrow
     radioButtons("cenario_estiagem", label = NULL,
    choices = list("RCP 4.5" = 1, "RCP 8.5" = 2),
    selected = 1, inline = T),
     
     leafletOutput(outputId = 'mapa_estiagem') 
    
    ) #endbox
    ),
  column(8,
   box(
   title = "Séries",
        closable = F,
        width = 12,
        collapsible = T,
        height = "400px",
        solidHeader = FALSE,
    selectInput('municipio_estiagem', label = 'Município:', choices = municipiopoly$Municipio, 
     selected = 'Florianópolis', multiple = F),
    fluidRow(
    column(7, 
   h4('Série temporal'),
    generalhighOutput('serie_inteira_estiagem')),
    column(5,
    h4('Estação selecionada (apenas cenário)'),
    generalhighOutput('serie_estacao_estiagem')
    )) #endrow 
      
   ) #endbox
   )
  ), #endrow
  box(
   title = "Indicadores de estiagem",
        closable = F,
        width = 12,
        collapsible = T,
        height = "400px",
        solidHeader = FALSE,
   fluidRow(
    column(4,
     h4('Quantidade de dias secos (< 1mm), por ano'),
     generalhighOutput('dias_secos')
     ),
    column(3,
    valueBoxOutput('estiagem_txt1', width = NULL),
    valueBoxOutput('estiagem_txt2', width = NULL),
    valueBoxOutput('estiagem_txt3', width = NULL)
    ), 
    column(5,
     h4('Máximo dias secos consecutivos (mês)'),
     plotlyOutput('dias_consecutivos')
     ) 
     
     ), #end row     
    hr(),
   fluidRow(
    column(6,
     h4('Índice Padronizado de Precipitação'), 
     generalhighOutput('serie_spi')
    ),
    column(6,
     h4('Índice de Severidade de Seca de Palmer'), 
     generalhighOutput('serie_palmer')
    )
    
    )#end orw    
        ) #endbox
      
     ) #end tabitem