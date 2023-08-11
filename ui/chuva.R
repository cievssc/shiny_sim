   tabItem(tabName = 'chuva',
    h2('Chuvas extremas'),
   
   fluidRow( 
   column(4,
    box(title = "Cenários",
        closable = F,
        collapsible = T,
        width = 12,
        height = "500px",
        solidHeader = FALSE,
        fluidRow(
        column(6,
     #selectInput('municipio_estiagem', label = 'Município:', choices = municipiopoly$Municipio, 
     #selected = municipiopoly$Municipio[1], multiple = F),
     selectInput('retorno_chuva', label = 'Período retorno:', 
     choices = c('2 anos' = 2, '5 anos' = 5, '10 anos' = 10,
                 '20 anos' = 20,'50 anos' = 50,'100 anos' = 100), selected = 20
                 )),
     column(6,
     selectInput("periodo_chuva", label = "Período do ano:",
    choices = list("Verão" = 1, "Outono" = 2, "Inverno" = 3, 'Primavera' = 4),
    selected = 1))
    ), #endrow
     radioButtons("cenario_chuva", label = NULL,
    choices = list('Histórico' = 1,"RCP 4.5" = 2, "RCP 8.5" = 3),
    selected = 1, inline = T),
     
     leafletOutput(outputId = 'mapa_chuva') 
    
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
    selectInput('municipio_chuva', label = 'Município:', choices = municipiopoly$Municipio, 
     selected = 'Florianópolis', multiple = F),
    fluidRow(
    column(7, 
   h4('Série retorno'),
    generalhighOutput('serie_inteira_chuva')),
    column(5,
    h4('Estação selecionada (apenas cenário)'),
    generalhighOutput('serie_estacao_chuva')
    )) #endrow 
      
   ) #endbox
   )
  ), #endrow
  box(
   title = "Indicadores de chuva",
        closable = F,
        width = 12,
        collapsible = T,
        height = "400px",
        solidHeader = FALSE,
   fluidRow(
    column(4,
     h4('Quantidade de dias úmidos, por ano'),
     radioButtons("chuva_mm", label = NULL,
    choices = list('> 1mm' = 1,">= 20mm" = 2),
    selected = 1, inline = T),
     generalhighOutput('dias_umidos')
     ),
    column(3,
    valueBoxOutput('chuva_txt1', width = NULL),
    valueBoxOutput('chuva_txt2', width = NULL),
    valueBoxOutput('chuva_txt3', width = NULL)
    ), 
    column(5,
     h4('Máximo dias úmidos consecutivos (mês)'),
     plotlyOutput('dias_consecutivos_chuva')
     ) 
     
     ), #end row     
    hr(),
   fluidRow(
    column(4,
     h4('Índice de Anomalia de Chuva (IAC)'), 
     generalhighOutput('serie_rai')
    ),
    column(3,
    tags$div(
    style = 'display: inline-block;' ,
    h4('Percentis'), 
    radioButtons("percentis_chuva", label = NULL,
    choices = list('P90' = 1,"P95" = 2, 'P99'= 3),
    selected = 1, inline = T
    )
    ),
    valueBoxOutput('chuva_p1', width = NULL),
    valueBoxOutput('chuva_p2', width = NULL)
    ),
    column(5,
    h5('Precipitação (mm)'),
    generalhighOutput('serie_pmedia'),
    h5('Percentual'),
    generalhighOutput('serie_ppercentual' )
    )
    
    )#end orw    
        ) #endbox
      
     ) #end tabitem