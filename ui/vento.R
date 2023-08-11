 #aba de ventos (11-nov-2021, 09:31h)
   tabItem(tabName = 'vento',
    h2('Ventos extremos'),
   
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
     selectInput('retorno_vento', label = 'Período retorno:', 
     choices = c('2 anos' = 2, '5 anos' = 5, '10 anos' = 10,
                 '20 anos' = 20,'50 anos' = 50,'100 anos' = 100), selected = 20
                 )),
     column(6,
     selectInput("periodo_vento", label = "Período do ano:",
    choices = list("Verão" = 1, "Outono" = 2, "Inverno" = 3, 'Primavera' = 4),
    selected = 1))
    ), #endrow
     radioButtons("cenario_vento", label = NULL,
    choices = list('Histórico' = 1,"RCP 4.5" = 2, "RCP 8.5" = 3),
    selected = 1, inline = T),
     
     leafletOutput(outputId = 'mapa_vento') 
    
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
    selectInput('municipio_vento', label = 'Município:', choices = municipiopoly$Municipio, 
     selected = 'Florianópolis', multiple = F),
    fluidRow(
    column(7, 
   h4('Série retorno'),
    generalhighOutput('serie_inteira_vento')),
    column(5,
    h4('Estação selecionada (apenas cenário)'),
    generalhighOutput('serie_estacao_vento')
    )) #endrow 
      
   ) #endbox
   )
  ) #endrow

      
     ) #end tabitem