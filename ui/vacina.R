 tabItem(tabName = 'vacina',
    h2('Vacinação'),
    fluidRow(
       column(7, 
       radioButtons("radio_vacina", label = h5("Período: "),
       choices = list("Todo" = 1, "7 dias" = 2, "14 dias" = 3), 
       selected = 1, inline = T),
          leafletOutput('mapa_vacina', width = '100%', height = '450px'),
          downloadButton('baixar_dado_vacina','Baixar dados')),
       
       column(5,
       #TODO colocar filtros por fabricantes
      # box( title = 'Santa Catarina', width = NULL,
      valueBoxOutput("total_vacina", width = NULL),
       fluidRow(
         column(6,
         valueBoxOutput("total_pessoas", width = NULL),
         valueBoxOutput("total_imunizacao", width = NULL)
         ) ,
         column(6,
         valueBoxOutput("perc_doses", width = NULL),
         valueBoxOutput("perc_imuniczacao", width = NULL)
         )
         )#end Row
         )#) 
         ), #end Row

    fluidRow(
      column(12,
      box(formattableOutput('tabela_sintese_vacina'), width = NULL, title = h3('Tabela síntese')))#DT::dataTableOutput('tabela_sintese'), width = NULL))
         ), #endrow  
    
    fluidRow(
    column(3,
      h3('Gráficos sínteses'),
      radioButtons("radio_perc_vacina", label = h5("Análise: "),
       choices = list("Absoluta" = 1, "Relativa" = 2), 
       selected = 2, inline = T)      
       )
       ), #endrow
     fluidRow(  
      column(5,
      h4('Regiões de saúde'),
      generalhighOutput('imunizacao_reg')
      ),#DT::dataTableOutput('tabela_sintese'), width = NULL))
      column(3,
      h4('Estado'),
      generalhighOutput('imunizacao_estado')
         ),
      column(4,
      h4('Fabricantes'),
      generalhighOutput('fabricante_estado')
         )  
      ), #endrow
     # br(),
     fluidRow(
      column(6,
      generalhighOutput('serie_vac_estado')),
      column(6,
      h5('Idade'),
      generalhighOutput('idade_estado'))
      ), #endrow
      
      br(), 
      fluidRow(
    column(3,
      h3('Regiões'),
      selectInput('reg_graficos_vacina', label = NULL, choices = c(unique(serie_obito$reg_saude)[-17]), 
     selected = 'Alto Uruguai Catarinense', multiple = F))      
       ), #endrow
     fluidRow(  
      column(6,
      h4('Série'),
      generalhighOutput('serie_vac_regiao')
      ),#DT::dataTableOutput('tabela_sintese'), width = NULL))
      column(6,
      h4('Fabricante'),
      generalhighOutput('fabricante_reg')
         )  
      ), #endrow
     fluidRow(  
      column(6,
      h5('Série fabricantes'),
      generalhighOutput('serie_fab_regiao')
      ),
       column(6,
      h5('Idade'),
      generalhighOutput('idade_regiao'))
         ) #endrow  
      
      
      )#end tabitem 
      