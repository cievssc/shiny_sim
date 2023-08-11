 tabItem(tabName = 'forecasting',
    h2('Previsões'),
    fluidRow(
       column(7, 
       selectInput("regiao_forecasting", label = h5("Região da saúde: "),
       choices = c('Estado',reg_pop[,1]), 
       selected = 1),
          generalhighOutput('forecasting_all')),
          #downloadButton('baixar_dado_vacina','Baixar dados')),
       
       column(5,
       #TODO colocar filtros por fabricantes
      # box( title = 'Santa Catarina', width = NULL,
      valueBoxOutput("total_previsao", width = NULL),
       fluidRow(
         column(6,
         valueBoxOutput("total_nowcasting", width = NULL),
         valueBoxOutput("total_forecasting", width = NULL)
         ) ,
         column(6,
         valueBoxOutput("obs_nowcasting", width = NULL),
         valueBoxOutput("obs_forecasting", width = NULL)
         )
         )#end Row
         )#) 
         ) #end Row

      
      
      )#end tabitem 
      