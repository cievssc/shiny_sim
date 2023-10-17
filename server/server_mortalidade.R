  #server para a aba mortalidade (22-set-2023, 15:41h)
 
   output$mort_dropopcoes <- renderUI(
                               tagList(
                                  selectInput('mort_tipo_obito', label = h5('Categoria óbito'),
                                      choices = c('Todos','Todos DCNT','Diabetes mellitus', 'Doenças cardiovasculares',
                                      'Doenças respiratórias','Neoplasias', 'Suicídio', 'Acidente V.terrestres', 'Afogamento'), selected = 'Todos' ,
                                      multiple = F),
                                  selectInput('mort_tipo_sexo', label = h5('Sexo'),
                                      choices = c('Masculino', 'Feminino'), selected = c('Masculino', 'Feminino'),
                                      multiple = T),     
                                  sliderInput('mort_tipo_idade', label = 'Idade:', 
                                  min = 0, max = 150, value = c(0,150))
                                 #  br(),     
                                # actionButton('mort_atualizar', 'Atualizar') 
                                       )      
                                     )


  #organizando os dados
  dados_all_mort <- reactiveVal(NULL)
  dados_analise_mort <- reactiveVal(NULL)
  pops <- reactiveVal()
  
  teste <- reactive({list(input$current_tab == 'mortalidade', input$head_atualizar)})

  observeEvent({input$current_tab =='mortalidade' |  input$head_atualizar},{ #teste(),{ #
                 #if(input$current_tab != 'mortalidade'){return()}
                 # req(input$home_dateyear)
                 if(is.null(input$home_dateyear)){
                  anos <- c((as.numeric(format(Sys.Date(), '%Y'))-2):(as.numeric(format(Sys.Date(), '%Y'))))
                 }else{anos <- input$home_dateyear}

                 if(input$head_municipio != 'Todos'){
                                lista_mun <- municipios_br[which(municipios_br$uf == 'Santa Catarina'),c(3,4)]
                                lista_mun <- floor(municipios_br$codigo[municipios_br$municipio %in% input$head_municipio]/10)
                 query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT ",paste(lista_sim, collapse = ', ')," , substr(cod_municipio_ibge_residencia,1,2) AS 'cod_uf_resid'
                                   FROM fat_declaracao_obito_sim
                                   WHERE cod_municipio_ibge_residencia IN (\'",paste(lista_mun, collapse = "\',\'"),"\') AND
                                   num_ano_obito IN ( ?code1 )
                                   "),
                                   code1 =  SQL(toString(sprintf("'%s'",  anos)))
                                   ) }else{              
                 query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT ",paste(lista_sim, collapse = ', ')," , substr(cod_municipio_ibge_residencia,1,2) AS 'cod_uf_resid'
                                    FROM fat_declaracao_obito_sim
                                   WHERE num_ano_obito IN (?code1)
                                   "),
                                   code1 =  SQL(toString(sprintf("'%s'",  anos)))
                                   ) }
                 dadoi <- DBI::dbGetQuery(conn(), query)
                    
                  if(nrow(dadoi) == 0){
                  showModal(modalDialog(
                  title = NULL,
                  tagList(
                   p("Não há registros no período.")),
                   easyClose = TRUE,
                   footer = NULL
                   ))
                   NULL
                   }else{
                   dadoi <- dadoi[dadoi$cod_uf_resid == '42' ,]
                   dadoi <- func_sim(dadoi)
                   dadoi <- dadoi[!is.na(dadoi$cod_tipo_idade),]
                   dadoi$cod_municipio_ibge_residencia %<>% as.numeric
                   dadoi$dat_obito %<>% as.Date
                   dadoi <- dplyr::arrange(dadoi, dat_obito)
                   dadoi$semana_epid <- with(dadoi, paste0(epiweek(dat_obito),'.',epiyear(dat_obito))) %>% factor(., levels = unique(.))
                   dadoi <- left_join(dadoi, tab_regioes[,c(3:5)], by = c('cod_municipio_ibge_residencia' = 'cod6'))
                   dados_all_mort(dadoi)}
                   #dadoi}
                   }, ignoreNULL = F) #end observeEvent ignoreNULL = F
  

  observeEvent(c( input$head_atualizar),{# input$mort_atualizar,
                     req(!is.null(dados_all_mort()))
                     dadoi <- dados_all_mort()
                     if(input$mort_dropdown >0){
                     if(input$mort_tipo_obito %in% c('Diabetes mellitus', 'Doenças cardiovasculares',
                                      'Doenças respiratórias','Neoplasias')){
                        dadoi <- dadoi[dadoi$tipodcnt %in% input$mort_tipo_obito,]
                        }
                     if(input$mort_tipo_obito == 'Todos DCNT'){
                        dadoi <- dadoi[dadoi$dcnt == T,]
                        }
                     if(input$mort_tipo_obito == 'Suicídio'){
                        dadoi <- dadoi[dadoi$suicidio == T,]
                        }
                     if(input$mort_tipo_obito == 'Acidente V.terrestres'){
                        dadoi <- dadoi[!is.na(dadoi$tipo_transito),]
                        }
                     if(input$mort_tipo_obito == 'Afogamento'){
                        dadoi <- dadoi[dadoi$afogamento == T,]
                        }      
                     if((input$mort_tipo_sexo == 'Masculino') & length(input$mort_tipo_sexo) == 1){
                        dadoi <- dadoi[dadoi$sgl_sexo == 'M',]
                        }
                     if((input$mort_tipo_sexo == 'Feminino')  & length(input$mort_tipo_sexo) == 1){
                        dadoi <- dadoi[dadoi$sgl_sexo == 'F',]
                        }
                     if(!is.null(input$mort_tipo_idade)){
                        sequencia <- seq(input$mort_tipo_idade[1],input$mort_tipo_idade[2], by = 1)
                        dadoi <- dadoi[as.numeric(dadoi$num_idade) %in% sequencia, ] 
                        }      
                        }
                   
                     
                    if(nrow(dadoi) == 0){
                  showModal(modalDialog(
                  title = NULL,
                  tagList(
                   p("Não há registros no período.")),
                   easyClose = TRUE,
                   footer = NULL
                   ))
                   NULL
                   }else{

                   dados_analise_mort(dadoi)
                   }     
                          })
  
  #população (add em 16-out-2023, 1525h)

  observeEvent(list(input$head_atualizar, input$home_dateyear),{
      if(input$current_tab != 'mortalidade'){return()}
      pops <- pop_all[pop_all$ano %in% as.numeric(input$home_dateyear),-2]
      pops(pops)
  }, ignoreNULL = F)
  

 
  output$testei <- renderPrint({ head(dados_analise_mort())
                                   })
  
  