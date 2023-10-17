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
  
  #TODO melhorar a reatividade do dados_all_mort() - 17-*out-2023
  #teste <- reactive({list(input$current_tab == 'mortalidade', input$head_atualizar)})
  #output$testei <- renderPrint({ head(dados_analise_mort())})

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
                   #add em 17-out-23
                   dadoi$ano <- with(dadoi, substr(dat_obito,1,4)) %>% as.numeric
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
      pops <- left_join(, tab_regioes[,c(3:5)], by = c('cod6'))
      pops(pops)
  }, ignoreInit = T)#ignoreNULL = F)
  
  #------------------------------------------------------------------------
  #dados necessários (17-ou-2023, 1339h)
  
  

  #========================================================================                      
  #cards
  
  #card total 
  mod_summary_card_server('mort_total', 
                            card_large(heading =  'Taxa de mortalidade no período',
                              uiOutput('mort_total_card') %>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$mort_total_card <- renderUI({
                    dadoi <- dados_analise_mort()
                    pops <- aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((sum(table(dadoi$ano))/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                    )
                    })
                    
                    
  #card dcnt
  mod_summary_card_server('mort_total_dcnt', 
                            card_large(heading =  'DCNT´s',
                              uiOutput('mort_totalcard_dcnt')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$mort_totalcard_dcnt <- renderUI({
                    dadoi <- dados_analise_mort()
                    dadoi <- dadoi[which(dadoi$dcnt == T),]
                    pops <- aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((sum(table(dadoi$ano))/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    )
                    })
  
  #card suicídio
  mod_summary_card_server('mort_total_suic', 
                            card_large(heading =  'Suicídio',
                              uiOutput('mort_totalcard_suic')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$mort_totalcard_suic <- renderUI({
                    dadoi <- dados_analise_mort()
                    dadoi <- dadoi[which(dadoi$suicidio == T),]
                    pops <- aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((sum(table(dadoi$ano))/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi[which.max(dadoi[,2]),1])
                    )
                    })
 
 #veiculo terrestre
  mod_summary_card_server('mort_total_acid', 
                            card_large(heading =  'Acidente com veículos terrestres',
                              uiOutput('mort_totalcard_acid')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$mort_totalcard_acid <- renderUI({
                    dadoi <- dados_analise_mort()
                    dadoi <- dadoi[!is.na(dadoi$tipo_transito),]
                    pops <- aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((sum(table(dadoi$ano))/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi[which.max(dadoi[,2]),1])
                    )
                    })
 
 #afogamento (add 20-jun-2023, 21:48h)
  mod_summary_card_server('mort_total_afog', 
                            card_large(heading =  'Afogamentos',
                              uiOutput('mort_totalcard_afog')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$mort_totalcard_afog <- renderUI({
                    dadoi <- dados_analise_mort()
                    dadoi <- dadoi[which(dadoi$afogamento == T),]
                    pops <- aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((sum(table(dadoi$ano))/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi[which.max(dadoi[,2]),1])
                    )
                    }) 
 

 #============================================================================
  #mapa
  mod_summary_card_server('mort_mapa', 
                   card_large(heading = tagList(h1('Mapa')),
                      leafletOutput('mort_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
                             
                             
  output$mort_mapa_leaflet <- renderLeaflet({ mort_leaflet_data()
        })
        
  mort_leaflet_data <- reactive({
  
   dadoi <- dados_analise_mort()
   dadoi <- lapply(split(dadoi$ano), function)
   
   mapa_dado <- as.data.frame(table(dadoi$reg_saude))     
   mapa_dado <- left_join(mapa_regionais, mapa_dado, by = c('reg_saude' = 'Var1'))
   
   fill_color <- function(x){
                   bins <- unique(as.vector(round(quantile(x, probs = c(0,0.30,0.50,0.7,0.85,0.95,0.98,1),na.rm = T))))
                   pal <- colorBin("YlOrRd", domain = x, bins = bins)
                   colorData <- pal(x)
                   list(pal, colorData)
                          }
   
   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s" , #  people / mi<sup>2</sup>",
 mapa_dado$reg_saude, 'Qtde óbitos: ', mapa_dado$Freq) %>% lapply(htmltools::HTML)
         }

   leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lat = -27.5, lng = -51, zoom = 7)  %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor =  fill_color(mapa_dado$Freq)[[2]], 
        stroke = T, smoothFactor = 0.5, fillOpacity = 0.8, weight = 1.5,
    highlight = highlightOptions(
    weight = 5,
    color = "#666",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labells(mapa_dado),
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "12px",
    maxWidth = '200px',
    direction = "auto")) %>% 
    addLegend(pal = fill_color(mapa_dado$Freq)[[1]], values = fill_color(mapa_dado$Freq)[[2]], opacity = 0.7, title = 'Qtde. de Óbitos',
  position = "bottomright", layerId="colorLegend2",className = 'info legenda')
                      
  }) #reactive                       
  