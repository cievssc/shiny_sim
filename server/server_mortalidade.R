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
                                  selectInput('mort_tipo_idade', label = 'Idade:',
                                  choices = levels(pop_all$variable), selected = levels(pop_all$variable), multiple = T)
                                       )      
                                     )


  #organizando os dados
  dados_all_mort <- reactiveVal(NULL)
  dados_analise_mort <- reactiveVal(NULL)
  pops <- reactiveVal()
  
  #teste <- reactive({list(input$current_tab == 'mortalidade', input$head_atualizar)})
  #output$testei <- renderPrint({head(dados_analise_mort())})

  observeEvent({input$current_tab =='mortalidade' |  input$head_atualizar},{ #teste(),{ #
                 if(input$current_tab != 'mortalidade'){return()}
                 # req(input$home_dateyear)
                 if(is.null(input$home_dateyear)){
                  anos <- c((as.numeric(format(Sys.Date(), '%Y'))-2):(as.numeric(format(Sys.Date(), '%Y'))))
                 }else{anos <- input$home_dateyear}

                 if(any(input$head_municipio != 'Todos')){
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
                   dadoi$faixa_idade <- cut(dadoi$num_idade, breaks = c(-Inf,5,10,15,20,30,40,50,60,70,80, Inf), right = F, include.lowest = T,
                                                labels = levels(pop_all$variable))
                   dados_all_mort(dadoi)}
                   #dadoi}
                   }, ignoreNULL = F) #end observeEvent ignoreNULL = F
  

  observeEvent({input$current_tab =='mortalidade' |  input$head_atualizar},{# input$mort_atualizar,
                     req(!is.null(dados_all_mort()))
                     if(input$current_tab != 'mortalidade'){return()}
                     dadoi <- dados_all_mort()
                     pops <- pops()
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
                     if((input$mort_tipo_sexo[1] == 'Masculino') & length(input$mort_tipo_sexo) == 1){
                        dadoi <- dadoi[dadoi$sgl_sexo == 'M',]
                        pops <- pops[pops$sexo == 'Masculino',]
                        }
                     if((input$mort_tipo_sexo[1] == 'Feminino')  & length(input$mort_tipo_sexo) == 1){
                        dadoi <- dadoi[dadoi$sgl_sexo == 'F',]
                        pops <- pops[pops$sexo == 'Feminino',]
                        }
                     if(!is.null(input$mort_tipo_idade)){
                        dadoi <- dadoi[dadoi$faixa_idade %in% input$mort_tipo_idade, ]
                        pops <- pops[pops$variable %in% input$mort_tipo_idade, ]
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
                   pops(pops)
                   }     
                          })
  
  #população (add em 16-out-2023, 1525h)

  observeEvent({input$current_tab =='mortalidade' |  input$head_atualizar},{
      if(input$current_tab != 'mortalidade'){return()}
      if(is.null(input$home_dateyear)){
                  anos <- c((as.numeric(format(Sys.Date(), '%Y'))-2):(as.numeric(format(Sys.Date(), '%Y'))))
                 }else{anos <- input$home_dateyear}
      pops <- pop_all[pop_all$ano %in% anos,]
      pops <- left_join(pops, tab_regioes[,c(3:5)], by = c('cod6'))
      if(any(input$head_municipio != 'Todos')){
      pops <- pops[which(pops$municipio %in% input$head_municipio),]}
      pops(pops)
  }, ignoreNULL = F)
  
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
                    pops <- pops()#aggregate(value ~ano, data = pops(), FUN = sum)
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
                    pops <- pops()#aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((nrow(dadoi)/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
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
                    pops <- pops()#aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((nrow(dadoi)/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
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
                    pops <- pops()#aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((nrow(dadoi)/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
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
                    pops <- pops()#aggregate(value ~ano, data = pops(), FUN = sum)
                    dadoi <- round((nrow(dadoi)/sum(pops$value))*1E5,2)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
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
  
  #dados_analise_mort
   dadoi <- dados_analise_mort()
   dadoi$total <-  1
   dadoi <- aggregate(total ~ cod_municipio_ibge_residencia, data = dadoi, FUN = sum)
   
   #pop_all
   pops <- pops()
   pops <- aggregate(value ~ cod6, data = pops, FUN = sum)

   dadoi <- left_join(dadoi, pops, by = c('cod_municipio_ibge_residencia' = 'cod6'))
   dadoi$mortalidade <- with(dadoi, round((total*1E5)/value,2))

   mapa_dado <- left_join(municipiosf, dadoi, by = c('cod6' = 'cod_municipio_ibge_residencia'))
   
   fill_color <- function(x){
                   bins <- unique(as.vector(round(quantile(x, probs = c(0,0.30,0.50,0.7,0.85,0.95,0.98,1),na.rm = T))))
                   pal <- colorBin("YlOrRd", domain = x, bins = bins)
                   colorData <- pal(x)
                   list(pal, colorData)
                          }
   
   labells <- function(x){
             mapa_dado <- x
     sprintf(
  "<strong>%s</strong><br/> %s %s <br/> %s: %s" , #  people / mi<sup>2</sup>",
 mapa_dado$Municipio, 'Qtde óbitos: ', mapa_dado$total, 'Tx. Mortalidade', mapa_dado$mortalidade) %>% lapply(htmltools::HTML)
         }

   leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lat = -27.5, lng = -51, zoom = 7)  %>% clearControls() %>% clearShapes() %>%
        addPolygons(data = mapa_dado,  color = "#444444", fillColor =  fill_color(mapa_dado$mortalidade)[[2]], 
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
    addLegend(pal = fill_color(mapa_dado$mortalidade)[[1]], values = fill_color(mapa_dado$mortalidade)[[2]], opacity = 0.7, title = 'Tx. Mortalidade',
  position = "bottomright", layerId="colorLegend2",className = 'info legenda')
                      
  }) #reactive                       


 #============================================================================
 #gráficos
 #card serie óbitos
   mod_summary_card_server('mort_serie', 
                            tagList(
                     div(class = 'card',
                       div(class = 'card-header', style = 'display:flex;   justify-content:space-between;',
                           h1(class = 'card-title', 'Série Tx. Mortalidade')), 
                           
                            div(class = 'body',
                      apexchartOutput('long_mortserie_chart', height = '500px') %>% withSpinner(color="#0dc5c1"))
                             ) #end card
                            )) #end 
  
  


    output$long_mortserie_chart <- renderApex({
                           dadoi <- dados_analise_mort()
                           dadoi <- with(dadoi, as.data.frame(table(ano), stringsAsFactors = F))

                           pops <- pops()
                           pops <- aggregate(value ~ano, data = pops, FUN = sum)

                           dadoi$mortalidade <- round(dadoi$Freq*1E5/pops$value, 2)
                           
                       list(series = list(list(name = 'Tx.Mortalidade',data = dadoi[,3])
                                              ),
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = TRUE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = dadoi$ano
                                                      ),
                                            
                                              plotOptions = list(
                                                        bar = list(
                                                          horizontal = FALSE,
                                                          dataLabels = list(
                                                             total = list(
                                                                enabled = F,
                                                                style = list(
                                                                    fontSize = '13px',
                                                                    fontWeight = 900)
                                                                )
                                                             )
                                                          )
                                                          ),
                                              legend = c(show = TRUE)
                                              )
                                     })  #end renderapex                       
                        
 
 #sexo ----------------------------
 mod_summary_card_server('mort_sexo', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Sexo')),
                            div(class = 'body',
                      apexchartOutput('mort_sexo_chart', height = '250px')))
                             ) #end taglist
                             )
 
 output$mort_sexo_chart <- renderApex({
                           dadoi  <- dados_analise_mort()
                           dadoi <- dadoi[which(dadoi$dsc_sexo != 'Ignorado'),]
                           dadoi$dsc_sexo <- factor(dadoi$dsc_sexo, levels = c('Masculino','Feminino'))
                           dadoi$total <- 1
                           dadoi <- aggregate(total ~ ano + dsc_sexo, data = dadoi, FUN = sum) 
                           #dadoi <- tidyr::spread(dadoi, key = dsc_sexo, value = total)

                           pops <- pops()
                           pops <- aggregate(value ~ ano + sexo, data = pops, FUN = sum)

                           dadoi <- left_join(dadoi, pops, by = c('ano', 'dsc_sexo' = 'sexo'))
                           dadoi$mortalidade <- with(dadoi, round((total*1E5)/value,2))

                           dadoi <- tidyr::spread(dadoi[,c(1,2,5)], key = dsc_sexo, value = mortalidade)

                           if(ncol(dadoi) == 2){lista <- list(list(name = input$home_tipo_sexo[1], data = dadoi[,2]))}else{
                           lista <- list(list(name = 'Masculino',data = dadoi[,3]),
                                              list(name = 'Feminino',data = dadoi[,2]))}

                          list(series = lista,
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = FALSE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = dadoi$ano
                                                      ),
                                            
                                              plotOptions = list(
                                                        bar = list(
                                                          horizontal = FALSE,
                                                          dataLabels = list(
                                                             total = list(
                                                                enabled = F,
                                                                style = list(
                                                                    fontSize = '13px',
                                                                    fontWeight = 900)
                                                                )
                                                             )
                                                          )
                                                          ),
                                              legend = c(show = TRUE)
                                              )
                             
                                     })  #end renderapex 

   #pirâmide etária
 mod_summary_card_server('mort_idade', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Idade')),
                            div(class = 'body',
                      apexchartbarOutput('mort_idadesexo_chart', height = '250px')))
                             ) #end taglist
                             )
 
 output$mort_idadesexo_chart <- renderApex({
                          dadoi <- dados_analise_mort()
                          pops <- pops()

                          dadoi <- dadoi[which(dadoi$dsc_sexo != 'Ignorado'),]
                           dadoi$dsc_sexo <- factor(dadoi$dsc_sexo, levels = c('Masculino','Feminino'))
                          dadoi$faixa_idade <- cut(dadoi$num_idade, breaks = c(-Inf,5,10,15,20,30,40,50,60,70,80, Inf), right = F, include.lowest = T,
                                                labels = levels(pops$variable))
                          
                          dadoi$total <- 1
                          dadoi <- aggregate(total ~  faixa_idade + dsc_sexo, data = dadoi, FUN = sum)#with(dadoi,as.data.frame(table(faixa_idade, dsc_sexo)))
                          pops <- aggregate(value ~ variable + sexo, data = pops, FUN = sum)

                          dadoi <- left_join(dadoi, pops, by = c('faixa_idade' = 'variable','dsc_sexo' = 'sexo'))
                          dadoi$mortalidade <- with(dadoi, round((total*1E5)/value,2))


                           dadoi <- tidyr::spread(dadoi[,c(1,2,5)], value = mortalidade, key = dsc_sexo)
                           if(ncol(dadoi) == 2){lista <- list(list(name = input$mort_tipo_sexo[1], data = dadoi[,2]))}else{
                           lista <- list(list(name = 'Homem',data = dadoi[,'Masculino']*(-1)),
                                          list(name = 'Mulher',data = dadoi[,'Feminino']))}
                                          
                           list(series = lista,
                                              chart = list(type = 'bar', 
                                                       #toolbar = c(show = FALSE),
                                                       height = '100%',
                                                       stacked = T),
                                              #colors = c('#008FFB', '#FF4560'),
                                              dataLabels = c(enabled = FALSE),
                                              plotOptions = list(bar = list(horizontal = T,
                                                                       barheight = '80%')),
                                              xaxis = list(#labels = c(show = FALSE),
                                                      categories =dadoi[,1]
                                                      ),
                                              
                                              grid = list(
                                                          xaxis = list(lines = c(show = FALSE))),
                                              legend = c(show = T)
                                              )
                                     })  #end renderapex


#==================================================
  #tabelas
  #região saúde
  tabela_mortreg_saude <- reactive({
                            dadoi <- dados_analise_mort()
                            dadoi <- with(dadoi,as.data.frame(table(reg_saude), stringsAsFactors = F))#)
                           
                            pops <- pops()
                            #pops <- left_join(pops, tab_regioes[,c(3,4)], by = 'cod6')
                            pops <- aggregate(value ~reg_saude, data = pops, FUN = sum)
                            

                            dadoi <- left_join(dadoi, pops, by = 'reg_saude')
                            dadoi$Perc_registros <- with(dadoi, round(Freq*100/sum(Freq),2))
                            dadoi$Tx_mortalidade <- with(dadoi, round((Freq*1E5)/value,2))
                            dadoi <- dadoi[,-3]
                            names(dadoi)[1:2] <- c('Região', 'Registros')
                            dadoi <- dplyr::arrange(dadoi, desc('Tx_mortalidade')) 
                            row.names(dadoi)  <- NULL
                            dadoi 
                               })
 
   mod_summary_card_server('mort_tabela_reg',
                      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h1('Região Saúde')),
                    tags$div(class = 'card-body',
                    #tableOutput('mort_tabelaregsaude_out')
                    reactableOutput('mort_tabelaregsaude_out')
                    ),
                    tags$div(class = 'card-footer',
                       tags$div(class = "ms-auto lh-1",
                          downloadButton('download_morttabela_reg', 'Baixar')      )       
                             )) #end divs     
                    )  
                    
  output$mort_tabelaregsaude_out <- renderReactable({  #DT::renderDataTable({#
                           dadoi <- tabela_mortreg_saude()
                           dadoi <- dplyr::arrange(dadoi, desc(Registros))   
                           reactable(dadoi)
                           #kbl(dadoi) %>%
                           #kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           #scroll_box(width = "100%", height = "480px")
                            }) 
 
 output$download_morttabela_reg <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tabela_mortreg_saude(), file)
    }
  )


#capítulo cid
  mort_tabela_cap <- reactive({
                           pops <- pops()
                           
                           
                            dadoi <- dados_analise_mort()
                            dadoi$CAT <- substr(dadoi$cod_cid_causa_basica,1,3)
                            dadoi <- left_join(dadoi[which(dadoi$dsc_sexo != 'Ignorado'),], cid10, by = 'CAT')
                            dadoi$ones <- 1
                            dadoi <- aggregate(ones ~ cap_romano + descricao_cap, data = dadoi, FUN = sum)                            
                            dadoi[is.na(dadoi)] <- 0
                            dadoi$Tx_mortalidade <- with(dadoi, round((ones*1E5)/sum(pops$value),2))
                            
                            names(dadoi)[1:3] <- c('Capítulo', 'Descrição', 'Registros') 
                            row.names(dadoi)  <- NULL
                            dadoi 
                               })
 
   mod_summary_card_server('mort_tabela_cap',
                      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h1('Capítulo CID10')),
                    tags$div(class = 'card-body',
                    #tableOutput('mort_tabelacid_out')
                    reactableOutput('mort_tabelacap_out')
                    ),
                    tags$div(class = 'card-footer',
                       tags$div(class = "ms-auto lh-1",
                          downloadButton('download_mort_tabela_cap', 'Baixar')      )       
                             )) #end divs     
                    )  
                    
  output$mort_tabelacap_out <- renderReactable({  #DT::renderDataTable({#
                           dadoi <- mort_tabela_cap()
                           reactable(dadoi)
                           #kbl(dadoi) %>%
                           #kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           #scroll_box(width = "100%", height = "480px")
                            }) 
 
 output$download_mort_tabela_cid <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(mort_tabela_cap(), file)
    }
  )
  

                              