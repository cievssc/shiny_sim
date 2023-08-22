  #server para a aba home (17-abr-2023, )
  #atualizado em 07-jun-2023 (16:54h)
  
  #opções botões dropdown (18-nov-2022,10:24h)
   output$home_dropopcoes <- renderUI(
                               tagList(
                                  selectInput('home_tipo_obito', label = h5('Categoria óbito'),
                                      choices = c('Todos','DCNT', 'Suicídio', 'Acidente V.terrestres', 'Afogamento'), selected = 'Todos' ,
                                      multiple = T),
                                  selectInput('home_tipo_sexo', label = h5('Sexo'),
                                      choices = c('Masculino', 'Feminino'), selected = c('Masculino', 'Feminino'),
                                      multiple = T),     
                                  sliderInput('home_tipo_idade', label = 'Idade:', 
                                  min = 0, max = 150, value = c(0,150))
                                 #  br(),     
                                # actionButton('home_atualizar', 'Atualizar') 
                                       )      
                                     )
  
  #output$testei <- renderPrint({input$home_dropdown
   #             })
  #organizando os dados
  dados_all     <- reactiveVal()
  dados_analise <- reactiveVal(0)
  
  observeEvent(input$head_atualizar,{
                 if(input$head_municipio != 'Todos'){
                                lista_mun <- municipios_br[which(municipios_br$uf == 'Santa Catarina'),c(3,4)]
                                lista_mun <- floor(municipios_br$codigo[municipios_br$municipio %in% input$head_municipio]/10)
                 query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT ",paste(lista_sim, collapse = ', ')," , substr(cod_municipio_ibge_residencia,1,2) AS 'cod_uf_resid'
                                    FROM fat_declaracao_obito_sim
                                   WHERE dat_obito >= ?code1 AND dat_obito <= ?code2 AND
                                   cod_municipio_ibge_residencia IN (\'",paste(lista_mun, collapse = "\',\'"),"\')"), 
                                   code1 = input$home_daterange[1],
                                   code2 = input$home_daterange[2]
                                   ) }else{              
                 query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT ",paste(lista_sim, collapse = ', ')," , substr(cod_municipio_ibge_residencia,1,2) AS 'cod_uf_resid'
                                    FROM fat_declaracao_obito_sim
                                   WHERE dat_obito >= ?code1 AND dat_obito <= ?code2"), 
                                   code1 = input$home_daterange[1],
                                   code2 = input$home_daterange[2]
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
                   dados_all(dadoi)}
                   }, ignoreNULL = F)                            
  
  
   
   
  
  observeEvent(c( input$head_atualizar),{# input$home_atualizar,
                     req(!is.null(dados_all()))
                     dadoi <- dados_all()
                     if(input$home_dropdown >0){
                     if(input$home_tipo_obito == 'DCNT'){
                        dadoi <- dadoi[dadoi$dcnt == T,]
                        }
                     if(input$home_tipo_obito == 'Suicídio'){
                        dadoi <- dadoi[dadoi$suicidio == T,]
                        }
                     if(input$home_tipo_obito == 'Acidente V.terrestres'){
                        dadoi <- dadoi[!is.na(dadoi$tipo_transito),]
                        }      
                     if((input$home_tipo_sexo == 'Masculino') & length(input$home_tipo_sexo) == 1){
                        dadoi <- dadoi[dadoi$sgl_sexo == 'M',]
                        }
                     if((input$home_tipo_sexo == 'Feminino')  & length(input$home_tipo_sexo) == 1){
                        dadoi <- dadoi[dadoi$sgl_sexo == 'F',]
                        }
                     if(!is.null(input$home_tipo_idade)){
                        sequencia <- seq(input$home_tipo_idade[1],input$home_tipo_idade[2], by = 1)
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
                   dados_analise(dadoi)
                   }     
                          })
  
  #séries temporais dos dados                      
  #serie_falta <- reactive({
   #              dadoi <- dados_analise() 
    #             dadoi$mes_ano <- with(dadoi,factor(mes_ano, levels = unique(mes_ano[order(falta_7)])))
     #             serie <- with(dadoi,as.data.frame(table(mes_ano))  )
      #            serie
       #                 })   
  
  
  
  #========================================================================                      
  #cards
  
  #card total 
  mod_summary_card_server('home_total', 
                            card_large(heading =  'Total de registros no período',
                              uiOutput('home_total_card') %>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$home_total_card <- renderUI({
                    dadoi <- dados_analise()
                    dadoi <- nrow(dadoi)
                    tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi)
                    )
                    })
                    
                    
  #card dcnt
  mod_summary_card_server('home_total_dcnt', 
                            card_large(heading =  'DCNT´s',
                              uiOutput('home_totalcard_dcnt')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$home_totalcard_dcnt <- renderUI({
                    dadoi <- dados_analise()
                    dadoi <- paste0(sum(dadoi$dcnt, na.rm = T),'/',round((sum(dadoi$dcnt, na.rm = T)*100/nrow(dadoi)),2),'%')
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    )
                    })
  
  #card suicídio
  mod_summary_card_server('home_total_suic', 
                            card_large(heading =  'Suicídio',
                              uiOutput('home_totalcard_suic')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$home_totalcard_suic <- renderUI({
                    dadoi <- dados_analise()
                    dadoi <- paste0(sum(dadoi$suicidio, na.rm = T),'/',round((sum(dadoi$suicidio, na.rm = T)*100/nrow(dadoi)),2),'%')
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi[which.max(dadoi[,2]),1])
                    )
                    })
 
 #veiculo terrestre
  mod_summary_card_server('home_total_acid', 
                            card_large(heading =  'Acidente com veículos terrestres',
                              uiOutput('home_totalcard_acid')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$home_totalcard_acid <- renderUI({
                    dadoi <- dados_analise()
                    dadoi <- paste0(sum(!is.na(dadoi$tipo_transito)),'/',round((sum(!is.na(dadoi$tipo_transito))*100/nrow(dadoi)),2),'%')
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi[which.max(dadoi[,2]),1])
                    )
                    })
 
 #afogamento (add 20-jun-2023, 21:48h)
  mod_summary_card_server('home_total_afog', 
                            card_large(heading =  'Afogamentos',
                              uiOutput('home_totalcard_afog')%>% withSpinner(color="#0dc5c1")
                             )
                          )
  
  output$home_totalcard_afog <- renderUI({
                    dadoi <- dados_analise()
                    dadoi <- paste0(sum(dadoi$afogamento, na.rm = T),'/',round((sum(dadoi$afogamento, na.rm = T)*100/nrow(dadoi)),2),'%')
                    tagList(tags$div(class = 'text-center display-6 fw-bold my-3',dadoi)
                    #tagList(tags$div(class = 'text-center display-5 fw-bold my-3',dadoi[which.max(dadoi[,2]),1])
                    )
                    }) 
  #============================================================================
  #mapa
  mod_summary_card_server('home_mapa', 
                   card_large(heading = tagList(h1('Mapa')),
                      leafletOutput('home_mapa_leaflet') %>% withSpinner(color="#0dc5c1"))
                             )
                             
                             
  output$home_mapa_leaflet <- renderLeaflet({ home_leaflet_data()
        })
        
  home_leaflet_data <- reactive({
  
   dadoi <- dados_analise()
   
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
  
                        
 #============================================================================
 #gráficos
 #card serie óbitos
  mod_summary_card_server('home_serie', 
                            tagList(
                     div(class = 'card',
                       div(class = 'card-header', style = 'display:flex;   justify-content:space-between;',
                           h1(class = 'card-title', 'Série óbito'), 
                           selectInput('home_tipograf1', label = NULL, choices = c('Semana Epidemiológica' = 1, 'Mensal' = 2),
                           selected = 1, multiple = F)),
                            div(class = 'body',
                      apexchartOutput('long_serie_chart', height = '500px') %>% withSpinner(color="#0dc5c1")))
                             ) #end taglist
                            ) #end 
  
  


    output$long_serie_chart <- renderApex({
                           dadoi <- dados_analise()
                           if(input$home_tipograf1 == 1){
                           dadoi <- as.data.frame(table(dadoi$semana_epid))
                           dadoi[dadoi == 0] <-NA 
                           
                       list(series = list(list(name = 'Óbitos',data = dadoi[,2])
                                              ),
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%',
                                                       stacked = TRUE),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = dadoi$Var1
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
                                              )} else {
                           dadoi$ano <- with(dadoi, substr(dat_obito,1,4)) %>% as.numeric
                           dadoi$mes <- with(dadoi, substr(dat_obito,6,7)) %>% as.numeric
                           dadoi <- with(dadoi, as.data.frame(table(mes, ano)))
                           levels(dadoi$mes) <- c('jan', 'fev', 'mar', 'abr','maio', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez')
                           serie <- lapply(split(dadoi, dadoi$ano), function(x){
                                           list(name = x$ano[1],
                                           data = x$Freq)
                                                        }) %>% unname
                          
                       list(series = serie,
                                              chart = list(type = 'bar', 
                                                       toolbar = c(show = TRUE),
                                                       height = '100%'),
                                              dataLabels = list(enabled = FALSE),
                                              xaxis = list(
                                                      categories = levels(dadoi$mes )
                                                      ),
                                            
                                              plotOptions = list(
                                                        bar = list(
                                                          horizontal = FALSE,
                                                          endingShape = 'rounded'
                                                          )
                                                          ),
                                              dataLabels = list(
                                                             enabled = F),
                                              legend = c(show = TRUE)
                                              )  
                                              }
                                     })  #end renderapex                       
                        
 
 #sexo ----------------------------
 mod_summary_card_server('home_sexo', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Sexo')),
                            div(class = 'body',
                      apexchartOutput('home_sexo_chart', height = '250px')))
                             ) #end taglist
                             )
 
 output$home_sexo_chart <- renderApex({
                           dadoi  <- dados_analise()
                           dadoi$dsc_sexo <- factor(dadoi$dsc_sexo, levels = c('Masculino','Feminino',  'Ignorado'))
                           dadoi <-  as.data.frame(table(dadoi$dsc_sexo))

                           list(series = c(dadoi[,2]),
                            chart = list(type = 'donut',
                                         height = '100%'),
                            labels = dadoi[,1],
                            legend = list(position = 'bottom'))
                             
                                     })  #end renderapex 
                                     
                                     
 #sexo escolaridade
 mod_summary_card_server('home_escolaridade', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Escolaridade')),
                            div(class = 'body',
                      apexchartbarOutput('home_escola_chart', height = '250px')))
                             ) #end taglist
                             )
 
 output$home_escola_chart <- renderApex({
                           dadoi <- dados_analise()
                           dadoi$dsc_escolaridade <- factor(dadoi$dsc_tipo_escolaridade, levels = c('Ignorado', 'Nenhuma', 
                                                     'de 1 a 3', 'de 4 a 7', 'de 8 a 11', '12 e mais'))
                           dadoi <- with(dadoi,as.data.frame(table(dsc_tipo_escolaridade, dsc_sexo)))
                           
                           dadoi <- tidyr::spread(dadoi, value = Freq, key = dsc_sexo)
                           if(ncol(dadoi) == 2){lista <- list(list(name = input$home_tipo_sexo[1], data = dadoi[,2]))}else{
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
                                     
 #pirâmide etária
 mod_summary_card_server('home_idade', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Idade')),
                            div(class = 'body',
                      apexchartbarOutput('home_idadesexo_chart', height = '250px')))
                             ) #end taglist
                             )
 
 output$home_idadesexo_chart <- renderApex({
                          dadoi <- dados_analise()
                          dadoi$faixa_idade <- ifelse(dadoi$cod_tipo_idade == '4', 
                                                cut(dadoi$num_idade, breaks = c(-Inf,10,20,30,40,50,60,70,80, Inf), right = F, include.lowest = T,
                                                labels = F), NA) %>% as.factor
                          dadoi$faixa_idade <-  dplyr::recode_factor(dadoi$faixa_idade, 
                                                       `1` = '0-9',
                                                       `2` = '10-19',
                                                       `3` = '20-29',
                                                       `4` = '30-39',
                                                       `5` = '40-49',
                                                       `6` = '50-59',
                                                       `7` = '60-69',
                                                       `8` = '70-79',
                                                       `9` = '+80')
                          
                          
                          dadoi <- with(dadoi,as.data.frame(table(faixa_idade, dsc_sexo)))
                           
                           dadoi <- tidyr::spread(dadoi, value = Freq, key = dsc_sexo)
                           if(ncol(dadoi) == 2){lista <- list(list(name = input$home_tipo_sexo[1], data = dadoi[,2]))}else{
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

 #sunburst (20-jun-2023, 16:25h)----------------------------
 mod_summary_card_server('home_graf_classific', 
                   tagList(
                     div(class = 'card',
                       div(class = 'card-header',
                           h1(class = 'card-title', 'Detalhamento tipo óbito')),
                            div(class = 'body',
                      echartsOutput('home_grafsun', height = '600px')))
                             ) #end taglist
                             )
 
 output$home_grafsun <- renderEcharts({
                           dadoi  <- dados_analise()
                           dcnt <- as.data.frame(table(dadoi$tipodcnt))
                           acidente <- as.data.frame(table(dadoi$tipo_transito))
                           afogamento <- as.data.frame(table(dadoi$tipoafogamento))
                           dadoii <- dplyr::bind_rows(list(dcnt, acidente, afogamento))
                           dadoi <- list(list(
                                    name = 'DCNT',
                                    children = lapply(1:nrow(dcnt), function(x){func_sunburst(x,y = dcnt)})
                                        ),
                                        list(
                                    name = 'Acidente V.Terrestre',
                                    children = lapply(1:nrow(acidente), function(x){func_sunburst(x,y = acidente)})    
                                        ),      
                                        list(
                                    name = 'Afogamento',
                                    children = lapply(1:nrow(afogamento), function(x){func_sunburst(x,y = afogamento)})    
                                        ) 
                                        )    
                           

                           list(
                                visualMap = list(
                                            type = 'continuous',
                                            min = min(dadoii[,2]), 
                                            max = sum(dadoii[,2]),
                                            inRange = list(color = c('#2F93C8', '#AEC48F', '#FFDB5C', '#F98862')))         
                                         ,
                                series = list(
                                         type = 'sunburst',
                                         data = dadoi,
                                         #radius = c(0, '90%'),
                                         levels = list(list(),
                                                    list(r0 = '10%',
                                                         r1 = '40%',
                                                         label = list(rotate = 'radial')
                                                         ),
                                                    list(r0 = '40%',
                                                         r1 = '100%',
                                                         label = list(position = 'outside',
                                                                      padding = 3,
                                                                      silent = FALSE))
                                         
                                         )
                                   ))
                                     })  #end renderecharts
  #==================================================
  #tabelas
  #região saúde
  tabela_reg_saude <- reactive({
                            dadoi <- dados_analise()
                            dadoi <- with(dadoi,as.data.frame(table(reg_saude)))#, stringsAsFactors = F))
                            dadoi$Perc_registros <- with(dadoi, round(Freq*100/sum(Freq),2))
                            pops <- aggregate(pop ~reg_saude, data = pops_2022, FUN = sum,stringsAsFactors = F)
                            pops$Perc_populacao <- with(pops, round(pop*100/sum(pop),2))
                            dadoi <- dplyr::left_join(dadoi, pops[,-2], by = 'reg_saude')
                            names(dadoi)[1:2] <- c('Região', 'Registros')
                            dadoi <- dplyr::arrange(dadoi, desc('Registros')) 
                            row.names(dadoi)  <- NULL
                            dadoi 
                               })
 
   mod_summary_card_server('home_tabela_reg',
                      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h1('Região Saúde')),
                    tags$div(class = 'card-body',
                    #tableOutput('home_tabelaregsaude_out')
                    reactableOutput('home_tabelaregsaude_out')
                    ),
                    tags$div(class = 'card-footer',
                       tags$div(class = "ms-auto lh-1",
                          downloadButton('download_hometabela_reg', 'Baixar')      )       
                             )) #end divs     
                    )  
                    
  output$home_tabelaregsaude_out <- renderReactable({  #DT::renderDataTable({#
                           dadoi <- tabela_reg_saude()
                           dadoi <- dplyr::arrange(dadoi, desc(Registros))   
                           reactable(dadoi)
                           #kbl(dadoi) %>%
                           #kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           #scroll_box(width = "100%", height = "480px")
                            }) 
 
 output$download_hometabela_reg <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tabela_reg_saude(), file)
    }
  )
  
  
  #cid causa principal
  tabela_cid <- reactive({
                            dadoi <- dados_analise()
                            dadoi$CAT <- substr(dadoi$cod_cid_causa_basica,1,3)
                            dadoi <- left_join(dadoi[which(dadoi$dsc_sexo != 'Ignorado'),], cid10, by = 'CAT')
                            dadoi$ones <- 1
                            dadoi <- aggregate(ones ~ CAT + DESCRICAO + dsc_sexo, data = dadoi, FUN = sum)
                            dadoi <- tidyr::spread(dadoi, key = dsc_sexo, value = ones)
                            dadoi[is.na(dadoi)] <- 0
                            if(length(input$home_tipo_sexo) != 1){
                            dadoi$Total <- apply(dadoi[,3:4],1,sum)
                            dadoi <- dplyr::arrange(dadoi, desc('Total'))}
                            names(dadoi)[1:2] <- c('CID10', 'Descrição') 
                            row.names(dadoi)  <- NULL
                            dadoi 
                               })
 
   mod_summary_card_server('home_tabela_cid',
                      tags$div(class = 'card',
                    tags$div(class = 'card-header',
                    h1('Causa Principal')),
                    tags$div(class = 'card-body',
                    #tableOutput('home_tabelacid_out')
                    reactableOutput('home_tabelacid_out')
                    ),
                    tags$div(class = 'card-footer',
                       tags$div(class = "ms-auto lh-1",
                          downloadButton('download_hometabela_cid', 'Baixar')      )       
                             )) #end divs     
                    )  
                    
  output$home_tabelacid_out <- renderReactable({  #DT::renderDataTable({#
                           dadoi <- tabela_cid()
                           reactable(dadoi)
                           #kbl(dadoi) %>%
                           #kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                           #scroll_box(width = "100%", height = "480px")
                            }) 
 
 output$download_hometabela_cid <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tabela_cid(), file)
    }
  )
  
  
  
 output$teste <- renderApex({
                list(
                xAxis = list(type = 'category',
                             data = letters[1:7]),
                yAxis = list(type = 'value'),
                series = list(data = c(150,230,224,218,135,147,260),
                         type = 'line'
                         )
                )
              })  