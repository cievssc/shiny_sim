 #cod6 para o mapa
 municipiopoly$cod6 <- floor(as.numeric(municipiopoly$CD_GEOCMU)/10)
 municipiosf$cod6 <- floor(as.numeric(municipiosf$CD_GEOCMU)/10)
 
 #codigo municipio formatado
 pops_2022$codigo <- with(pops_2022, paste0(cod_uf, cod_mun)) %>% as.numeric
 pops_2022 <- dplyr::left_join(pops_2022, tab_regioes[,c(1,3)], by = 'codigo')
 pops_2022$pop <- as.numeric(pops_2022$pop)
 #funão para obter coordenadas de endereço (sem utiliudade no momento)
 func_geo <- function(x, ...){
                     key  <-
                     dadoi <- google_geocode(x, key = key) %>%
                              geocode_coordinates
                     dadoi[1,] 
                      }
                      
 #função para agregar os dados de endereço
  #func_agregar_end <- function(x){
  #                       with(x, paste0(endereco_obito,', ', bairro_obito,', ',municipio_obito, ', ',long_uf_obito))      
  #                             }
                               
  #funcao para contar os dados
   func_contagem <- function(x, y){
                          sum(x[x == y], na.rm = T)  
                           } 
                           
                           
  #função para atribuir categorias aos dados do SIM
  
  func_sim <- function(x){
              dadoi <- x
              dadoi$num_idade <- ifelse(as.numeric(dadoi$cod_tipo_idade) == 5, as.numeric(dadoi$num_idade) + 100,
                                 ifelse(!(dadoi$cod_tipo_idade %in% c('4','5')), 0, 
                                    as.numeric(dadoi$num_idade)))
              
  dadoi$dcnt <- with(dadoi, ifelse(#(cod_tipo_idade == '4' & (num_idade >= 30 & num_idade <= 69)) &
                            (grepl('I',cod_cid_causa_basica) | 
                             grepl(paste(paste0('J',c(30:35,37:98)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(c(paste0('C0',c(0:9)),paste0('C',c(10:97))), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('E',c(10:14)), collapse = '|'),cod_cid_causa_basica)), T, F
                            ))
  
  dadoi$tipodcnt <- with(dadoi, ifelse(dcnt == T & grepl(paste(paste0('J',c(30:35,37:98)), collapse = '|'),cod_cid_causa_basica), 'Doenças respiratórias',
                                ifelse(dcnt == T & grepl('I',cod_cid_causa_basica), 'Doenças cardiovasculares',
                                ifelse(dcnt == T & grepl(paste(c(paste0('C0',c(0:9)),paste0('C',c(10:97))), collapse = '|'),cod_cid_causa_basica), 'Neoplasias',
                                ifelse(dcnt == T & grepl(paste(paste0('E',c(10:14)), collapse = '|'),cod_cid_causa_basica), 'Diabetes mellitus',
                                NA
                                )))))
                                
 #suicídio                               
 dadoi$suicidio <- with(dadoi, ifelse((cod_tipo_idade == '4' & (num_idade >= 5)) &
                            (grepl(paste(paste0('X',c(60:84)), collapse = '|'),cod_cid_causa_basica) |
                             grepl('Y87',cod_cid_causa_basica)), T, F
                            )) 
                                
 #acidente de trânsito
 dadoi$tipo_transito <- with(dadoi, 
                                ifelse(grepl(paste(c(paste0('V0',c(0:9))), collapse = '|'),cod_cid_causa_basica), 'Pedestre traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(10:19)), collapse = '|'),cod_cid_causa_basica), 'Ciclista traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(20:29)), collapse = '|'),cod_cid_causa_basica), 'Motociclista traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(30:39)), collapse = '|'),cod_cid_causa_basica), 'Ocupante de triciclo motorizado traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(40:49)), collapse = '|'),cod_cid_causa_basica), 'Motorista de automóvel traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(50:59)), collapse = '|'),cod_cid_causa_basica), 'Motorista de caminhonete traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(60:69)), collapse = '|'),cod_cid_causa_basica), 'Ocupante de veículo de transporte pesado traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(70:79)), collapse = '|'),cod_cid_causa_basica), 'Ocupante de ônibus traumatizado em acidente de transporte',
                                ifelse(grepl(paste(paste0('V',c(80:89)), collapse = '|'),cod_cid_causa_basica), 'Outros acidentes de transporte terrestre',
                                NA
                                )))))))))
                                )
                                
 #óbito materno
 dadoi$materno <- with(dadoi, ifelse(((cod_tipo_idade == '4' & sgl_sexo == 'F') & (num_idade >= 10 & num_idade <= 49)) &
                            (grepl(paste(c(paste0('O0',c(0:9)), paste0('O', c(10:95,98,99))), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(c('A34'), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(c(paste0('B',c(20:24))), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(c('D392'), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(c('E230'), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(c('F53'), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(c('M830'), collapse = '|'),cod_cid_causa_basica) 
                             ), T, F
                            ))
 
 dadoi$materno <- with(dadoi, ifelse(materno == T & grepl("O",cod_cid_causa_basica), T, F))
 
 #add 20-jun-2023, 20:57h
 #afogamento
 dadoi$afogamento <- with(dadoi, ifelse(#(cod_tipo_idade == '4' & (num_idade >= 30 & num_idade <= 69)) &
                             grepl(paste(paste0('V',c(90,92)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('W',c(65,66)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('W',c(67,68)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('W',c(69,70)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('W',c(73,74)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('X',c(71)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('X',c(92)), collapse = '|'),cod_cid_causa_basica) |
                             grepl(paste(paste0('Y',c(21)), collapse = '|'),cod_cid_causa_basica),  T, F
                            ))
                            
 dadoi$tipoafogamento <- with(dadoi, ifelse(#(cod_tipo_idade == '4' & (num_idade >= 30 & num_idade <= 69)) &
                      grepl(paste(paste0('V',c(90,92)), collapse = '|'),cod_cid_causa_basica), 'Acidente com embarcação ou transporte com água',
                      ifelse(grepl(paste(paste0('W',c(65,66)), collapse = '|'),cod_cid_causa_basica), 'Afogamento em banheira',
                      ifelse(grepl(paste(paste0('W',c(67,68)), collapse = '|'),cod_cid_causa_basica), 'Afogamento em piscina',
                      ifelse(grepl(paste(paste0('W',c(69,70)), collapse = '|'),cod_cid_causa_basica), 'Afogamento em águas naturais',
                      ifelse(grepl(paste(paste0('W',c(73,74)), collapse = '|'),cod_cid_causa_basica), 'Outros afogamentos',
                      ifelse(grepl(paste(paste0('X',c(71)), collapse = '|'),cod_cid_causa_basica), 'Lesão autoprovocada intencionalmente para afogamento ou submersão', 
                      ifelse(grepl(paste(paste0('X',c(92)), collapse = '|'),cod_cid_causa_basica), 'Agressão por afogamento',
                      ifelse(grepl(paste(paste0('Y',c(21)), collapse = '|'),cod_cid_causa_basica), 'Afogamento com intenção não determinada', NA)))))))))
 
 
 dadoi
              }
              
              
 #funcao para dados sunburst (20-jun-2023, 17:05h)
 func_sunburst <- function(x,y){
                  dadoi <- y[x,]
                  list(name = dadoi[1,1],
                       value = dadoi[1,2])
                  }
 