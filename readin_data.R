 #lendo API de leitos (22-jun-20)
 #versão para shiny (19-jul-20)
 
  library('curl')
 library('httr')
 library('jsonlite')
 
# leitos_swagger <- GET('http://api-leitos.saude.sc.gov.br:3000/docs/swagger.json')
 #leitos_auth <- GET('http://api-leitos.saude.sc.gov.br:3000/auth/login')
 cabec <- c(authorization = 'Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJkaW1pdHJpYmVzc2FAZ21haWwuY29tIiwiaXNzIjoibWVkc3VpdGUtbGVpdG9zIiwiaWF0IjoxNTkyODQ4MzY1fQ.4nDZPnJwpL1u8ScOCjGohpaK2j20p4MIQz56cVTdqZU',
          Pragma = 'no-cache')
 
 #leitos
 leitos_leitos <- GET('http://api-leitos.saude.sc.gov.br:3000/csv/leitos', add_headers(cabec), accept('text/csv'))
 leitos_dado <- rawToChar(content(leitos_leitos))
 leitos_dado <- strsplit(leitos_dado, '\n')
 leitos_dado <- strsplit(leitos_dado[[1]],',')
 leitos_dado <- lapply(leitos_dado, function(x){str_replace_all(x, '\\"','')})
 leitos_dado <- purrr::map_df(leitos_dado, function(x){as.data.frame(t(x))})
 nomes <- as.vector(leitos_dado[1,])
 leitos_dado <- leitos_dado[-1,] 
 names(leitos_dado) <- nomes
 leitos_dado[,1] %<>% as.numeric()
 leitos_dado$data <- substr(leitos_dado[,8],1,10) %>% as.Date()
 leitos_dado$hora <- substr(leitos_dado[,8],12,16) %>% data.table::as.ITime()
 leitos_dado[, which(is.na(names(leitos_dado)))] <- NULL
 
 
 #---------------------- adicionando informações
 load('dado_cnes.RData')
 macroi_saude <- macro_saude
 macroi_saude$cod6 <- with(macroi_saude, floor(codigo/10))
 
 leitos_dado <- left_join(leitos_dado, dado_cnes, by = c('cnes' = 'CO_CNES')) %>%
                left_join(., macroi_saude, by = c('CO_MUNICIPIO_GESTOR' = 'cod6')) %>%
               # left_join(., pops_sc[,-c(1:4)], by = 'codigo') %>%
                left_join(., tab_regioes, by = 'codigo')
                
 leitos_dado$leito_simp <- with(leitos_dado, ifelse(tipo_leito == 'UTI ADULTO' |
                           tipo_leito == 'UTI NEONATAL' |tipo_leito == 'UTI PEDIATRICA', 'UTI','Convencional' ))
 
 leitos_dado$classificacao <- with(leitos_dado, ifelse(classificacao == 'uti', 'UTI','Enfermaria' ))
                           
 dia_atual <- Sys.time() 
 
 save(dia_atual, file = './dia_atual.RData')
 save(leitos_dado, file = './leito_dado.RData')