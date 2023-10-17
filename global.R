 #carregando os pacotes...
 library('shiny')
 library('shinydashboard')
 library('shinydashboardPlus')
 library('shinyWidgets')
 #library('plotly')
 library('leaflet')
 library('leaflet.extras')
 #library('leaflet.extras2')
 #library('bs4Dash')
 library('htmltools')
 library('shinycssloaders') #add em 18-nov-2022(14:07h)

 library('dplyr')        #manipulação de dados - tydiverse
 library('stringr')      #funções de string  - tydiverse
 library('rgeos') #leitura de mapas
 library('rgdal') #leitra de mapas
 library('sf') #plot maps
 library('magrittr')     #para mudar nome de colunas
 library('reshape2')
 library('data.table')
 library('RColorBrewer')
 #library('scales')
 library('raster')
 library('lubridate') #para operações de data
 library('reactable') #análogo ao DT
 #library('ggplot2')
 #library('googleway') #funcões georreferenciamento
 #para predições
# library('forecast')
 #funções de banco de dados
 library('odbc')
 library('DBI')
 
 #tabelas
 library('kableExtra')
 #library('formattable') #tabelas personalizadas
 #carregand dados
 
 load('municipios_br.RData')
 load('municipiopoly.RData')
 load('cid10.RData') #todos os cids
 load('estab_saude.RData')
 load('regioes_saude.RData')
 load('tabs.RData')
 load('mapa_regionais.RData')
 load('pops_2022.RData')
 load('serie_pop.RData')
 load('municipiosf.RData')
 
 
 source('./treating_data.R', local = T, encoding = 'UTF-8')
 source('./variaveis_ext.R', local = T, encoding = 'UTF-8')
 options(warn = -1)
 
 #apexcharts
 source('./www/apexchart/general_apex.R')
 
 #echarts (add em 13-jun-2023)
 source('./www/echarts/general_echarts.R')
 
 
 #plotly
 #source('./www/highcharts/generalhigh.R')
 #source('./www/highcharts/plotlyjs.R')
 
 #carregando funçes dashboard
 source('./www/tablerdash/funcoes_dashboard.R')
 source('./www/tablerdash/cards.R')
 source('./www/tablerdash/icons.R')
 
 #TODO esconder as credenciais de acesso ao Boa Vista
 conn <- function(){DBI::dbConnect(odbc::odbc(),'CIASC BoaVista',
       uid = vetor_a,
        pwd = vetor_b,
        schema = 'ses')
        }
 
 #conn <- function(){DBI::dbConnect(odbc::odbc(),
 #                     driver = '/opt/cloudera/impalaodbc/lib/64/libclouderaimpalaodbc64.so',
  #                    SSL=1,AuthMech=3,
  #                    host = 'boavista-dados.ciasc.sc.gov.br',
   #                   port = 21050,
    #                  uid = vetor_a,
     #                 pwd = vetor_b,
      #                schema = 'ses')}
 
 