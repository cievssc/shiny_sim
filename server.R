 #app saúde
 #atualizado em 07-jun-2023 (16:54h)
 shinyServer(function(input, output, session) {
    
    #----------------------------------------------------------------------
    #dados sim
    source('./server/server_home.R', local = T, encoding = 'UTF-8')
    
    #mortalidade
    source('./server/server_mortalidade.R', local = T, encoding = 'UTF-8')
    
 
 }) #end server function
