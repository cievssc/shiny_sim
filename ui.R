 #app SIM (17-mar-23))
 #atualizado em 07-jun-2023 (16:54h)
 #atualizado em 25-set-2023 (14:36h)
 ui <-  tabler_page(dark = F,
   header_tabler(logo = HTML('<img src="./images/cievs_nacional.png" width="232px" height="110" alt="Tabler" class="navbar-brand-image">'),
               tags$link(rel = "stylesheet",type = 'text/css', href = './css/legenda_leaflet.css'),
               tags$div( class =  "navbar-nav flex-row order-md-last", 
                 tags$div(class = "nav-item d-none d-md-flex me-3",
                     selectInput("head_municipio", label = "Município: ", 
                       choices =  c('Todos', lapply(split(tab_regioes, tab_regioes$reg_saude), function(x){x[,'municipio']})),
                       multiple = T, selected = 'Todos')
                       ), #end dib
                  tags$div(class = "nav-item d-none d-md-flex me-3",
                     uiOutput('home_dateinput')
                      #códigtos em server_home
                     
                       ), #end dib      
                      tags$div(class = "nav-item d-none d-md-flex me-3",
                      actionButton("head_atualizar", label = "Atualizar")
                          ) #end div
                          )), #endheader
   tabler_navbar(
     #brand_url = "https://preview-dev.tabler.io",
     #brand_image = "https://preview-dev.tabler.io/static/logo.svg",
     nav_menu = tabler_navbar_menu(
      inputId = 'current_tab',
     
       #id =  "current_tab",
       tabler_navbar_menu_item(
         text = "Frequência Óbitos",
         icon = icon_home(),
         tabName = "home",
         selected = TRUE
       ),
       tabler_navbar_menu_item(
         text = "Tx. Mortalidade",
         icon = icon_calc(),
         tabName = "mortalidade"#,
         #selected = FALSE
       )
     )#,
     #tags$button("update", "Change tab", icon = icon("exchange-alt"))
   ),
   tabler_body(classe = 'page-wrapper',
     tabler_tab_items(
                      
      source('./ui/home.R',  local = T, encoding = 'UTF-8')$value,
      source('./ui/mortalidade.R', local = T, encoding = 'UTF-8')$value  
    
       
     ),
    tabler_footer(
       left = "CIEVS/DIVE",
       right = a(href = "https://www.google.com")
     )
   )
  )
                        