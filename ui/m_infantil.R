 #ui mortalidade infantil
 
  tabler_tab_item(
         tabName = "infantil",
          page_heading(title = 'Mortalidade Infantil', # pretitle = 'DCNT´s', 
                       tags$div( class ="col-auto ms-auto d-print-none",
                          shinyWidgets::dropdown(style = "unite", icon = icon("gear"),  inputId = 'inf_dropdown',
                                uiOutput('inf_dropopcoes'), left = T))),
       tags$div(class = 'page-body',
        fluidRow(class = 'row row-deck row-cards',
             column(4,
               fluidRow(class = 'row row-deck row-cards',
                  mod_summary_card_ui('inf_total_obito', div_class = "col-md-12"),
                  mod_summary_card_ui('inf_perc_evitavel', div_class = "col-md-12"), 
                  mod_summary_card_ui('inf_var_semana', div_class = "col-md-12")
                  )), #end column
                  mod_summary_card_ui('inf_serie_temporal', div_class = "col-md-8"),
                  mod_summary_card_ui('inf_mapa', div_class ='col-md-7'),
                  mod_summary_card_ui('inf_sexo', div_class = 'col-md-5'),
                  column(12,
                  tags$div(class = 'card',
                   tags$div(class = 'card-header',
                    h1('Perfil')),
                    tags$div(class = 'card-body',
                     tags$div(class = 'row row-cards',
                      mod_summary_card_ui('inf_cor', div_class = 'col-md'),
                      mod_summary_card_ui('inf_escolaridade', div_class = 'col-md'),
                      mod_summary_card_ui('inf_sitconjugal', div_class = 'col-md'),
                      mod_summary_card_ui('inf_idaderesp', div_class = 'col-md'),
                      mod_summary_card_ui('inf_qtde_filhos', div_class = 'col-md')
                      ))) #end divs
                  )  #end column
                
                  )#end div      
       
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item