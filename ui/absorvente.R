 #ui cras (12-dez-2022, 10:32h)
 
  tabler_tab_item(
         tabName = "absorvente",
          page_heading(title = 'Distribuição de Absorventes nas Escolas', pretitle = NULL),
       tags$div(class = 'page-body',
        fluidRow(class = 'row row-deck row-cards',
             mod_summary_card_ui('absorvente_mapa', div_class = 'col-md-8'),
             mod_summary_card_ui('absorvente_total_card', div_class = 'col-md-4'), 
             mod_summary_card_ui('absorvente_tabela', div_class = 'col-md-5'),
             mod_summary_card_ui('absorvente_tabela_all', div_class = 'col-md-7')
             
                  )#end div      
       
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item