 #ui cras (12-dez-2022, 10:32h)
 
  tabler_tab_item(
         tabName = "cras",
          page_heading(title = 'Cadastramento dos municípios', pretitle = 'CRAS'),
       tags$div(class = 'page-body',
        fluidRow(class = 'row row-deck row-cards',
             mod_summary_card_ui('cras_progress', div_class = 'col-md-12'),
             mod_summary_card_ui('cras_mapa', div_class = 'col-md-8'),
             mod_summary_card_ui('cras_tabela', div_class = 'col-md-4'),
             mod_summary_card_ui('cras_tabela_all', div_class = 'col-md-12')
             
                  )#end div      
       
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item