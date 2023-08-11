 #ui moradia  (15-dez-2022, 15:04h)
 
  tabler_tab_item(
         tabName = "moradia",
          page_heading(title = 'Documentação e status das obras', pretitle = 'SC Mais Moradia'),
       tags$div(class = 'page-body',
        fluidRow(class = 'row row-deck row-cards',
             mod_summary_card_ui('moradia_progress', div_class = 'col-md-12'),
             mod_summary_card_ui('moradia_mapa', div_class = 'col-md-8'),
             mod_summary_card_ui('moradia_tabela', div_class = 'col-md-4'),
             mod_summary_card_ui('moradia_graf_status', div_class = 'col-md-5'),
             mod_summary_card_ui('moradia_tabela_all', div_class = 'col-md-7')
             
                  )#end div      
       
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item