 #ui cras (12-dez-2022, 10:32h)
 
  tabler_tab_item(
         tabName = "bolsa",
          page_heading(title = 'Quantidade de bolsas concedidas', pretitle = 'Bolsa estudante'),
       tags$div(class = 'page-body',
        fluidRow(class = 'row row-deck row-cards',
             mod_summary_card_ui('bolsa_progress', div_class = 'col-md-12'),
             mod_summary_card_ui('bolsa_mapa', div_class = 'col-md-7'),
             mod_summary_card_ui('bolsa_tabela', div_class = 'col-md-5'),
             mod_summary_card_ui('bolsa_tabela_all', div_class = 'col-md-12')
             
                  )#end div      
       
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item