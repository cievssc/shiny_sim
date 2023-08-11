 #ui del(06-jan-23, 00:32h)
 
  tabler_tab_item(
         tabName = "del",
          page_heading(title = 'Etapas de implementação', pretitle = 'DEL'),
       tags$div(class = 'page-body',
        fluidRow(class = 'row row-deck row-cards',
            column(5,
             fluidRow(class = 'row row-deck row-cards',
             mod_summary_card_ui('del_progress_init', div_class = 'col-md-6'),
             mod_summary_card_ui('del_progress_sens', div_class = 'col-md-6'),
             mod_summary_card_ui('del_progress_doc', div_class = 'col-md-6'),
             mod_summary_card_ui('del_progress_situac', div_class = 'col-md-6'),
             mod_summary_card_ui('del_progress_gov', div_class = 'col-md-6'),
             mod_summary_card_ui('del_progress_estrut', div_class = 'col-md-6')
             )), #end column
             column(7,
             mod_summary_card_ui('del_graf', div_class = 'col-md-12')
             ), #end column
             mod_summary_card_ui('del_mapa', div_class = 'col-md-7'),
             mod_summary_card_ui('del_graf2', div_class = 'col-md-5'),
             mod_summary_card_ui('del_tabela_all', div_class = 'col-md-12')
             
                  )#end div      
       
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item