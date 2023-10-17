 #ui home  (17-abr-2023, 15:45h)
 
  tabler_tab_item(
         tabName = "home",
          page_heading(title = 'SIM', pretitle = 'Sistema de Informação de Mortalidade', 
                       tags$div( class ="col-auto ms-auto d-print-none",
                          shinyWidgets::dropdown(style = "unite", icon = icon("gear"),  inputId = 'home_dropdown',
                                uiOutput('home_dropopcoes'), left = T))),
       tags$div(class = 'page-body',
        tags$div(class = 'container-xl',
        fluidRow(class = 'row row-deck row-cards',
             #column(4,
              # fluidRow(class = 'row row-deck row-cards',
                  mod_summary_card_ui('home_total', div_class = "col-md-3"),
                  mod_summary_card_ui('home_total_dcnt', div_class = "col-md-3"),
                  mod_summary_card_ui('home_total_suic', div_class = "col-md-2"),
                  mod_summary_card_ui('home_total_acid', div_class = "col-md-2"),
                  mod_summary_card_ui('home_total_afog', div_class = "col-md-2"),
                  #)), #end column
             
                  mod_summary_card_ui('home_mapa', div_class = "col-md-7"),
                  mod_summary_card_ui('home_serie', div_class = 'col-md-5'),
                  mod_summary_card_ui('home_sexo', div_class = 'col-md-4'),
                  mod_summary_card_ui('home_escolaridade', div_class = 'col-md-4'),
                  mod_summary_card_ui('home_idade', div_class = 'col-md-4'),
                  mod_summary_card_ui('home_tabela_reg', div_class = 'col-md-5'),
                  mod_summary_card_ui('home_graf_classific', div_class = "col-md-7"),
                  mod_summary_card_ui('home_tabela_cid', div_class = "col-md-12"),
                  mod_summary_card_ui('home_tabela_cap', div_class = "col-md-12")
                   
                  )#end row
       )
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item