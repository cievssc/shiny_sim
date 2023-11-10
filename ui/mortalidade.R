 #ui mortalidade (22-set-2023, 15:40h)

  tabler_tab_item(
         tabName = "mortalidade",
          page_heading(title = 'SIM', pretitle = 'Sistema de Informação de Mortalidade', 
                       tags$div( class ="col-auto ms-auto d-print-none",
                          shinyWidgets::dropdown(style = "unite", icon = icon("gear"),  inputId = 'mort_dropdown',
                                uiOutput('mort_dropopcoes'), left = T))),
       tags$div(class = 'page-body',
         tags$div(class = 'container-xl',        
        fluidRow(class = 'row row-deck row-cards',
             #column(4,
              # fluidRow(class = 'row row-deck row-cards',
                  mod_summary_card_ui('mort_total', div_class = "col-md-3"),
                  mod_summary_card_ui('mort_total_dcnt', div_class = "col-md-3"),
                  mod_summary_card_ui('mort_total_suic', div_class = "col-md-2"),
                  mod_summary_card_ui('mort_total_acid', div_class = "col-md-2"),
                  mod_summary_card_ui('mort_total_afog', div_class = "col-md-2"),
                  #)), #end column
             
                  mod_summary_card_ui('mort_mapa', div_class = "col-md-7"),
                  mod_summary_card_ui('mort_serie', div_class = 'col-md-5'),
                  mod_summary_card_ui('mort_sexo', div_class = 'col-md-6'),
                  #mod_summary_card_ui('mort_escolaridade', div_class = 'col-md-4'),
                  mod_summary_card_ui('mort_idade', div_class = 'col-md-6'),
                  mod_summary_card_ui('mort_tabela_reg', div_class = 'col-md-12'),
                  #mod_summary_card_ui('mort_graf_classific', div_class = "col-md-7"),
                  #mod_summary_card_ui('mort_tabela_cid', div_class = "col-md-12"),
                  mod_summary_card_ui('mort_tabela_cap', div_class = "col-md-12")
                   
                  )#end row
         ) #end container-xl
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item