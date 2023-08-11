 #usando apexchart (26-set-22, 01-24h)
 #deixando aberta alternativas de uso entre diversos tipos (30-maio-2021)
# To be called from ui.R
 apachechartOutput <- function(inputId, width="100%", height="100%") { #
  style <- sprintf("width: %s; height: %s;",
    validateCssUnit(width), validateCssUnit(height))
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple lineChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
    
      tags$script(src="https://cdn.jsdelivr.net/npm/echarts@5.4.2/dist/echarts.min.js"),
      tags$script(src = './JS/apache_general.js')
    )),
    div(id=inputId, class="apachechart", style = style,
    #tag("svg", list())
    )
  )
 }
 
 