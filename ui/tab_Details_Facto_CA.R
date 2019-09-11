
tabBox(
  id = "tabBox_ORC",
  width = 12,
  titlePanel(div(paste(gettext("CA on the dataset"),nomData),style="color:#2A0A29",align="center"),windowTitle="CAshiny"),
  div(style = 'height: 80vh; overflow-y: auto;',
      sidebarLayout(
        sidebarPanel(
          tags$head(
            tags$style("body {background-color: #F0E6E6; }"),
            tags$style(type='text/css', "#title1 { height: 25px; }")
          ),
          wellPanel(
            div(align="center",checkboxInput("caparam",gettext("Show CA parameters"),FALSE)),
            conditionalPanel(
              condition="input.caparam==true",
              br(),
              if(is.null(colonnesup)){
                radioButtons("selecactive",label=h6(gettext("Choose the active columns")),
                             choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))}
              else{
                radioButtons("selecactive",label=h6(gettext("Choose the active columns")),
                             choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose")) 
              },
              conditionalPanel(
                #        condition="input.selecactive=='choix'",
                condition=paste("input.selecactive=='",gettext("Choose"),"'",sep=''),
                if(is.null(colonnesup)){
                  selectInput("supvar",label=h6(gettext("Select the supplementary columns")),
                              choices=list(IdChoices=VariableChoices),
                              multiple=TRUE)}
                else{
                  selectInput("supvar",label=h6(gettext("Select the supplementary columns")),
                              choices=list(IdChoices=VariableChoices),
                              multiple=TRUE,selected=colonnesup)  
                }
              ),
              if(is.null(catsup)){
                if(length(QualiChoice)>1){
                  selectInput("supquali",label=h6(gettext("Select the supplementary categorical variables")),choices=list(Idqualisup=as.vector(QualiChoice)),multiple=TRUE)
                }
                if (length(QualiChoice)==1){
                  h6(gettext("Select the supplementary categorical variables"))
                  checkboxInput("supquali",QualiChoice,FALSE)
                }}
              else{
                if(length(QualiChoice)>1){
                  selectInput("supquali",label=h6(gettext("Select the supplementary categorical variables")),choices=list(Idqualisup=as.vector(QualiChoice)),multiple=TRUE,selected=catsup)
                }
                if (length(QualiChoice)==1){
                  h6(gettext("Select the supplementary categorical variables"))
                  checkboxInput("supquali",QualiChoice,TRUE)
                } 
              },
              br(),
              if(is.null(lignesup)){
                selectInput("rowsupl",label=h6(gettext("Select the supplementary rows")),choices=list(num=nom),multiple=TRUE)}
              else{
                selectInput("rowsupl",label=h6(gettext("Select the supplementary rows")),choices=list(num=nom),multiple=TRUE,selected=lignesup)
              }
            )
          ),
          wellPanel(
            div(align="center",checkboxInput("graph",gettext("Show graphs options"),FALSE)),
            conditionalPanel(
              condition="input.graph==true",
              fluidRow(
                column(5,uiOutput("NB1")),
                column(5,uiOutput("NB2"))),
              hr(),
              textInput("title1",h6(gettext("Title of the graph: ")),title1),
              if(is.null(Invisible)){
                #          selectInput("invis",h6(gettext("Invisible elements")),choices=list("Rows"="row","Columns"="col","Supplementary rows"="row.sup","Supplementary columns"="col.sup","Supplementary qualitative variable"="quali.sup"),multiple=TRUE)}
                selectInput("invis",h6(gettext("Invisible elements")),choices=list(gettext("Rows"),gettext("Columns"),gettext("Supplementary rows"),gettext("Supplementary columns"),gettext("Supplementary qualitative variables")),multiple=TRUE)}
              else{
                #          selectInput("invis",h6(gettext("Invisible elements")),choices=list("Rows"="row","Columns"="col","Supplementary rows"="row.sup","Supplementary columns"="col.sup","Supplementary qualitative variable"="quali.sup"),multiple=TRUE,selected=Invisible)
                selectInput("invis",h6(gettext("Invisible elements")),choices=list(gettext("Rows"),gettext("Columns"),gettext("Supplementary rows"),gettext("Supplementary columns"),gettext("Supplementary qualitative variables")),multiple=TRUE,selected=Invisible)
              },
              br(),
              sliderInput("cex",h6(gettext("Size of labels")),min=0.5,max=1.5,value=size,step=0.05),
              br(),
              uiOutput("col1"),
              uiOutput("col2"),
              uiOutput("col3"),
              uiOutput("col4"),
              br(),
              if(is.null(ellipses)){
                div(align="left",checkboxInput("el",h6(gettext("Draw confidence ellipses")),FALSE))
              }else{
                div(align="left",checkboxInput("el",h6(gettext("Draw confidence ellipses")),TRUE))},
              conditionalPanel(
                condition='input.el==true',
                uiOutput("ellipses")
              ),
              hr(),
              radioButtons("seleccol",h6(gettext("Draw columns according to:")), choices=list(gettext("No selection"),"Cos2"="cos2","Contribution"="contrib"),selected=selec1,inline=TRUE),
              conditionalPanel(
                condition="input.seleccol=='cos2'",
                if(selec1=="cos2"){
                  sliderInput("slider3",label=h6(gettext("Draw columns that have a cos2 greater than:")),
                              min=0,max=1,value=valueselec1,step=0.05)
                }
                else{
                  sliderInput("slider3",label=h6(gettext("Draw columns that have a cos2 greater than:")),
                              min=0,max=1,value=0,step=0.05) 
                }),
              conditionalPanel(
                condition="input.seleccol=='contrib'",
                uiOutput("contribcol")),
              br(),
              radioButtons("selecrow",h6(gettext("Draw rows according to:")), choices=list(gettext("No selection"),"Cos2"="cos2","Contribution"="contrib"),selected=selec2,inline=TRUE),
              conditionalPanel(
                condition="input.selecrow=='cos2'",
                if(selec2=="cos2"){sliderInput("slider4",label=h6(gettext("Draw rows that have a cos2 greater than:")),
                                               min=0,max=1,value=valueselec2,step=0.05)}
                else{sliderInput("slider4",label=h6(gettext("Draw rows that have a cos2 greater than:")),
                                 min=0,max=1,value=0,step=0.05)}),
              conditionalPanel(
                condition="input.selecrow=='contrib'",
                uiOutput("contribrow"))
            )
          ),
          wellPanel(
            h5(gettext("Save graphs as"),align="center"),
            radioButtons("paramdown","",
                         choices=list("PNG"="png","JPG"="jpg","PDF"="pdf"),selected="png")
          )
          ,width=3)
      ),  
      div(style = 'height: 80vh; overflow-y: auto;',
          mainPanel(
            tags$style(type = "text/css", "a{color: #2F0B3A;}"),
            tabsetPanel(id = "graph_sort",
                        tabPanel(gettext("Graphs"),
                                 br(),
                                 div(verbatimTextOutput("warn")),
                                 br(),
                                 div(align="center",plotOutput("map",width=550,height=550)),
                                 br(),
                                 conditionalPanel(
                                   condition="input.paramdown=='jpg'",
                                   p(downloadButton("downloadData1",gettext("Download as jpg")),align="center")),
                                 conditionalPanel(
                                   condition="input.paramdown=='png'",
                                   p(downloadButton("downloadData",gettext("Download as png")),align="center")),
                                 conditionalPanel(
                                   condition="input.paramdown=='pdf'",
                                   p(downloadButton("downloadData2",gettext("Download as pdf")),align="center"))),
                        tabPanel(gettext("Values"),
                                 br(),
                                 uiOutput("out22"),
                                 br(),
                                 conditionalPanel(
                                   #                               condition="input.out=='eig'", 
                                   condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
                                   div(align="center",tableOutput("sorties")),
                                   div(align="center",plotOutput("map3", width = "700", height="500"))),
                                 conditionalPanel(
                                   #                               condition="input.out=='CA'",
                                   condition=paste("input.out=='",gettext("Summary of outputs"),"'",sep=''),
                                   numericInput("nbele",h6(gettext("Number of elements to print")),value=10),
                                   verbatimTextOutput("summaryCA"),
                                   p(downloadButton("summary2",gettext("Download the summary")),align="center")
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out=='var'",
                                   condition=paste("input.out=='",gettext("Results for the columns"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties1")),
                                   h6("cos2"),
                                   div(align="center",tableOutput("sorties2")),
                                   h6("Contributions"),
                                   div(align="center",tableOutput("sorties3"))
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out=='ind'",
                                   condition=paste("input.out=='",gettext("Results for the rows"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties4")),
                                   h6("cos2"),
                                   div(align="center",tableOutput("sorties5")),
                                   h6("Contributions"),
                                   div(align="center",tableOutput("sorties6"))
                                 ),
                                 conditionalPanel(
                                   condition=paste("input.out=='",gettext("Results for the supplementary rows"),"'",sep=''),
                                   #                               condition="input.out=='suprow'",
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties7")),
                                   h6("cos2"),
                                   div(align="center",tableOutput("sorties8"))
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out=='supcol'",
                                   condition=paste("input.out=='",gettext("Results for the supplementary columns"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties9")),
                                   h6("cos2"),
                                   div(align="center",tableOutput("sorties10"))
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out=='qualico'",
                                   condition=paste("input.out=='",gettext("Results for the categorical variables"),"'",sep=''),
                                   div(align="center",tableOutput("sorties11"))
                                 )
                        ),
                        tabPanel(gettext("Summary of dataset"),
                                 br(),
                                 verbatimTextOutput("summary")),
                        
                        tabPanel(gettext("Data"),
                                 br(),
                                 dataTableOutput("JDD")
                        )
            )
            ,width=9)
      )
  )
)
