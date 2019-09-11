
output$facto_meta_pca_ui<-renderUI({
  values$Details_Data_FA
  return(
    selectInput(inputId = "facto_meta_pca",label = "Choose meta data field",choices = colnames(values$facto_meta))
  )
})



observe({
  validate(
    need(!is.null(input$facto_meta_pca),message=F)
  )
  x<-as.data.frame((as.matrix(values$facto_dtm)))
  x<-cbind(isolate(values$facto_meta[,input$facto_meta_pca]),x)
  colnames(x)[1]<-"X1"
  attach(x)
  x<-as.data.frame(aggregate(x[,2:dim(x)[2]],by=list(X1),FUN = sum))
  colnames(x)[1]<-input$facto_meta_pca

  if(is.data.frame(x)==TRUE){
    quanti_PCA=names(which(sapply(x,is.numeric)))
    quali_PCA=names(which(!(sapply(x,is.numeric))))
    if(length(quanti_PCA)<=2){
      shinyalert::shinyalert("not enough quantitative variables in your dataset",type="warning")
    }
    else{
      ##
      assign("nomData_PCA",paste0("Collection ",isolate(values$facto_info[[5]])),envir=.GlobalEnv)
      assign("newdata_PCA",x,envir=.GlobalEnv)
      values$newdata_PCA<-newdata_PCA
      assign("quantisup_PCA",NULL,envir=.GlobalEnv)
      assign("qualisup_PCA",NULL,envir=.GlobalEnv)
      assign("indsupl_PCA",NULL,envir=.GlobalEnv)
      assign("axe1_PCA",1,envir=.GlobalEnv)
      assign("axe2_PCA",2,envir=.GlobalEnv)
      assign("habillageind_PCA",NULL,envir=.GlobalEnv)
      assign("selection_PCA",gettext("No selection_PCA"),envir=.GlobalEnv)
      assign("selection_PCA2",NULL,envir=.GlobalEnv)
      assign("selection_PCA3",gettext("No selection_PCA"),envir=.GlobalEnv)
      assign("selection_PCA4",NULL,envir=.GlobalEnv)
      assign("size_PCA",1,envir=.GlobalEnv)
      assign("size_PCA2",1,envir=.GlobalEnv)
      assign("titre_PCA1",gettext("Individuals factor map (PCA)"),envir=.GlobalEnv)
      assign("titre_PCA2",gettext("Variables factor map (PCA)"),envir=.GlobalEnv)
      assign("ellipses_PCA",FALSE,envir=.GlobalEnv)
      assign("activeind_PCA","black",envir=.GlobalEnv)
      assign("supind_PCA","blue",envir=.GlobalEnv)
      assign("categ_PCA","magenta",envir=.GlobalEnv)
      assign("coloractvar_PCA","black",envir=.GlobalEnv)
      assign("colorsupvar_PCA","blue",envir=.GlobalEnv)
      assign("norme_PCA",TRUE,envir=.GlobalEnv)
      assign("poids_PCA1",NULL,envir=.GlobalEnv)
      assign("poids_PCA2",NULL,envir=.GlobalEnv)
      assign("quanti_PCA",names(which(sapply(newdata_PCA,is.numeric))),envir=.GlobalEnv)
      assign("quali_PCA",names(which(!(sapply(newdata_PCA,is.numeric)))),envir=.GlobalEnv)
      assign("VariableChoices_PCA",quanti_PCA,envir=.GlobalEnv)
      assign("nom_PCA",rownames(newdata_PCA),envir=.GlobalEnv)
      assign("num_PCA",c(1:length(nom_PCA)),envir=.GlobalEnv)
      assign("QualiChoice_PCA",quali_PCA,envir=.GlobalEnv)
      assign("IdChoices_PCA",c(1:length(VariableChoices_PCA)),envir=.GlobalEnv)
      assign("Idqualisup_PCA",c(1:length(QualiChoice_PCA)),envir=.GlobalEnv)
      
      values$Facto_ready_PCA<-TRUE
      values$Facto_new_Pca<-runif(n = 1,min = 0,max = 1) 
    }
  }
})







output$Details_Facto_PCA_UI<-renderUI({
  validate(
    need(values$Facto_ready_PCA==TRUE,message=FALSE)
  )
  values$Facto_new_Pca
  
  
  return(tagList(
    div(paste(gettext("PCA on the "),nomData_PCA),style="color:#2A0A29"),
    sidebarLayout(
      sidebarPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
            tags$head(
              tags$style("body {background-color: #EFFBFB; }"),
              tags$style(type='text/css', "#title1_PCA { height: 25px; }"),
              tags$style(type='text/css', "#title2_PCA { height: 25px; }")
              #tags$style(type='text/css', "#NB1_PCA { height: 20px; }")
            ),
            wellPanel(
              div(align="center",checkboxInput("pcaparam_PCA",gettext("Show PCA parameters"),FALSE)),
              conditionalPanel(
                condition="input.pcaparam_PCA==true",
                if(is.null(quantisup_PCA)){
                  radioButtons("selecactive_PCA",label=h6(gettext("Choose the active variables")),
                               choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))
                }
                else{
                  radioButtons("selecactive_PCA",label=h6(gettext("Choose the active variables")),
                               choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose"))
                },
                conditionalPanel(
                  #          condition="input.selecactive_PCA=='choix'",
                  condition=paste("input.selecactive_PCA=='",gettext("Choose"),"'",sep=''),
                  selectInput("supvar_PCA",label=gettext("select the supplementary quantitative variables"),
                              choices=list(IdChoices_PCA=VariableChoices_PCA),
                              selected=quantisup_PCA,multiple=TRUE)
                ),
                br(),      
                h6(gettext("select the supplementary categorical variables")),
                
                if(length(QualiChoice_PCA)>1){
                  if(is.null(qualisup_PCA)){
                    selectInput("supquali_PCA",label="",choices=list(Idqualisup_PCA=as.vector(QualiChoice_PCA)),multiple=TRUE)
                  }
                  else{
                    selectInput("supquali_PCA",label="",choices=list(Idqualisup_PCA=as.vector(QualiChoice_PCA)),multiple=TRUE,selected=qualisup_PCA)  
                  }
                }
                else if (length(QualiChoice_PCA)==1){
                  if(is.null(qualisup_PCA)){
                    checkboxInput("supquali_PCA",QualiChoice_PCA,FALSE)
                  }
                  else{
                    checkboxInput("supquali_PCA",QualiChoice_PCA,TRUE)
                  }
                }
                else if(length(QualiChoice_PCA)==0){
                  p(gettext("No categorical variable in your dataset"))
                },
                
                br(),
                h6(gettext("select the supplementary individuals")),
                if(is.null(indsupl_PCA)){
                  selectInput("indsup_PCA","",choices=list(num_PCA=nom_PCA), multiple=TRUE)
                }
                else{
                  selectInput("indsup_PCA","",choices=list(num_PCA=nom_PCA), multiple=TRUE,selected=indsupl_PCA)
                },
                #          br(),
                checkboxInput("nor_PCA",gettext("Scale data to unit value"),norme_PCA)
              )
            ),
            wellPanel(
              div(align="center",checkboxInput("graph_PCA",gettext("Show graphs options"),FALSE)),
              conditionalPanel(
                condition="input.graph_PCA==true",
                fluidRow(
                  column(5,uiOutput("NB1_PCA")),
                  column(5,uiOutput("NB2_PCA"))),
                hr(),
                div(align="center",radioButtons("ind_var_PCA","",
                                                #                     choices=list("graph_PCA of Individuals"="Ind","graph_PCA of Variables"="Var"),selected="var",inline=TRUE)),
                                                choices=list(gettext("graph of individuals"),gettext("graph of variables")),selected=gettext("graph of individuals"),inline=TRUE)),
                #          br(),
                conditionalPanel(
                  #          condition="input.ind_var_PCA=='Ind'",
                  condition=paste("input.ind_var_PCA=='",gettext("graph of individuals"),"'",sep=''),
                  textInput("title1_PCA",h6(gettext("Title of the graph: ")), titre_PCA1),
                  sliderInput("cex_PCA",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size_PCA,step=0.05,ticks=FALSE),
                  selectInput("select_PCA",label=h6(gettext("Draw individuals according to:")),
                              choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="contrib",gettext("Manual")),selected=selection_PCA),
                  conditionalPanel(
                    condition="input.select_PCA=='cos2'",
                    if(selection_PCA=="cos2"){
                      div(align="center",sliderInput("slider1_PCA", label = "cos2",
                                                     min = 0, max = 1, value =as.numeric(selection_PCA2),step=0.05))}
                    else{
                      div(align="center",sliderInput("slider1_PCA", label = "cos2",
                                                     min = 0, max = 1, value =0,step=0.05))
                    }),
                  conditionalPanel(
                    condition="input.select_PCA=='contrib'",
                    if(selection_PCA=="contrib"){
                      div(align="center",sliderInput("slider0_PCA", label = gettext("Number of the most contributive individuals"),
                                                     min = 1, max = length(nom_PCA), value =as.numeric(selection_PCA2),step=1))}
                    else{
                      div(align="center",sliderInput("slider0_PCA", label = gettext("Number of the most contributive individuals"),
                                                     min = 1, max = length(nom_PCA), value =length(nom_PCA),step=1)) 
                    }),
                  conditionalPanel(
                    condition=paste("input.select_PCA=='",gettext("Manual"),"'",sep=''),
                    #            condition="input.select_PCA=='Manuel'",
                    if(selection_PCA==gettext("Manual")){
                      selectInput("indiv_PCA",label=gettext("select individuals:"),
                                  choices=list(num_PCA=nom_PCA),multiple=TRUE,selected=selection_PCA2)}
                    else{
                      selectInput("indiv_PCA",label=gettext("select individuals:"),
                                  choices=list(num_PCA=nom_PCA),multiple=TRUE)
                    }),
                  #colourInput("colour1","Colour of active points",value="blue"),
                  colourpicker::colourInput("coloract_PCA", h6(gettext("Choose colour for active individuals")), activeind_PCA),
                  uiOutput("colourn2_PCA"),
                  uiOutput("colourn3_PCA"),
                  if(is.null(habillageind_PCA)){
                    checkboxInput("habi_PCA",gettext("Points colour depend on categorical variable"),FALSE)
                  }
                  else{
                    checkboxInput("habi_PCA",gettext("Points colour depend on categorical variable"),TRUE)
                  },
                  conditionalPanel(
                    condition="input.habi_PCA==true",
                    uiOutput("habillage2_PCA"),
                    uiOutput("ellipses_PCA")
                  )
                ),
                conditionalPanel(
                  #          condition="input.ind_var_PCA=='Var'",
                  condition=paste("input.ind_var_PCA=='",gettext("graph of variables"),"'",sep=''),
                  textInput("title2_PCA",h6(gettext("Title of the graph:")), titre_PCA2),
                  sliderInput("cex2_PCA",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size_PCA2,step=0.05,ticks=FALSE),
                  selectInput("select0_PCA",label=h6(gettext("Draw variables according to:")),
                              choices=list(gettext("No selection_PCA"),"cos2"="cos2","Contribution"="contrib"),selected=selection_PCA3),
                  conditionalPanel(
                    condition="input.select0_PCA=='contrib'",
                    uiOutput("slider3_PCA")
                  ),
                  conditionalPanel(
                    condition="input.select0_PCA=='cos2'",
                    if(selection_PCA3=="cos2"){
                      div(align="center",sliderInput("slider00_PCA", label = "cos2",
                                                     min = 0, max = 1, value =as.numeric(selection_PCA4),step=0.05))  
                    }
                    else{
                      div(align="center",sliderInput("slider00_PCA", label = "cos2",
                                                     min = 0, max = 1, value =0,step=0.05))}
                  ),
                  colourpicker::colourInput("coloractvar_PCA", h6(gettext("Choose colour for active variables")), coloractvar_PCA),
                  uiOutput("varsu_PCA")
                )
              )
            ),
            wellPanel(
              h5(gettext("Save graphs as"),align="center"),
              radioButtons("paramdown_PCA","",
                           choices=list("PNG"="png","JPG"="jpg","PDF"="pdf"),selected="png")
            )
        )
        ,width=3),
      
      mainPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
            tabsetPanel(id = "graph_sort_PCA",
                        tabPanel(gettext("Graphs"),
                                 fluidRow(
                                   br(),
                                   column(width = 6,plotOutput("map2_PCA", width = "500", height="500"),
                                          #                             div(align = "center",plotOutput("map2_PCA", width = 500, height=500)),
                                          br(),
                                          conditionalPanel(
                                            condition="input.paramdown_PCA=='jpg'",
                                            p(downloadButton("downloadData4_PCA",gettext("Download as jpg")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_PCA=='png'",
                                            p(downloadButton("downloadData3_PCA",gettext("Download as png")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_PCA=='pdf'",
                                            p(downloadButton("downloadData5_PCA",gettext("Download as pdf")),align="center")),
                                          br(),align="center"),
                                   column(width = 6,plotOutput("map_PCA", width = "500",height="500"),
                                          #                             div(align="center",plotOutput("map", width = 500, height=500)),
                                          br(),
                                          conditionalPanel(
                                            condition="input.paramdown_PCA=='jpg'",
                                            p(downloadButton("downloadData1_PCA",gettext("Download as jpg")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_PCA=='png'",
                                            p(downloadButton("downloadData_PCA",gettext("Download as png")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_PCA=='pdf'",
                                            p(downloadButton("downloadData2_PCA",gettext("Download as pdf")),align="center"))
                                          ,align="center"))),
                        
                        tabPanel(gettext("Values"),
                                 br(),
                                 uiOutput("out22_PCA"),
                                 br(),
                                 conditionalPanel(
                                   #                               condition="input.out_PCA=='eig'",
                                   condition=paste("input.out_PCA=='",gettext("Eigenvalues"),"'",sep=''),
                                   div(align="center",tableOutput("sorties_PCA")),
                                   plotOutput("map3_PCA", width = "700", height="500")
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out_PCA=='resvar'",
                                   condition=paste("input.out_PCA=='",gettext("Results of the variables"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties2_PCA")),
                                   br(),
                                   h6("Contributions"),
                                   div(align="center",tableOutput("sorties3_PCA")),
                                   br(),
                                   h6("Cos2"),
                                   div(align="center",tableOutput("sorties4_PCA"))),
                                 conditionalPanel(
                                   #                               condition="input.out_PCA=='resind'",
                                   condition=paste("input.out_PCA=='",gettext("Results of the individuals"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties22_PCA")),
                                   br(),
                                   h6("Contributions"),
                                   div(align="center",tableOutput("sorties33_PCA")),
                                   br(),
                                   h6("Cos2"),
                                   div(align="center",tableOutput("sorties44_PCA"))),
                                 conditionalPanel(
                                   #                               condition="input.out_PCA=='ACP'",
                                   condition=paste("input.out_PCA=='",gettext("Summary of outputs"),"'",sep=''),
                                   numericInput("nbele_PCA",gettext("Number of elements to print"),value=10),
                                   verbatimTextOutput("summaryPCA"),
                                   p(downloadButton("summary2_PCA",gettext("Download the summary_PCA")),align="center")
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out_PCA=='varsup'",
                                   condition=paste("input.out_PCA=='",gettext("Results of the supplementary variables"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties23_PCA")),
                                   h6("Correlations"),
                                   div(align="center",tableOutput("sorties32_PCA"))
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out_PCA=='quali_PCAco'",
                                   condition=paste("input.out_PCA=='",gettext("Results of the categorical variables"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties12_PCA")),
                                   h6("V-test"),
                                   div(align="center",tableOutput("sorties13_PCA"))
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out_PCA=='supind_PCA'",
                                   condition=paste("input.out_PCA=='",gettext("Results of the supplementary individuals"),"'",sep=''),
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties36_PCA")),
                                   h6("Cos2"),
                                   div(align="center",tableOutput("sorties37_PCA"))
                                 )
                        ),
                        tabPanel(gettext("Automatic description of axes"),
                                 br(),
                                 radioButtons("Dim_PCA",label=gettext("Choose the dimensions"),choices=list("Dimension 1"="Dim1","Dimension 2"="Dim2","Dimension 3"="Dim3"),selected="Dim1"),
                                 conditionalPanel(
                                   condition="input.Dim_PCA=='Dim1'",
                                   p("Quantitative"),
                                   div(align="center",tableOutput("sortieDimdesc3_PCA")),
                                   p("Qualitative"),
                                   div(align="center",tableOutput("sortieDimdesc4_PCA"))
                                   
                                 ),
                                 br(),
                                 conditionalPanel(
                                   condition="input.Dim_PCA=='Dim2'",
                                   p("Quantitative"),
                                   div(align="center",tableOutput("sortieDimdesc33_PCA")),
                                   p("Qualitative"),
                                   div(align="center",tableOutput("sortieDimdesc44_PCA"))
                                 ),
                                 br(),
                                 conditionalPanel(
                                   condition="input.Dim_PCA=='Dim3'",
                                   p("Quantitative"),
                                   div(align="center",tableOutput("sortieDimdesc333_PCA")),
                                   p("Qualitative"),
                                   div(align="center",tableOutput("sortieDimdesc444_PCA"))
                                 )
                        ),
                        tabPanel(gettext("Summary of dataset"),
                                 br(),
                                 verbatimTextOutput("summary_PCA"),
                                 br(),
                                 selectInput("bam_PCA",h6(gettext("Graphs for")),choices=list(IdChoices_PCA=VariableChoices_PCA),multiple=FALSE),
                                 plotOutput("histo_PCA")),
                        
                        tabPanel(gettext("Data"),
                                 br(),
                                 dataTableOutput("JDD_PCA")
                        )
            )
        )
        ,width=9)
    )
    
    
  ))    
})



values_PCA=reactive({
  if (input$selecactive_PCA==gettext("All")){
    data.selec=values$newdata_PCA[,VariableChoices_PCA]
  }
  else{
    validate(
      need(length(input$supvar_PCA)>0, gettext("Please select at least one supplementary variable"))
    )
    data.selec=values$newdata_PCA[,c(getactive_PCA())]
  }
  
  if(length(QualiChoice_PCA)==0){
    choixquali_PCA=NULL
  }
  else if (length(QualiChoice_PCA)==1){
    if(input$supquali_PCA==FALSE){
      choixquali_PCA=NULL
    }
    else{
      data.selec=cbind(data.selec,values$newdata_PCA[,QualiChoice_PCA])
      colnames(data.selec)[dim(data.selec)[2]]=QualiChoice_PCA
      choixquali_PCA=length(data.selec)
    }
  }
  else{
    if(length(input$supquali_PCA)==0){
      choixquali_PCA=NULL
    }
    else{
      data.selec=cbind(data.selec,values$newdata_PCA[,input$supquali_PCA])
      if(length(input$supquali_PCA)==1){
        choixquali_PCA=length(data.selec)
        colnames(data.selec)[choixquali_PCA]=input$supquali_PCA
      }
      else{
        choixquali_PCA=seq((dim(data.selec)[2]-length(input$supquali_PCA)+1),dim(data.selec)[2])
        colnames(data.selec)[choixquali_PCA]=input$supquali_PCA
      }
    }
  }
  if(length(input$supvar_PCA)==0){
    choixquanti_PCA=NULL
  }
  else {
    data.selec=cbind(data.selec,values$newdata_PCA[,input$supvar_PCA])
    if(length(input$supvar_PCA)==1){
      choixquanti_PCA=length(data.selec)
      colnames(data.selec)[choixquanti_PCA]<-input$supvar_PCA
    }
    else{
      choixquanti_PCA=seq((dim(data.selec)[2]-length(input$supvar_PCA)+1),dim(data.selec)[2])
    }
  }
  if (length(input$habiller)==2 && input$habi_PCA==TRUE){
    data.selec <- data.frame(data.selec,newCol=paste(values$newdata_PCA[,input$habiller[1]],values$newdata_PCA[,input$habiller[2]],sep="/"))
    choixquali_PCA=c(choixquali_PCA,dim(data.selec)[2])
  }
  if(length(input$indsup_PCA)==0){
    suple=NULL
  }
  else{
    # suple=c()
    # for (i in 1:length(nom_PCA)){
    # if(nom_PCA[i]%in%input$indsup_PCA){
    # suple=c(suple,i)
    # }
    # }
    suple=which(nom_PCA%in%input$indsup_PCA)
  }
  list(res.PCA=(PCA(data.selec,quali.sup=choixquali_PCA,quanti.sup=choixquanti_PCA,scale.unit=input$nor_PCA,graph=FALSE,ncp=max(5,as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),ind.sup=suple,row.w=poids_PCA1,col.w=poids_PCA2)),DATA=(data.selec),choixquant=(choixquanti_PCA),choixqual=(choixquali_PCA),choixsuple=(suple))
})

Plot1_PCA <- reactive({
  validate(
    need(input$NB1_PCA != input$NB2_PCA, gettext("Please select two different dimensions"))
  )
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(input$select0_PCA=="cos2"){
    if(input$slider00_PCA!=1){
      selecindiv=paste("cos2 ",input$slider00_PCA)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  if(input$select0_PCA==gettext("No selection_PCA")){
    selecindiv=NULL
    selecindivText="NULL"
  }
  if(input$select0_PCA=="contrib"){
    selecindiv=paste("contrib ",input$slider4)
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  if(is.null(input$colorsupvar_PCA)){
    colo="blue"
  }else{
    colo=input$colorsupvar_PCA
  }
  list(PLOT=(plot.PCA(values_PCA()$res.PCA,axes=c(as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),choix="var",select=selecindiv,unselect=0,col.quanti.sup=colo,col.var=input$coloractvar_PCA,cex=input$cex2_PCA,cex.main=input$cex2_PCA,cex.axis=input$cex2_PCA,title=input$title2_PCA)),SELECTION=(selecindiv),selecindivText=(selecindivText))
})

output$map_PCA <- renderPlot({
  p <- Plot1_PCA()$PLOT
})

observe({
  validate(need(!is.null(input$habi_PCA),message=F))
  if(input$habi_PCA==FALSE){
    updateCheckboxInput(session, "elip_PCA", value = FALSE)
  }
})

Plot2_PCA <- reactive({
  validate(
    need(input$NB1_PCA != input$NB2_PCA, gettext("Please select two different dimensions"))
  )
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  validate(
    need(input$habiller == TRUE || input$habiller == FALSE || length(input$habiller)<=2,gettext("Please select maximum 2 variables as habillage"))
  )
  if(!is.null(input$elip_PCA)){
    validate(
      need(!(input$habi_PCA==FALSE&&input$elip_PCA==TRUE),"")
    )
  }
  if(input$select_PCA=="cos2"){
    if(input$slider1_PCA!=1){
      selecindiv=paste("cos2 ",input$slider1_PCA)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivtext=paste0("'",selecindiv,"'")
  }
  if(input$select_PCA==gettext("No selection")){
    selecindiv=NULL
    selecindivtext="NULL"
  }
  if(input$select_PCA=="contrib"){
    selecindiv=paste("contrib ",input$slider0_PCA)
    selecindivtext=paste0("'",selecindiv,"'")
  }
  if(input$select_PCA==gettext("Manual")){
    selecindiv=c(input$indiv_PCA)
  }
  if(input$supquali_PCA==FALSE || length(QualiChoice_PCA)==0 || length(input$supquali_PCA)==0 || input$habi_PCA==FALSE){
    hab="none"
    #        colquali_PCA="magenta"
  }
  else if(length(QualiChoice_PCA)==1 && input$supquali_PCA==TRUE){
    if(input$habi_PCA==TRUE){
      hab=QualiChoice_PCA
      #          colquali_PCA="blue"
    }
    else{
      hab="none"
      #          colquali_PCA="magenta"
    }
  }
  else if (length(input$supquali_PCA)==1){
    if(input$habi_PCA==TRUE){
      hab=input$supquali_PCA
      #          colquali_PCA="blue"
    }
    else{
      hab="none"
      #          colquali_PCA="magenta"
    }
  }
  if(length(input$supquali_PCA)>1){
    if(length(input$habiller)==0){
      hab="none"
      #          colquali_PCA="magenta"
    }
    if (length(input$habiller)==1 & input$habi_PCA==TRUE){
      hab=as.character(input$habiller)
      #          colquali_PCA="blue"
    }
    if (length(input$habiller)==2 & input$habi_PCA==TRUE){
      hab=dim(values_PCA()$DATA)[2]
      #          colquali_PCA="blue"
    }
  }
  
  if(input$select_PCA==gettext("Manual")){
    if(length(input$indiv_PCA)==0){
      selecindivtext="NULL"
    }
    if(length(input$indiv_PCA)>1){
      # vec<-NULL
      # vec<-paste(vec,"'",selecindiv[1],"'",sep="")
      # for (i in 2:(length(selecindiv))){
      # vec<-paste(vec,paste("'",selecindiv[i],"'",sep=""),sep=",")
      # }
      vec<- paste("'",paste(selecindiv,collapse="','"),"'",sep="")
      selecindivtext<-paste("c(",vec,")",sep="")
    }
    else if (length(input$indiv_PCA)==1){
      selecindivtext=paste0("'",c(input$indiv_PCA),"'")
    }
  }
  if(!is.null(input$colorsup)){
    colors=input$colorsup
  }else{
    colors="blue"
  }
  if(!is.null(input$colorquali_PCA)){
    colorss=input$colorquali_PCA
  }else{
    colorss="magenta"
  }
  if(!is.null(input$elip_PCA)&&input$elip_PCA==TRUE){
    if(!is.null(values_PCA()$res.PCA$call$ind.sup)){
      aa=cbind.data.frame(values_PCA()$DATA[-c(values_PCA()$res.PCA$call$ind.sup),hab],values_PCA()$res.PCA$ind$coord)
    }else{
      aa=cbind.data.frame(values_PCA()$DATA[,hab],values_PCA()$res.PCA$ind$coord)
    }
    bb=coord.ellipse(aa,bar=TRUE)
    formula=plot.PCA(values_PCA()$res.PCA,axes=c(as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),choix="ind",cex=input$cex_PCA,cex.main=input$cex_PCA,cex.axis=input$cex_PCA,select=selecindiv,habillage=hab,title=input$title1_PCA,ellipse=bb,col.ind=input$coloract_PCA,col.ind.sup=colors,col.quali=colorss)
    
  }else{
    #browser()
    formula=plot.PCA(values_PCA()$res.PCA,axes=c(as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),choix="ind",cex=input$cex_PCA,cex.main=input$cex_PCA,cex.axis=input$cex_PCA,select=selecindiv,habillage=hab,title=input$title1_PCA,col.ind=input$coloract_PCA,col.ind.sup=colors,col.quali=colorss)
  }
  colquali_PCA=colorss
  list(PLOT=(formula),SELECTION2=(selecindiv),SELECTION3=(selecindivtext),HABILLAGE=(hab),colquali_PCA=(colorss),colindsup=(colors), text="")      
})

output$map2_PCA <- renderPlot({
  p <- Plot2_PCA()$PLOT
  
})

output$colourn2_PCA=renderUI({
  sup=values_PCA()$choixsuple
  if(!is.null(sup)){
    if(!is.null(supind_PCA)){
      return(colourpicker::colourInput("colorsup", h6(gettext("Choose colour for supplementary individuals")), supind_PCA))
    }else{
      return(colourpicker::colourInput("colorsup", h6(gettext("Choose colour for supplementary individuals")), "blue")) 
    }
  }
})

output$colourn3_PCA=renderUI({
  sup=values_PCA()$choixqual
  if(!is.null(sup)){
    if(!is.null(categ_PCA)){
      return(colourpicker::colourInput("colorquali_PCA", h6(gettext("Choose colour for the categories")), categ_PCA))
    }else{
      return(colourpicker::colourInput("colorquali_PCA", h6(gettext("Choose colour for the categories")), "magenta"))
    }
  }
})





codeGraphVar<-function(){
  
  if(length(input$slider4)==0){
    selection_PCA="NULL"
  }
  else{
    selection_PCA=Plot1_PCA()$selecindivText
  }
  if(is.null(input$colorsupvar_PCA)){
    colo="blue"
  }else{
    colo=input$colorsupvar_PCA
  }
  Call1=paste("plot.PCA(res.PCA,axes=c(",input$NB1_PCA,",",input$NB2_PCA,"),choix='var',select=",selection_PCA,",cex_PCA=",input$cex2_PCA,",cex.main=",input$cex2_PCA,",cex.axis=",input$cex2_PCA,",title='",input$title2_PCA,"',unselect=0,col.quanti_PCA.sup='",colo,"',col.var='",input$coloractvar_PCA,"')",sep="")
  return(Call1)
}

codeellipses_PCA=function(){
  Datasel<-values_PCA()$DATA
  indsupl_PCA<-values_PCA()$choixsuple
  
  vec<-NULL
  for (i in 1:length(colnames(Datasel))){
    vec<-c(vec,colnames(Datasel)[i])
  }
  vec2<-NULL
  vec2<-paste(vec2,"'",vec[1],"'",sep="")
  for (i in 2:(length(vec))){
    vec2<-paste(vec2,paste("'",vec[i],"'",sep=""),sep=",")
  }
  vecfinal<-paste(nomData_PCA,"[,c(",vec2,")","]",sep="")
  vec=vecfinal
  if(input$supquali_PCA==FALSE || length(QualiChoice_PCA)==0 || length(input$supquali_PCA)==0 || input$habi_PCA==FALSE){
    hab="none"
    colquali_PCA="magenta"
  }
  else if(length(QualiChoice_PCA)==1 && input$supquali_PCA==TRUE){
    if(input$habi_PCA==TRUE){
      hab=QualiChoice_PCA
      colquali_PCA="blue"
    }
    else{
      hab="none"
      colquali_PCA="magenta"
    }
  }
  else if (length(input$supquali_PCA)==1){
    if(input$habi_PCA==TRUE){
      hab=input$supquali_PCA
      colquali_PCA="blue"
    }
    else{
      hab="none"
      colquali_PCA="magenta"
    }
  }
  if(length(input$supquali_PCA)>1){
    if(length(input$habiller)==0){
      hab="none"
      colquali_PCA="magenta"
    }
    if (length(input$habiller)==1 & input$habi_PCA==TRUE){
      hab=as.character(input$habiller)
      colquali_PCA="blue"
    }
    if (length(input$habiller)==2 & input$habi_PCA==TRUE){
      hab=dim(values_PCA()$DATA)[2]
      colquali_PCA="blue"
    }
  }
  phrase1=paste("aa=cbind.data.frame(",vec,"[,'",hab,"'],res.PCA$ind$coord)",sep="")
  #phrase2="bb=coord.ellipse(aa,bar=TRUE)"
  #phrasefinal=paste(phrase1,phrase2,sep="\n")
  return(phrase1)
}

codeGraphInd<-function(){
  if (length(input$habiller)<=1 & input$habi_PCA==TRUE || input$habi_PCA==FALSE){
    hab=paste("'",Plot2_PCA()$HABILLAGE,"'",sep="")
  }
  else if (length(input$habiller)==2 & input$habi_PCA==TRUE){
    hab=Plot2_PCA()$HABILLAGE
  }
  if(!is.null(input$elip_PCA)&&input$elip_PCA==TRUE){
    Call2=paste("plot.PCA(res.PCA,","axes=c(",input$NB1_PCA,",",input$NB2_PCA,"),choix='ind',select=",Plot2_PCA()$SELECTION3,",habillage=",hab,",title='",input$title1_PCA,"',cex_PCA=",input$cex_PCA,",cex.main=",input$cex_PCA,",cex.axis=",input$cex_PCA,",col.ind='",input$coloract_PCA,"',col.ind.sup='",Plot2_PCA()$colindsup,"',col.quali='",Plot2_PCA()$colquali_PCA,"',ellipse=bb)",sep="")
  }else{
    Call2=paste("plot.PCA(res.PCA,","axes=c(",input$NB1_PCA,",",input$NB2_PCA,"),choix='ind',select=",Plot2_PCA()$SELECTION3,",habillage=",hab,",title='",input$title1_PCA,"',cex_PCA=",input$cex_PCA,",cex.main=",input$cex_PCA,",cex.axis=",input$cex_PCA,",col.ind='",input$coloract_PCA,"',col.ind.sup='",Plot2_PCA()$colindsup,"',col.quali='",Plot2_PCA()$colquali_PCA,"')",sep="")
  }
  return(Call2)
}

##### Fin de la fonction recuperation du code_PCA


output$out22_PCA=renderUI({
  #      choix=list("Summary of PCA"="ACP","Eigenvalues"="eig","Results of the variables"="resvar","Results of the individuals"="resind")
  choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
  if(!is.null(values_PCA()$choixsuple)){
    #        choix=c(choix,"Results of the supplementary individuals"="supind_PCA")
    choix=c(choix,gettext("Results of the supplementary individuals"))
  }
  if(!is.null(values_PCA()$choixquant)){
    #        choix=c(choix,"Results of the supplementary variables"="varsup")
    choix=c(choix,gettext("Results of the supplementary variables"))
  }
  if(!is.null(values_PCA()$choixqual)){
    #        choix=c(choix,"Results of the categ_PCAorical variables"="quali_PCAco")
    choix=c(choix,gettext("Results of the categorical variables"))
  }
  radioButtons("out_PCA",gettext("Which outputs do you want?"),
               #                   choices=choix,selected="ACP",inline=TRUE)
               choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
})

getactive_PCA=function(){
  if(input$selecactive_PCA==gettext("Choose")){
    sup=NULL
    if(length(input$supvar_PCA)==0){
      activevar=VariableChoices_PCA
    }
    else{
      # for (i in 1:length(VariableChoices_PCA)){
      # if(VariableChoices_PCA[i]%in%input$supvar_PCA){
      # sup=c(sup,i)
      # }
      # }
      sup=which(VariableChoices_PCA%in%input$supvar_PCA)
      if (length(sup)==0) sup=NULL
      activevar=VariableChoices_PCA[-sup]
    }
    return(activevar)
  }
}


output$NB1_PCA=renderUI({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(input$selecactive_PCA==gettext("All") || length(getactive_PCA())>5){
    # return(selectInput("NB1_PCA", label = h6(gettext("x axis")), 
    # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5,"6"=6,"7"=7), selected = axe1_PCA,width='80%'))
    return(textInput("NB1_PCA", label = h6(gettext("x axis")), axe1_PCA,width='50%'))
  }
  else{
    baba=c(1:length(getactive_PCA()))
    return(selectInput("NB1_PCA",label=h6(gettext("x axis")), choices=baba,selected=axe1_PCA,width='80%'))
  }
})

output$NB2_PCA=renderUI({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(input$selecactive_PCA==gettext("All") || length(getactive_PCA())>5){
    # return(selectInput("NB2_PCA", label = h6(gettext("y axis")), 
    # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5,"6"=6,"7"=7), selected = axe2_PCA,width='80%'))
    return(textInput("NB2_PCA", label = h6(gettext("y axis")), axe2_PCA,width='50%'))
  }
  else{
    baba=c(1:length(getactive_PCA()))
    return(selectInput("NB2_PCA",label=h6(gettext("y axis")), choices=baba,selected=axe2_PCA,width='80%'))
  }
})

code_PCA<-function(){
  vecquant<-values_PCA()$choixquant
  choixqual<-values_PCA()$choixqual
  Datasel<-values_PCA()$DATA
  indsupl_PCA<-values_PCA()$choixsuple
  data1=values$newdata_PCA
  data2=Datasel
  test=identical(data1,data2)
  # vec<-NULL
  # for (i in 1:length(colnames(Datasel))){
  # vec<-c(vec,colnames(Datasel)[i])
  # }
  vec <-colnames(Datasel)
  # vec2<-NULL
  # vec2<-paste(vec2,"'",vec[1],"'",sep="")
  # for (i in 2:(length(vec))){
  # vec2<-paste(vec2,paste("'",vec[i],"'",sep=""),sep=",")
  # }
  vec2<-paste("'",paste(colnames(Datasel),collapse="','"),"'",sep="")
  if(test==FALSE){
    vecfinal<-paste(nomData,"[,c(",vec2,")","]",sep="")
  }else{
    vecfinal=nomData
  }
  
  # vec4<-NULL
  # vec4<-paste(vec4,vecquant[1],sep="")
  # for (i in 2:(length(vecquant))){
  # vec4<-paste(vec4,vecquant[i],sep=",")
  # }
  vec4 <- paste(vecquant,collapse=",")
  vecquant1<-paste("c(",vec4,")",sep="")
  vecquant2<-vecquant
  
  vecqual<-choixqual
  # vec5<-NULL
  # vec5<-paste(vec5,vecqual[1],sep="")
  # for (i in 2:(length(vecqual))){
  # vec5<-paste(vec5,vecqual[i],sep=",")
  # }
  vec5 <- paste(vecqual,collapse=",")
  vecqual1<-paste("c(",vec5,")",sep="")
  vecqual2<-vecqual
  
  # vecind<-NULL
  # vecind<-paste(vecind,indsupl_PCA[1],sep="")
  # for (i in 2:(length(indsupl_PCA))){
  # vecind<-paste(vecind,indsupl_PCA[i],sep=",")
  # }
  vecind <- paste(indsupl_PCA,collapse=",")
  vecind1<-paste("c(",vecind,")",sep="")
  vecind2<-indsupl_PCA
  vec<-vecfinal
  
  if(length(input$indsup)==0){
    indsupl_PCA<-"NULL"
  }
  else if(length(input$indsup)==1){
    indsupl_PCA<-vecind2
  }
  else if(length(input$indsup)>1){
    indsupl_PCA<-vecind1
  }
  
  
  if(length(input$supvar)>1){
    vecquant<-vecquant1
  }
  else if(length(input$supvar)==1){
    vecquant<-vecquant2
  }
  else if(length(input$supvar)==0){
    vecquant<-"NULL"
  }
  
  if (length(input$supquali)>1){ 
    vecqual<-vecqual1
  }
  if(length(QualiChoice)==1){
    if(input$supquali==TRUE){
      vecqual<-vecqual2 
    }
    else{
      vecqual<-"NULL"  
    }
  }
  
  else if(length(QualiChoice)>1){
    if(length(input$supquali)==1){
      vecqual<-vecqual2  
    }
    else if (length(input$supquali)>1){ 
      vecqual<-vecqual1
    }
    else if (length(input$supquali)==0){ 
      vecqual<-"NULL"
    }  
  }
  else if(length(QualiChoice)==0){
    vecqual<-"NULL"
  }
  if(!is.null(poids_PCA1)){
    prow=paste(",row.w=c(",paste(poids_PCA1,collapse=","),")",sep="")
  }
  if(!is.null(poids_PCA2)){
    pcol=paste(",col.w=c(",paste(poids_PCA2,collapse=","),")",sep="")
  }
  if(!is.null(poids_PCA1)&&!is.null(poids_PCA2)){
    Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl_PCA,prow,pcol,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep="")) 
  }else if (!is.null(poids_PCA1)&&is.null(poids_PCA2)){
    Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl_PCA,prow,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep="")) 
  }else if (is.null(poids_PCA1)&&!is.null(poids_PCA1)){
    Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl_PCA,pcol,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep="")) 
  }else{
    Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl_PCA,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep=""))
  }
  return(Call1)
}



output$sorties_PCA=renderTable({
  return(as.data.frame(values_PCA()$res.PCA$eig))
},rownames=TRUE)

output$sorties12_PCA=renderTable({
  validate(
    need((length(input$supquali_PCA)>0 || input$supquali_PCA==TRUE), gettext("No categorical variables selected"))
  )
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$quali.sup$coord))
},rownames=TRUE)

output$sorties13_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  validate(
    need((length(input$supquali_PCA)>0 || input$supquali_PCA==TRUE), gettext("No categorical variables selected"))
  )
  return(as.data.frame(values_PCA()$res.PCA$quali.sup$v.test))
},rownames=TRUE)

output$sorties2_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$var$coord))
},rownames=TRUE)

output$sorties22_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$ind$coord))
},rownames=TRUE)

output$sorties23_PCA=renderTable({
  validate(
    need(length(input$supvar_PCA)!=0, gettext("No supplementary quantitative variables"))
  )
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$quanti.sup$coord))
},rownames=TRUE)

output$sorties32_PCA=renderTable({
  validate(
    need(length(input$supvar_PCA)!=0, gettext("No supplementary quantitative variables"))
  )
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$quanti.sup$cor))
},rownames=TRUE)

output$sorties36_PCA=renderTable({
  validate(
    need(length(input$indsup_PCA)!=0, gettext("No supplementary individuals"))
  )
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variables"))
  )
  return(as.data.frame(values_PCA()$res.PCA$ind.sup$coord))
},rownames=TRUE)

output$sorties37_PCA=renderTable({
  validate(
    need(length(input$indsup_PCA)!=0, gettext("No supplementary individuals"))
  )
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$ind.sup$cos2))
},rownames=TRUE)


output$sorties3_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$var$contrib))
},rownames=TRUE)

output$sorties33_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$ind$contrib))
},rownames=TRUE)

output$sorties4_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$var$cos2))
},rownames=TRUE)

output$sorties44_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_PCA()$res.PCA$ind$cos2))
},rownames=TRUE)

output$sortieDimdesc3_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>2 || input$selecactive_PCA==gettext("All"),gettext("Please select more variables"))
  )
  validate(need(length(dimdesc(values_PCA()$res.PCA))>0,gettext("No quantitative variable describes axis 1")))
  return(as.data.frame(dimdesc(values_PCA()$res.PCA)[[1]]$quanti))
},rownames=TRUE)

output$sortieDimdesc4_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>2 || input$selecactive_PCA==gettext("All"),gettext("Please select more variables"))
  )
  validate(need(length(dimdesc(values_PCA()$res.PCA))>0,gettext("No categorical variable describes axis 1")))
  return(as.data.frame(dimdesc(values_PCA()$res.PCA)[[1]]$quali))
},rownames=TRUE)

#DIM2

output$sortieDimdesc33_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>2 || input$selecactive_PCA==gettext("All"),gettext("Please select more variables"))
  )
  validate(need(length(dimdesc(values_PCA()$res.PCA))>1,gettext("No quantitative variable describes axis 2")))
  return(as.data.frame(dimdesc(values_PCA()$res.PCA)[[2]]$quanti))
},rownames=TRUE)

output$sortieDimdesc44_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>2 || input$selecactive_PCA==gettext("All"),gettext("Please select more variables"))
  )
  validate(need(length(dimdesc(values_PCA()$res.PCA))>1,gettext("No categorical variable describes axis 2")))
  return(as.data.frame(dimdesc(values_PCA()$res.PCA)[[2]]$quali))
},rownames=TRUE)

#DIM3

output$sortieDimdesc333_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>2 || input$selecactive_PCA==gettext("All"),gettext("Please select more variables"))
  )
  validate(need(length(dimdesc(values_PCA()$res.PCA))>2,gettext("No quantitative variable describes axis 3")))
  return(as.data.frame(dimdesc(values_PCA()$res.PCA)[[3]]$quanti))
},rownames=TRUE)

output$sortieDimdesc444_PCA=renderTable({
  validate(
    need(length(getactive_PCA())>2 || input$selecactive_PCA==gettext("All"),gettext("Please select more variables"))
  )
  validate(need(length(dimdesc(values_PCA()$res.PCA))>2,"No categorical variable describes axis 3"))
  return(as.data.frame(dimdesc(values_PCA()$res.PCA)[[3]]$quali))
},rownames=TRUE)


output$map3_PCA=renderPlot({
  return(barplot(values_PCA()$res.PCA$eig[,1],names.arg=rownames(values_PCA()$res.PCA$eig),las=2))
})

output$JDD_PCA=renderDataTable({
  cbind(Names=rownames(values$newdata_PCA),values$newdata_PCA)},
  options = list(    "orderClasses" = TRUE,
                     "responsive" = TRUE,
                     "pageLength" = 10))

output$summary_PCA=renderPrint({
  summary(newdata_PCA)
})

output$summaryPCA=renderPrint({
  validate(
    need(input$nbele_PCA!=0, gettext("Please select at least one element"))
  )
  a<-values_PCA()$res.PCA
  a$call$call<-code_PCA()
  summary.PCA(a,nbelements=input$nbele_PCA)
})

output$summary2_PCA=downloadHandler(filename = function() { 
  paste('summaryofPCA','.txt', sep='') 
},
content = function(file) {
  summary_PCA.PCA(values_PCA()$res.PCA,nbelements=input$nbele_PCA,file=file)
},
contentType='text/csv')


output$slider3_PCA=renderUI({
  validate(
    need(length(getactive_PCA())>1 || input$selecactive_PCA==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(input$selecactive_PCA==gettext("All")){
    maxvar=length(VariableChoices_PCA)
  }
  if(input$selecactive_PCA==gettext("Choose")){
    maxvar=length(getactive_PCA())
  }
  if(selection_PCA3=="contrib"){
    return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                          min=1,max=maxvar,value=selection_PCA4,step=1)))  
  }
  else{
    return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                          min=1,max=maxvar,value=maxvar,step=1)))}
})


output$habillage2_PCA=renderUI({
  if(length(QualiChoice_PCA)==0 || input$supquali_PCA==FALSE || length(input$supquali_PCA)==0){
    return(p(gettext("No categorical variable")))
  }
  if(length(input$supquali_PCA)>1){
    if(is.null(habillageind_PCA)){
      num_PCA=c(1:length(input$supquali_PCA))
      return(selectInput("habiller",gettext("select 1 or 2 variables"), choices=list(num_PCA=input$supquali_PCA),multiple=TRUE))
    }
    else{
      num_PCA=c(1:length(input$supquali_PCA))
      return(selectInput("habiller",gettext("select 1 or 2 variables"), choices=list(num_PCA=input$supquali_PCA),multiple=TRUE,selected=habillageind_PCA))
    }
  }
})

output$ellipses_PCA=renderUI({
  #validate(need(!is.null(input$habiller),""))
  if(length(QualiChoice_PCA)==0 || input$supquali_PCA==FALSE || length(input$supquali_PCA)==0){
    return(p(" "))
  }else{
    return(checkboxInput("elip_PCA",gettext("Draw the confidence ellipses_PCA around the categories"),ellipses_PCA))
  }
})

output$varsu_PCA=renderUI({
  test=values_PCA()$choixquant
  if(!is.null(test)){
    if(!is.null(colorsupvar_PCA)){
      return(colourpicker::colourInput("colorsupvar_PCA", h6(gettext("Choose colour for supplementary variables")), colorsupvar_PCA))
    }else{
      return(colourpicker::colourInput("colorsupvar_PCA", h6(gettext("Choose colour for supplementary variables")), "blue"))
    }
  }
})

output$histo_PCA=renderPlot({
  par(mfrow=c(1,2))
  boxplot(newdata_PCA[,input$bam_PCA])
  hist(newdata_PCA[,input$bam_PCA],main="",xlab="")
})



output$downloadData_PCA = downloadHandler(
  filename = function() { 
    paste('graph1','.png', sep='') 
  },
  content = function(file) {
    png(file)
    Plot11()
    dev.off()
  },
  contentType='image/png')

output$downloadData1_PCA = downloadHandler(
  filename = function() { 
    paste('graph1','.jpg', sep='') 
  },
  content = function(file) {
    jpeg(file)
    Plot11()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData2_PCA = downloadHandler(
  filename = function() { 
    paste('graph1','.pdf', sep='') 
  },
  content = function(file) {
    pdf(file)
    Plot11()
    dev.off()
  },
  contentType=NA)

output$downloadData3_PCA = downloadHandler(
  filename = function() { 
    paste('graph2','.png', sep='') 
  },
  content = function(file) {
    png(file)
    Plot22()
    dev.off()
  },
  contentType='image/png')

output$downloadData4_PCA = downloadHandler(
  filename = function() { 
    paste('graph1','.jpg', sep='') 
  },
  content = function(file) {
    jpeg(file)
    Plot22()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData5_PCA = downloadHandler(
  filename = function() { 
    paste('graph1','.pdf', sep='') 
  },
  content = function(file) {
    pdf(file)
    Plot22()
    dev.off()
  },
  contentType=NA)

Plot11=function(){
  if(input$select0_PCA=="cos2"){
    if(input$slider00_PCA!=1){
      selecindiv=paste("cos2 ",input$slider00_PCA)
    }
    else{
      selecindiv="cos2 0.999"
    }
  }
  if(input$select0_PCA==gettext("No selection")){
    selecindiv=NULL
  }
  if(input$select0_PCA=="contrib"){
    selecindiv=paste("contrib ",input$slider4)
  }
  if(is.null(input$colorsupvar_PCA)){
    colo="blue"
  }else{
    colo=input$colorsupvar_PCA
  }
  plot.PCA(values_PCA()$res.PCA,axes=c(as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),choix="var",select=selecindiv,unselect=0,col.quanti.sup=colo,cex=input$cex2_PCA,cex.main=input$cex2_PCA,cex.axis=input$cex2_PCA,title=input$title2_PCA,col.var=input$coloractvar_PCA)
}
Plot22=function(){
  if(input$select_PCA=="cos2"){
    if(input$slider1_PCA!=1){
      selecindiv=paste("cos2 ",input$slider1_PCA)
    }
    else{
      selecindiv="cos2 0.999"
    }
  }
  if(input$select_PCA==gettext("No selection")){
    selecindiv=NULL
  }
  if(input$select_PCA=="contrib"){
    selecindiv=paste("contrib ",input$slider0_PCA)
  }
  if(input$select_PCA==gettext("Manual")){
    selecindiv=c(input$indiv_PCA)
  }
  if(input$supquali_PCA==FALSE || length(QualiChoice_PCA)==0 || length(input$supquali_PCA)==0 || input$habi_PCA==FALSE){
    hab="none"
    colquali_PCA="magenta"
  }
  else if(length(QualiChoice_PCA)==1 && input$supquali_PCA==TRUE){
    if(input$habi_PCA==TRUE){
      hab=QualiChoice_PCA
      colquali_PCA="blue"
    }
    else{
      hab="none"
      colquali_PCA="magenta"
    }
  }
  else if (length(input$supquali_PCA)==1){
    if(input$habi_PCA==TRUE){
      hab=input$supquali_PCA
      colquali_PCA="blue"
    }
    else{
      hab="none"
      colquali_PCA="magenta"
    }
  }
  if(length(input$supquali_PCA)>1){
    if(length(input$habiller)==0){
      hab="none"
      colquali_PCA="magenta"
    }
    if (length(input$habiller)==1 & input$habi_PCA==TRUE){
      hab=as.character(input$habiller)
      colquali_PCA="blue"
    }
    if (length(input$habiller)==2 & input$habi_PCA==TRUE){
      hab=dim(values_PCA()$DATA)[2]
      colquali_PCA="blue"
    }
  }
  if(!is.null(input$colorsup)){
    colors=input$colorsup
  }else{
    colors="blue"
  }
  if(!is.null(input$colorquali_PCA)){
    colorss=input$colorquali_PCA
  }else{
    colorss="magenta"
  }
  if(!is.null(input$elip_PCA)&&input$elip_PCA==TRUE){
    aa=cbind.data.frame(values_PCA()$DATA[,hab],values_PCA()$res.PCA$ind$coord)
    bb=coord.ellipse(aa,bar=TRUE)
    plot.PCA(values_PCA()$res.PCA,axes=c(as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),choix="ind",cex=input$cex_PCA,cex.main=input$cex_PCA,cex.axis=input$cex_PCA,select=selecindiv,habillage=hab,col.quali=colorss,col.ind.sup=colors,title=input$title1_PCA,ellipse=bb,col.ind = input$coloract_PCA)
    
  }else{
    plot.PCA(values_PCA()$res.PCA,axes=c(as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),choix="ind",cex=input$cex_PCA,cex.main=input$cex_PCA,cex.axis=input$cex_PCA,select=selecindiv,habillage=hab,col.quali=colorss,col.ind.sup=colors,title=input$title1_PCA,col.ind = input$coloract_PCA)
  }
  #plot.PCA(values_PCA()$res.PCA,axes=c(as.numeric(input$NB1_PCA),as.numeric(input$NB2_PCA)),choix="ind",cex=input$cex_PCA,cex.main=input$cex_PCA,cex.axis=input$cex_PCA,select=selecindiv,habillage=hab,col.quali=colquali_PCA,col.ind.sup="blue",title=input$title1_PCA)    
}