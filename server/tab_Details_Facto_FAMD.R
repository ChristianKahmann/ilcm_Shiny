


output$facto_meta_famd_ui<-renderUI({
  values$Details_Data_FA
  return(
    selectInput(inputId = "facto_meta_famd",label = "Choose meta data field",choices = colnames(values$facto_meta))
  )
})

observe({
  validate(
    need(!is.null(input$facto_meta_famd),message=F)
  )
  x<-as.data.frame((as.matrix(values$facto_dtm)))
  x<-cbind(isolate(values$facto_meta[,input$facto_meta_famd]),x)
  colnames(x)[1]<-"X1"
  attach(x)
  x<-as.data.frame(aggregate(x[,2:dim(x)[2]],by=list(X1),FUN = sum))
  colnames(x)[1]<-input$facto_meta_famd
  
 
  if(is.data.frame(x)==TRUE){
    quanti=names(which(sapply(x,is.numeric)))
    quali=names(which(!(sapply(x,is.numeric))))
    if(length(quanti)==0 || length(quali)==0){
      shinyalert::shinyalert('Your dataset is not mixed',type = "warning")
    }
    else{
      quanti=c()
      quali=c()
      posi=c()
      for ( i in 1:dim(x)[2]){
        if (is.numeric(x[,i])==TRUE){
          quanti=c(quanti,colnames(x)[i])
        }
        else{
          quali=c(quali,colnames(x)[i])
        }
      }
      
      
      assign("nomData",paste0("Collection ",isolate(values$facto_info[[5]])),envir=.GlobalEnv)
      assign("newdata_FAMD",x,envir=.GlobalEnv)
      assign("quantisup",NULL,envir=.GlobalEnv)
      assign("qualisup",NULL,envir=.GlobalEnv)
      assign("indsupl",NULL,envir=.GlobalEnv)
      assign("axe1",1,envir=.GlobalEnv)
      assign("axe2",2,envir=.GlobalEnv)
      assign("labind",TRUE,envir=.GlobalEnv)
      assign("labvar",TRUE,envir=.GlobalEnv)
      assign("habillageind",TRUE,envir=.GlobalEnv)
      assign("selection","NONE",envir=.GlobalEnv)
      assign("selection2",NULL,envir=.GlobalEnv)
      assign("selection3","NONE",envir=.GlobalEnv)
      assign("selection4",NULL,envir=.GlobalEnv)
      assign("selection5","NONE",envir=.GlobalEnv)
      assign("selection6",NULL,envir=.GlobalEnv)
      assign("size",1,envir=.GlobalEnv)
      assign("size2",1,envir=.GlobalEnv)
      assign("size3",1,envir=.GlobalEnv)
      assign("title1",gettext("Graph of individuals and categories"),envir=.GlobalEnv)
      assign("title2",gettext("Graph of the variables"),envir=.GlobalEnv)
      assign("title3",gettext("Correlation circle"),envir=.GlobalEnv)
      values$Facto_ready_FAMD<-TRUE
      values$Facto_new_Famd<-runif(n = 1,min = 0,max = 1) 
      
    }
  }
})





output$Details_Facto_FAMD_UI<-renderUI({
  validate(
    need(values$Facto_ready_FAMD==TRUE,message=FALSE)
  )
  values$Facto_new_Famd
  all=colnames(newdata_FAMD)
  quanti=names(which(sapply(newdata_FAMD,is.numeric)))
  quali=names(which(!(sapply(newdata_FAMD,is.numeric))))
  VariableChoices=quanti
  nom_FAMD=rownames(newdata_FAMD)
  assign("nom_FAMD",nom_FAMD,envir=.GlobalEnv)
  num=c(1:length(nom_FAMD))
  QualiChoice=quali
  IdChoices=c(1:length(VariableChoices))
  Idqualisup=c(1:length(QualiChoice))
  Idall=c(1:length(all))
  nomData=unlist(strsplit(as.character(nomData),"\\["))[1]
  assign("quanti",quanti,envir=.GlobalEnv)
  values$newdata_FAMD<-newdata_FAMD
  return(tagList(
    div(paste(gettext("FAMD on the "),nomData),style="color:#2A0A29"),
    sidebarLayout(
      sidebarPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
            tags$head(
              tags$style("body {background-color: #FFF0C7; }"),
              tags$style(type='text/css', "#title1 { height: 25px; }"),
              tags$style(type='text/css', "#title2 { height: 25px; }"),
              tags$style(type='text/css', "#title3 { height: 25px; }")
            ),
            wellPanel(
              div(align="center",checkboxInput("pcaparam",gettext("Show FAMD parameters"),FALSE)),
              conditionalPanel(
                condition="input.pcaparam==true",
                if(is.null(quantisup) && is.null(qualisup)){
                  radioButtons("selecactive_FAMD",label=h6(gettext("Choose the active variables")),
                               choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))
                }
                else{
                  radioButtons("selecactive_FAMD",label=h6(gettext("Choose the active variables")),
                               choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose"))
                },
                conditionalPanel(
                  #          condition="input.selecactive=='choix'",
                  condition=paste("input.selecactive_FAMD=='",gettext("Choose"),"'",sep=''),
                  h6(gettext("Select the supplementary quantitative variables")),
                  if(length(VariableChoices)>1){
                    selectInput("supvar_FAMD",label="",
                                choices=list(IdChoices=VariableChoices),
                                selected=quantisup,multiple=TRUE)}
                  else{
                    selectInput("supvar_FAMD",label="",
                                choices=VariableChoices,
                                selected=quantisup,multiple=TRUE)
                  },
                  h6(gettext("Select the supplementary qualitative variables")),
                  if(length(QualiChoice)>1){
                    selectInput("supvar1_FAMD",label="",
                                choices=list(Idqualisup=QualiChoice),
                                selected=qualisup,
                                multiple=TRUE)}
                  else{
                    selectInput("supvar1_FAMD",label="",
                                choices=quali,selected=qualisup,
                                multiple=TRUE)  
                  }
                ),
                br(),
                h6(gettext("Select the supplementary individuals")),
                if(is.null(indsupl)){
                  selectInput("indsup","",choices=list(num=nom_FAMD), multiple=TRUE)
                }
                else{
                  selectInput("indsup","",choices=list(num=nom_FAMD), multiple=TRUE,selected=indsupl)
                }
              )
            ),
            wellPanel(
              div(align="center",checkboxInput("graph_FAMD",gettext("Show graphs options"),FALSE)),
              conditionalPanel(
                condition="input.graph_FAMD==true",
                fluidRow(
                  column(5,uiOutput("NB1_FAMD")),
                  column(5,uiOutput("NB2_FAMD"))),
                hr(),
                div(align="center",selectInput("choixgraph",h6(gettext("Which graph would you like to modify?")), choices=list(gettext("Individuals and categories"),"Variables"="var",gettext("Quantitative variables")),selected=gettext("Individuals and categories"))),
                br(),
                conditionalPanel(
                  #          condition="input.choixgraph=='ind'",
                  condition=paste("input.choixgraph=='",gettext("Individuals and categories"),"'",sep=''),
                  textInput("title1_FAMD",h6(gettext("Title of the graph: ")), title1),
                  sliderInput("cex",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size,step=0.05,ticks=FALSE),
                  br(),
                  checkboxInput("labels2",gettext("Draw labels of individuals"),labind),
                  checkboxInput("labels",gettext("Draw labels of categories"),labvar),
                  selectInput("select",label=h6(gettext("Draw individuals according to:")),
                              choices=list("No selection"="NONE","cos2"="cos2","Contribution"="contrib","Manual"="Manuel"),selected=selection),
                  conditionalPanel(
                    condition="input.select=='cos2'",
                    if(selection=="cos2"){
                      div(align="center",sliderInput("slider1", label = "cos2",
                                                     min = 0, max = 1, value =as.numeric(selection2),step=0.05))}
                    else{
                      div(align="center",sliderInput("slider1", label = "cos2",
                                                     min = 0, max = 1, value =0,step=0.05))
                    }),
                  conditionalPanel(
                    condition="input.select=='contrib'",
                    uiOutput("slider7")),
                  conditionalPanel(
                    condition="input.select=='Manuel'",
                    if(selection=="Manuel"){
                      selectInput("indiv",label=gettext("Select individuals"),
                                  choices=list(num=nom_FAMD),multiple=TRUE,selected=selection2)}
                    else{
                      selectInput("indiv",label=gettext("Select individuals"),
                                  choices=list(num=nom_FAMD),multiple=TRUE)
                    }),
                  if(is.null(habillageind)){
                    checkboxInput("habi",gettext("Points colour depend on categorical variable"),FALSE)
                  }
                  else{
                    checkboxInput("habi",gettext("Points colour depend on categorical variable"),TRUE)
                  },
                  conditionalPanel(
                    condition="input.habi==true",
                    uiOutput("habillage2")
                  )
                ),
                conditionalPanel(
                  condition="input.choixgraph=='var'",
                  textInput("title2_FAMD",h6(gettext("Title of the graph: ")), title2),
                  sliderInput("cex2",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size2,step=0.05,ticks=FALSE),
                  br(),
                  selectInput("select0",label=h6(gettext("Draw variables according to:")),
                              choices=list("No selection"="NONE","cos2"="cos2","Contribution"="contrib"),selected=selection3),
                  conditionalPanel(
                    condition="input.select0=='contrib'",
                    uiOutput("slider3")
                  ),
                  conditionalPanel(
                    condition="input.select0=='cos2'",
                    if(selection3=="cos2"){
                      div(align="center",sliderInput("slider00", label = "cos2",
                                                     min = 0, max = 1, value =as.numeric(selection4),step=0.05))  
                    }
                    else{
                      div(align="center",sliderInput("slider00", label = "cos2",
                                                     min = 0, max = 1, value =0,step=0.05))}
                  )
                ),
                conditionalPanel(
                  condition=paste("input.choixgraph=='",gettext("Quantitative variables"),"'",sep=''),
                  #          condition="input.choixgraph=='quant'",
                  textInput("title3_FAMD",h6(gettext("Title of the graph: ")), title3),
                  sliderInput("cex3",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size3,step=0.05,ticks=FALSE),
                  br(),
                  selectInput("selecti",label=h6(gettext("Draw variables according to:")),
                              choices=list("No selection"="NONE","cos2"="cos2","Contribution"="contrib"),selected=selection5),
                  conditionalPanel(
                    condition="input.selecti=='contrib'",
                    uiOutput("slider5")
                  ),
                  conditionalPanel(
                    condition="input.selecti=='cos2'",
                    if(selection3=="cos2"){
                      div(align="center",sliderInput("slider000", label = "cos2",
                                                     min = 0, max = 1, value =as.numeric(selection6),step=0.05))  
                    }
                    else{
                      div(align="center",sliderInput("slider000", label = "cos2",
                                                     min = 0, max = 1, value =0,step=0.05))}
                  )
                )
              )
            ),
            wellPanel(
              h5(gettext("Save graphs as"),align="center"),
              radioButtons("paramdown_FAMD","",
                           choices=list("PNG"="png","JPG"="jpg","PDF"="pdf"),selected="png")
              
            )
        ),
        width=3),
      mainPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
            tabsetPanel(id = "graph_sort_FAMD",
                        tabPanel(gettext("Graphs"),
                                 fluidRow(
                                   br(),
                                   column(width = 6,plotOutput("map2_FAMD", width = "500", height="500"),
                                          ###                             div(align = "center",plotOutput("map2_FAMD", width = 500, height=500)),
                                          br(),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='jpg'",
                                            p(downloadButton("downloadData4_FAMD_FAMD",gettext("Download as jpg")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='png'",
                                            p(downloadButton("downloadData3_FAMD",gettext("Download as png")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='pdf'",
                                            p(downloadButton("downloadData5_FAMD",gettext("Download as pdf")),align="center")),
                                          br(),
                                          align="center"),
                                   column(width = 6,plotOutput("map_FAMD", width = "500", height="500"),
                                          ###                             div(align="center",plotOutput("map_FAMD", width = 500, height=500)),
                                          br(),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='jpg'",
                                            p(downloadButton("downloadData1_FAMD",gettext("Download as jpg")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='png'",
                                            p(downloadButton("downloadData_FAMD",gettext("Download as png")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='pdf'",
                                            p(downloadButton("downloadData2_FAMD",gettext("Download as pdf")),align="center")),
                                          br(),
                                          align="center")),
                                 fluidRow(
                                   br(),
                                   column(width = 6,plotOutput("map4_FAMD", width = "500", height="500"),
                                          #                             div(align="center",plotOutput("map4_FAMD", width = 500, height=500)),
                                          br(),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='jpg'",
                                            p(downloadButton("downloadData6_FAMD",gettext("Download as jpg")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='png'",
                                            p(downloadButton("downloadData7_FAMD",gettext("Download as png")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_FAMD=='pdf'",
                                            p(downloadButton("downloadData8_FAMD",gettext("Download as pdf")),align="center")),
                                          align="center"))),
                        
                        tabPanel(gettext("Values"),
                                 br(),
                                 uiOutput("out22_FAMD", width = "500", height="500"),
                                 br(),
                                 conditionalPanel(
                                   condition=paste("input.out_FAMD=='",gettext("Eigenvalues"),"'",sep=''),
                                   #                               condition="input.out_FAMD=='eig'",
                                   div(align="center",tableOutput("sorties_FAMD")),
                                   plotOutput("map3_FAMD", width = "700", height="500")),
                                 conditionalPanel(
                                   condition=paste("input.out_FAMD=='",gettext("Results of the variables"),"'",sep=''),
                                   #                               condition="input.out_FAMD=='resvar'",
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties_FAMD2")),
                                   br(),
                                   h6("Contributions"),
                                   div(align="center",tableOutput("sorties_FAMD3")),
                                   br(),
                                   h6("Cos2"),
                                   div(align="center",tableOutput("sorties_FAMD4"))),
                                 conditionalPanel(
                                   condition=paste("input.out_FAMD=='",gettext("Results of the individuals"),"'",sep=''),
                                   #                               condition="input.out_FAMD=='resind'",
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties_FAMD22")),
                                   br(),
                                   h6("Contributions"),
                                   div(align="center",tableOutput("sorties_FAMD33")),
                                   br(),
                                   h6("Cos2"),
                                   div(align="center",tableOutput("sorties_FAMD44"))),
                                 conditionalPanel(
                                   condition=paste("input.out_FAMD=='",gettext("Summary of outputs"),"'",sep=''),
                                   #                               condition="input.out_FAMD=='ACP'",
                                   numericInput("nbele",gettext("Number of elements to print"),value=10),
                                   verbatimTextOutput("summary_FAMD"),
                                   p(downloadButton("summary2_FAMD",gettext("Download the summary")),align="center")),
                                 conditionalPanel(
                                   condition=paste("input.out_FAMD=='",gettext("Results of the supplementary variables"),"'",sep=''),
                                   #                               condition="input.out_FAMD=='varsup'",
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties_FAMD23")),
                                   h6("Cos2"),
                                   div(align="center",tableOutput("sorties_FAMD32"))),
                                 conditionalPanel(
                                   condition=paste("input.out_FAMD=='",gettext("Results of the supplementary individuals"),"'",sep=''),
                                   #                               condition="input.out_FAMD=='supind'",
                                   h6(gettext("Coordinates")),
                                   div(align="center",tableOutput("sorties_FAMD36")),
                                   h6("Cos2"),
                                   div(align="center",tableOutput("sorties_FAMD37")))
                        ),
                        tabPanel(gettext("Summary of dataset"),
                                 br(),
                                 verbatimTextOutput("summary_FAMD2"),
                                 br(),
                                 selectInput("bam_FAMD",h6(gettext("Graphs for")),choices=list(Idall=all),multiple=FALSE),
                                 plotOutput("histo_FAMD", width = "1000", height="500")),
                        
                        tabPanel(gettext("Data"),
                                 br(),
                                 dataTableOutput("JDD_FAMD")
                        )
            )
        )
        ,width=9)
      
    )
    
  )
  )
})



values_FAMD=reactive({
  if (input$selecactive_FAMD==gettext("All")){
    data.selec=values$newdata_FAMD
  }
  else{
    validate(
      need(length(input$supvar_FAMD)>0 || length(input$supvar1_FAMD)>0, gettext("Please select at least one supplementary variable"))
    )
    data.selec=values$newdata_FAMD
  }
  choixsup=getactive_FAMD()$sup
  if(length(input$indsup)==0){
    suple=NULL
  }
  else{
    suple=which(nom_FAMD%in%input$indsup)
    # suple=c()
    # for (i in 1:length(nom_FAMD)){
    # if(nom_FAMD[i]%in%input$indsup){
    # suple=c(suple,i)
    # }
    # }
  }
  list(res.FAMD=(FAMD(data.selec,sup.var=choixsup,ind.sup=suple,graph=FALSE,ncp=max(5,as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)))),DATA=(data.selec),choixsuple=(suple),varsup=(choixsup))
})

Plot1_FAMD <- reactive({
  validate(
    need(input$nb1_FAMD != input$nb2_FAMD, gettext("Please select two different dimensions"))
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variables"))
  )
  if(input$select0=="cos2"){
    if(input$slider00!=1){
      selecindiv=paste("cos2 ",input$slider00)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  if(input$select0=="NONE"){
    selecindiv=NULL
    selecindivText="NULL"
  }
  if(input$select0=="contrib"){
    selecindiv=paste("contrib ",input$slider4)
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  list(PLOT=(plot.FAMD(values_FAMD()$res.FAMD,axes=c(as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)),choix="var",cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2_FAMD,select=selecindiv)),selecindivText=selecindivText)
})

output$map_FAMD <- renderPlot({
  p <- Plot1_FAMD()$PLOT
})

Plot2_FAMD <- reactive({
  validate(
    need(input$nb1_FAMD != input$nb2_FAMD, gettext("Please select two different dimensions"))
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  hab="none"
  if(length(QualiChoice)==0){
    hab="none"
  }
  else if(length(QualiChoice)==1 && input$habi==TRUE){
    if(input$habi==TRUE){
      hab=QualiChoice
      colquali="blue"
    }
    else{
      hab="none"
      colquali="magenta"
    }
  }
  else if (length(QualiChoice)>1){
    if(input$habi==TRUE){
      if(is.null(input$habiller)){
        hab="none"
      }
      else{
        hab=input$habiller
      }
    }
    else{
      hab="none"
      colquali="magenta"
    }
  }
  if(hab!="none"){
    hab=which(all==hab)
    hab=as.numeric(hab)
  }
  if(input$select=="cos2"){
    if(input$slider1!=1){
      selecindiv=paste("cos2 ",input$slider1)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivText=paste0("'",selecindiv,"'")
  }
  if(input$select=="NONE"){
    selecindiv=NULL
    selecindivText="NULL"
  }
  if(input$select=="contrib"){
    selecindiv=paste("contrib ",input$slider0)
    selecindivText=paste0("'",selecindiv,"'")
  }
  if(input$select=="Manuel"){
    selecindiv=c(input$indiv)
    selecindivText=paste0("'",selecindiv,"'")
  }
  list(PLOT=(plot.FAMD(values_FAMD()$res.FAMD,axes=c(as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)),choix="ind",select=selecindiv,lab.var=input$labels,lab.ind=input$labels2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,habillage=hab,title=input$title1_FAMD)),selecindivText=selecindivText,HABILLAGE=hab)
  
})

output$map2_FAMD <- renderPlot({
  p <- Plot2_FAMD()$PLOT
})


Plot4 <- reactive({
  validate(
    need(input$nb1_FAMD != input$nb2_FAMD, gettext("Please select two different dimensions"))
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(input$selecti=="cos2"){
    if(input$slider000!=1){
      selecindiv=paste("cos2 ",input$slider000)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  if(input$selecti=="NONE"){
    selecindiv=NULL
    selecindivText="NULL"
  }
  if(input$selecti=="contrib"){
    selecindiv=paste("contrib ",input$slider6)
    paste("'",selecindiv,"'",sep="")
  }
  list(PLOT=(plot.FAMD(values_FAMD()$res.FAMD,axes=c(as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)),choix="quanti",cex=input$cex3,cex.main=input$cex3,cex.axis=input$cex3,title=input$title3_FAMD,select=selecindiv)))
})

output$map4_FAMD <- renderPlot({
  p <- Plot4()$PLOT
})



codeGraphVar<-function(){
  
  if(length(input$slider4)==0){
    selection="NULL"
  }
  else{
    selection=Plot1_FAMD()$selecindivText
  }
  Call1=paste("plot.FAMD(res.FAMD,axes=c(",input$nb1_FAMD,",",input$nb2_FAMD,"),choix='var',select=",selection,",cex=",input$cex2,",cex.main=",input$cex2,",cex.axis=",input$cex2,",unselect=0)",sep="")
  return(Call1)
}

codeGraphInd<-function(){
  hab='none'
  if (length(input$habiller)<=1 & input$habi==TRUE || input$habi==FALSE){
    hab=paste(Plot2_FAMD()$HABILLAGE,sep="")
  }
  if (hab=="none") Call2=paste("plot.FAMD(res.FAMD, axes=c(",input$nb1_FAMD,",",input$nb2_FAMD,"),choix='ind',select=",Plot2_FAMD()$selecindivText,",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex,",lab.var=",input$labels,",lab.ind=",input$labels2,",title='",input$title1_FAMD,"')",sep="")
  else Call2=paste("plot.FAMD(res.FAMD, axes=c(",input$nb1_FAMD,",",input$nb2_FAMD,"),choix='ind',select=",Plot2_FAMD()$selecindivText,",habillage=",hab,",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex,",lab.var=",input$labels,",lab.ind=",input$labels2,",title='",input$title1_FAMD,"')",sep="")
  return(Call2)
}

##### Fin de la fonction recuperation du code

output$out22_FAMD=renderUI({
  #      choix=list("Summary of FAMD"="ACP","Eigenvalues"="eig","Results of the variables"="resvar","Results of the individuals"="resind")
  choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
  if(!is.null(values_FAMD()$choixsuple)){
    #        choix=c(choix,"Results of the supplementary individuals"="supind")
    choix=c(choix,gettext("Results of the supplementary individuals"))
  }
  if(!is.null(values_FAMD()$varsup)){
    #        choix=c(choix,"Results of the supplementary variables"="varsup")
    choix=c(choix,gettext("Results of the supplementary variables"))
  }
  radioButtons("out_FAMD",gettext("Which outputs do you want?"),
               choices=choix,inline=TRUE)
})

getactive_FAMD=function(){
  if(input$selecactive_FAMD==gettext("Choose")){
    sup<-sup2<-sup3<-NULL
    if(length(input$supvar_FAMD)==0&&length(input$supvar1_FAMD)==0){
      activevar<-all
      supl<-NULL
    }
    else if(length(input$supvar1_FAMD)==0&&length(input$supvar_FAMD)!=0){
      # for (i in 1:length(all)){
      # if(all[i]%in%input$supvar_FAMD){
      # sup=c(sup,i)
      # }
      # }
      sup<-which(all%in%input$supvar_FAMD)
      activevar<-all[-sup]
      supl<-VariableChoices[sup]
      quanti<-VariableChoices[-sup]
    }
    else if(length(input$supvar_FAMD)==0&&length(input$supvar1_FAMD)!=0){
      # for (i in 1:length(all)){
      # if(all[i]%in%input$supvar1_FAMD){
      # sup=c(sup,i)
      # }
      # }
      sup=which(all%in%input$supvar1_FAMD)
      activevar=all[-sup]
      supl=QualiChoice[sup]
      quali=QualiChoice[-sup]
    }
    else if(length(input$supvar_FAMD)!=0&&length(input$supvar1_FAMD)!=0){
      # for (i in 1:length(all)){
      # if(all[i]%in%input$supvar1_FAMD || all[i]%in%input$supvar_FAMD){
      # sup=c(sup,i)
      # }
      # }
      sup=which(all%in%c(input$supvar_FAMD,input$supvar1_FAMD))
      activevar=all[-sup]
      supl=all[sup]
      # for (i in 1:length(QualiChoice)){
      # if(QualiChoice[i]%in%input$supvar1_FAMD){
      # sup2=c(sup2,i)
      # }
      # }
      sup2=which(QualiChoice%in%input$supvar1_FAMD)
      # for (i in 1:length(VariableChoices)){
      # if(VariableChoices[i]%in%input$supvar_FAMD){
      # sup3=c(sup3,i)
      # }
      # }
      sup3=which(VariableChoices%in%input$supvar_FAMD)
      quali=QualiChoice[-sup2]
      quanti=VariableChoices[-sup3]
    }
    # ind=NULL
    # ind=which(all%in%supl)
    # if (length(ind)==0) ind=NULL
    # if(!is.null(supl)){
    # for(i in 1:length(supl)){
    # ind=c(ind,which(all==supl[i]))
    # }
    # }
    return(list(activevar=activevar,supl=supl,quanti=quanti,quali=quali,sup=sup))
  }
}
# 

output$NB1_FAMD=renderUI({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variables"))
  )
  if(input$selecactive_FAMD==gettext("All") || length(getactive_FAMD()$activevar)>5){
    # return(selectInput("nb1_FAMD", label = h6(gettext("x axis")),
    # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe1,width='80%'))
    return(textInput("nb1_FAMD", label = h6(gettext("x axis")), axe1,width='50%'))
  }
  else{
    baba=c(1:length(getactive_FAMD()$activevar))
    return(selectInput("nb1_FAMD",label=h6(gettext("x axis")), choices=baba,selected=axe1,width='80%'))
  }
})

output$NB2_FAMD=renderUI({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variables"))
  )
  if(input$selecactive_FAMD==gettext("All") || length(getactive_FAMD()$activevar)>5){
    # return(selectInput("nb2_FAMD", label = h6(gettext("y axis")),
    # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe2,width='80%'))
    return(textInput("nb2_FAMD", label = h6(gettext("y axis")), axe2,width='50%'))
  }
  else{
    baba=c(1:length(getactive_FAMD()$activevar))
    return(selectInput("nb2_FAMD",label=h6(gettext("y axis")), choices=baba,selected=axe2,width='80%'))
  }
})

output$sorties_FAMD=renderTable({
  return(as.data.frame(values_FAMD()$res.FAMD$eig))
},rownames=TRUE)

output$sorties_FAMD12=renderTable({
  validate(
    need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$quali.sup$coord))
},rownames=TRUE)

output$sorties_FAMD13=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variables"))
  )
  validate(
    need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$quali.sup$v.test))
},rownames=TRUE)

output$sorties_FAMD2=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variables"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$var$coord))
},rownames=TRUE)

output$sorties_FAMD22=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variables"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$ind$coord))
},rownames=TRUE)

output$sorties_FAMD23=renderTable({
  validate(
    need(length(input$supvar_FAMD)!=0, gettext("No supplementary quantitative variables"))
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$var$coord.sup))
},rownames=TRUE)

output$sorties_FAMD32=renderTable({
  validate(
    need(length(input$supvar_FAMD)!=0, gettext("No supplementary quantitative variables"))
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$var$cos2.sup))
},rownames=TRUE)

output$sorties_FAMD36=renderTable({
  validate(
    need(length(input$indsup)!=0, "No supplementary individuals")
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$ind.sup$coord))
},rownames=TRUE)

output$sorties_FAMD37=renderTable({
  validate(
    need(length(input$indsup)!=0, gettext("No supplementary individuals"))
  )
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$ind.sup$cos2))
},rownames=TRUE)


output$sorties_FAMD3=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$var$contrib))
},rownames=TRUE)

output$sorties_FAMD33=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$ind$contrib))
},rownames=TRUE)

output$sorties_FAMD4=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$var$cos2))
},rownames=TRUE)

output$sorties_FAMD44=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  return(as.data.frame(values_FAMD()$res.FAMD$ind$cos2))
},rownames=TRUE)

output$sortieDimdesc3=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>2 || input$selecactive_FAMD==gettext("All"),gettext("Please select more variables"))
  )
  return(as.data.frame(dimdesc(values_FAMD()$res.FAMD)[[1]]$quanti))
},rownames=TRUE)

output$sortieDimdesc4=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>2 || input$selecactive_FAMD==gettext("All"),gettext("Please select more variables"))
  )
  return(as.data.frame(dimdesc(values_FAMD()$res.FAMD)[[1]]$quali))
},rownames=TRUE)

#DIM2

output$sortieDimdesc33=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>2 || input$selecactive_FAMD==gettext("All"),gettext("Please select more variables"))
  )
  return(as.data.frame(dimdesc(values_FAMD()$res.FAMD)[[2]]$quanti))
},rownames=TRUE)

output$sortieDimdesc44=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>2 || input$selecactive_FAMD==gettext("All"),gettext("Please select more variables"))
  )
  return(as.data.frame(dimdesc(values_FAMD()$res.FAMD)[[2]]$quali))
},rownames=TRUE)

#DIM3

output$sortieDimdesc333=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>2 || input$selecactive_FAMD==gettext("All"),gettext("Please select more variables"))
  )
  return(as.data.frame(dimdesc(values_FAMD()$res.FAMD)[[3]]$quanti))
},rownames=TRUE)

output$sortieDimdesc444=renderTable({
  validate(
    need(length(getactive_FAMD()$activevar)>2 || input$selecactive_FAMD==gettext("All"),gettext("Please select more variables"))
  )
  return(as.data.frame(dimdesc(values_FAMD()$res.FAMD)[[3]]$quali))
},rownames=TRUE)


output$map3_FAMD=renderPlot({
  return(barplot(values_FAMD()$res.FAMD$eig[,1],names.arg=rownames(values_FAMD()$res.FAMD$eig),las=2))
})

output$JDD_FAMD=renderDataTable({
  cbind(Names=rownames(newdata_FAMD),newdata_FAMD)},
  options = list(    "orderClasses" = TRUE,
                     "responsive" = TRUE,
                     "pageLength" = 10))

output$summary_FAMD=renderPrint({
  summary(newdata_FAMD)
})


code<-function(){
  vec=nomData
  part2=""
  if(!is.null(input$supvar_FAMD)||!is.null(input$supvar1_FAMD)){
    choixsup=getactive_FAMD()$sup
    # vect3<-choixsup[1]
    # if (length(choixsup)>1){
    # for(i in 2:length(choixsup)){
    # vect3<-paste(vect3,paste(choixsup[i],sep=""),sep=",")
    # }
    # }
    vect3 <- paste(choixsup,collapse=",")
    part2=paste(",sup.var=c(",vect3,")",sep="")
  }
  part3=""
  if(!is.null(input$indsup)){
    # suple=c()
    # for (i in 1:length(nom_FAMD)){
    # if(nom_FAMD[i]%in%input$indsup){
    # suple=c(suple,i)
    # }
    # }
    suple=which(nom_FAMD%in%input$indsup)
    # vect4=NULL
    # vect4<-paste(vect4,suple[1],sep="")
    # if(length(suple)>1){
    # for(i in 2:length(suple)){
    # vect4<-paste(vect4,paste(suple[i],sep=""),sep=",")
    # }
    # }
    vect4 <- paste(suple,collapse=",")
    if(part2!=""){
      part3=paste("ind.sup=c(",vect4,")",sep="")
    }
    else{
      part3=paste(",ind.sup=c(",vect4,")",sep="")
    }
  }
  Call1=as.name(paste("res.FAMD<-FAMD(",vec,part2,part3,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)),")",sep=""))
  return(Call1)
}

# Attention, si le nombre d'individus passe en dessous de 10, bug
output$summary_FAMD2=renderPrint({
  validate(
    need(input$nbele!=0, gettext("Please select at least one element"))
  )
  a<-values_FAMD()$res.FAMD
  a$call$call<-code()
  summary.FAMD(a,nbelements=input$nbele)
})

output$summary2_FAMD=downloadHandler(filename = function() {
  paste('summaryofFAMD','.txt', sep='')
},
content = function(file) {
  summary.FAMD(values_FAMD()$res.FAMD,nbelements=input$nbele,file=file)
},
contentType='text/csv')


output$slider3=renderUI({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(input$selecactive_FAMD==gettext("All")){
    maxvar=length(all)
  }
  if(input$selecactive_FAMD==gettext("Choose")){
    maxvar=length(getactive_FAMD()$activevar)
  }
  if(selection3=="contrib"){
    return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                          min=1,max=maxvar,value=selection4,step=1)))
  }
  else{
    return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                          min=1,max=maxvar,value=maxvar,step=1)))}
})

output$slider5=renderUI({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(input$selecactive_FAMD==gettext("All")){
    maxvar=length(quanti)
  }
  if(input$selecactive_FAMD==gettext("Choose")){
    maxvar=length(getactive_FAMD()$quanti)
  }
  if(selection5=="contrib"){
    return(div(align="center",sliderInput("slider6",label=gettext("Number of the most contributive variables"),
                                          min=1,max=maxvar,value=selection6,step=1)))
  }
  else{
    return(div(align="center",sliderInput("slider6",label=gettext("Number of the most contributive variables"),
                                          min=1,max=maxvar,value=maxvar,step=1)))}
})

output$slider7=renderUI({
  validate(
    need(length(getactive_FAMD()$activevar)>1 || input$selecactive_FAMD==gettext("All"),gettext("Please select at least one supplementary variable"))
  )
  if(is.null(input$indsup)){
    maxi=length(nom_FAMD)
  }
  if(!is.null(input$indsup)){
    maxi=length(nom_FAMD)-length(input$indsup)
  }
  if(selection=="contrib"){
    return(div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals"),
                                          min = 1, max = maxi, value =as.numeric(selection2),step=1)))
  }
  else{
    return(div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals"),
                                          min = 1, max = maxi, value =maxi,step=1)))
  }
})


output$habillage2=renderUI({
  if(length(QualiChoice)==0){
    return(p(gettext("No categorical variable")))
  }
  if(length(QualiChoice)>1){
    if(is.null(habillageind)){
      num=c(1:length(QualiChoice))
      return(selectInput("habiller","", choices=list(num=QualiChoice),multiple=FALSE))
    }
    else{
      num=c(1:length(QualiChoice))
      return(selectInput("habiller","", choices=list(num=QualiChoice),multiple=FALSE,selected=habillageind))
    }
  }
  if(length(QualiChoice)==1){
    if(is.null(habillageind)){
      return(selectInput("habiller","", choices=QualiChoice,multiple=FALSE))
    }
    else{
      return(selectInput("habiller","", choices=QualiChoice,multiple=FALSE,selected=habillageind))
    }
  }
})

output$histo_FAMD=renderPlot({
  if(input$bam_FAMD%in%quanti){
    par(mfrow=c(1,2))
    boxplot(newdata_FAMD[,input$bam_FAMD])
    hist(newdata_FAMD[,input$bam_FAMD],main="",xlab="")
  }
  else{
    barplot(table(newdata_FAMD[,input$bam_FAMD]),cex.names=0.8)
  }
})



output$downloadData_FAMD = downloadHandler(
  filename = function() {
    paste('graph1','.png', sep='')
  },
  content = function(file) {
    png(file)
    Plot11_FAMD()
    dev.off()
  },
  contentType='image/png')

output$downloadData1_FAMD = downloadHandler(
  filename = function() {
    paste('graph1','.jpg', sep='')
  },
  content = function(file) {
    jpeg(file)
    Plot11_FAMD()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData2_FAMD = downloadHandler(
  filename = function() {
    paste('graph1','.pdf', sep='')
  },
  content = function(file) {
    pdf(file)
    Plot11_FAMD()
    dev.off()
  },
  contentType=NA)

output$downloadData3_FAMD = downloadHandler(
  filename = function() {
    paste('graph2','.png', sep='')
  },
  content = function(file) {
    png(file)
    Plot22_FAMD()
    dev.off()
  },
  contentType='image/png')

output$downloadData4_FAMD = downloadHandler(
  filename = function() {
    paste('graph1','.jpg', sep='')
  },
  content = function(file) {
    jpeg(file)
    Plot22_FAMD()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData5_FAMD = downloadHandler(
  filename = function() {
    paste('graph1','.pdf', sep='')
  },
  content = function(file) {
    pdf(file)
    Plot22_FAMD()
    dev.off()
  },
  contentType=NA)

output$downloadData6_FAMD = downloadHandler(
  filename = function() {
    paste('graph3','.jpg', sep='')
  },
  content = function(file) {
    jpeg(file)
    Plot33_FAMD()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData7_FAMD = downloadHandler(
  filename = function() {
    paste('graph3','.png', sep='')
  },
  content = function(file) {
    png(file)
    Plot33_FAMD()
    dev.off()
  },
  contentType='image/png')

output$downloadData8_FAMD = downloadHandler(
  filename = function() {
    paste('graph3','.pdf', sep='')
  },
  content = function(file) {
    pdf(file)
    Plot33_FAMD()
    dev.off()
  },
  contentType=NA)


Plot11_FAMD=function(){
  if(input$select0=="cos2"){
    if(input$slider00!=1){
      selecindiv=paste("cos2 ",input$slider00)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  if(input$select0=="NONE"){
    selecindiv=NULL
    selecindivText="NULL"
  }
  if(input$select0=="contrib"){
    selecindiv=paste("contrib ",input$slider4)
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  plot.FAMD(values_FAMD()$res.FAMD,axes=c(as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)),choix="var",cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2_FAMD,select=selecindiv)
}
Plot22_FAMD=function(){
  hab="none"
  if(length(QualiChoice)==0){
    hab="none"
  }
  else if(length(QualiChoice)==1 && input$habi==TRUE){
    if(input$habi==TRUE){
      hab=QualiChoice
      colquali="blue"
    }
    else{
      hab="none"
      colquali="magenta"
    }
  }
  else if (length(QualiChoice)>1){
    if(input$habi==TRUE){
      if(is.null(input$habiller)){
        hab="none"
      }
      else{
        hab=input$habiller
      }
    }
    else{
      hab="none"
      colquali="magenta"
    }
  }
  if(hab!="none"){
    hab=which(all==hab)
    hab=as.numeric(hab)
  }
  if(input$select=="cos2"){
    if(input$slider1!=1){
      selecindiv=paste("cos2 ",input$slider1)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivtext=paste0("'",selecindiv,"'")
  }
  if(input$select=="NONE"){
    selecindiv=NULL
  }
  if(input$select=="contrib"){
    selecindiv=paste("contrib ",input$slider0)
  }
  if(input$select=="Manuel"){
    selecindiv=c(input$indiv)
  }
  plot.FAMD(values_FAMD()$res.FAMD,axes=c(as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)),choix="ind",select=selecindiv,lab.var=input$labels,lab.ind=input$labels2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,habillage=hab,title=input$title1_FAMD)
}
Plot33_FAMD=function(){
  if(input$selecti=="cos2"){
    if(input$slider000!=1){
      selecindiv=paste("cos2 ",input$slider000)
    }
    else{
      selecindiv="cos2 0.999"
    }
    selecindivText=paste("'",selecindiv,"'",sep="")
  }
  if(input$selecti=="NONE"){
    selecindiv=NULL
    selecindivText="NULL"
  }
  if(input$selecti=="contrib"){
    selecindiv=paste("contrib ",input$slider6)
    paste("'",selecindiv,"'",sep="")
  }
  plot.FAMD(values_FAMD()$res.FAMD,axes=c(as.numeric(input$nb1_FAMD),as.numeric(input$nb2_FAMD)),choix="quanti",cex=input$cex3,cex.main=input$cex3,cex.axis=input$cex3,title=input$title3_FAMD,select=selecindiv)
}

# 
# 
