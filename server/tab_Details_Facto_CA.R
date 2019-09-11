


output$facto_meta_ca_ui<-renderUI({
  values$Details_Data_FA
  return(
    selectInput(inputId = "facto_meta_ca",label = "Choose meta data field",choices = colnames(values$facto_meta))
  )
})

observe({
  validate(
    need(!is.null(input$facto_meta_ca),message=F)
  )
  x<-as.data.frame(t(as.matrix(values$facto_dtm)))
  colnames(x)<-isolate(values$facto_meta[,input$facto_meta_ca])
  
  x<-as.data.frame(t(rowsum(t(x), group = colnames(x), na.rm = T)))
  #browser()
  if(is.data.frame(x)==TRUE){
    if(dim(x)[1]<3 || dim(x)[2]<3){
      shinyalert::shinyalert('not enough row/column',type = "warning")
    }
    else{
      assign("nomData",paste0("Collection ",isolate(values$facto_info[[5]])),envir=.GlobalEnv)
      assign("newdata",x,envir=.GlobalEnv)
      assign("colonnesup",NULL,envir=.GlobalEnv)
      assign("lignesup",NULL,envir=.GlobalEnv)
      assign("catsup",NULL,envir=.GlobalEnv)
      assign("axe1",1,envir=.GlobalEnv)
      assign("axe2",2,envir=.GlobalEnv)
      assign("Invisible",NULL,envir=.GlobalEnv)
      assign("selec1",gettext("No selection"),envir=.GlobalEnv)
      assign("selec2",gettext("No selection"),envir=.GlobalEnv)
      assign("valueselec1",NULL,envir=.GlobalEnv)
      assign("valueselec2",NULL,envir=.GlobalEnv)
      assign("size",1,envir=.GlobalEnv)
      assign("title1",gettext("CA factor map"),envir=.GlobalEnv)
      assign("col1","blue",envir=.GlobalEnv)
      assign("col2","red",envir=.GlobalEnv)
      assign("col3","darkblue",envir=.GlobalEnv)
      assign("col4","darkred",envir=.GlobalEnv)
      assign("ellipses",NULL,envir=.GlobalEnv)
      values$Facto_ready<-TRUE
      values$Facto_new<-runif(n = 1,min = 0,max = 1) 
      
    }
  }
})





output$Details_Facto_CA_UI<-renderUI({
  validate(
    need(values$Facto_ready==TRUE,message=FALSE)
  )
  values$Facto_new
  withna=c()
  rowna=c()
  nomrow=c()
  for (i in 1:dim(newdata)[2]){
    if(any(is.na(newdata[,i])==TRUE)){
      if(is.numeric(newdata[,i])==TRUE){
        withna=c(withna,colnames(newdata)[i])
      }
    }
  }
  
  for (i in 1:dim(newdata)[1]){
    if(any(is.na(newdata[i,])==TRUE)){
      rowna=c(rowna,i)
      nomrow=c(nomrow,rownames(newdata)[i])
    }
  }
  
  quanti=names(which(sapply(newdata,is.numeric)))
  quali=names(which(!(sapply(newdata,is.numeric))))
  VariableChoice=quanti
  noms=rownames(newdata)
  nums=c(1:length(noms))
  QualiChoice=quali
  IdChoice=c(1:length(VariableChoice))
  Idqualisup=c(1:length(QualiChoice))
  # sup=c()
  # for(i in IdChoice){
  # if(VariableChoice[i]%in%withna){
  # sup=c(sup,i)
  # }
  # }
  sup=which(VariableChoice%in%withna)
  if (length(sup)==0) sup=NULL
  
  #sup2=c()
  if(!(is.null(sup))){
    VariableChoices=VariableChoice[-sup]
  }
  if(is.null(sup)){
    VariableChoices=VariableChoice
  }
  IdChoices=1:length(VariableChoices)
  # for(i in nums){
  # if(noms[i]%in%nomrow){
  # sup2=c(sup2,i)
  # }
  # }
  sup2=which(noms%in%nomrow)
  if (length(sup2)==0) sup2 <- NULL
  if(!(is.null(sup2))){
    nom=noms[-sup2]
  }
  if(is.null(sup2)){
    nom=noms
  }
  num=c(1:length(nom))
  nomData=unlist(strsplit(as.character(nomData),"\\["))[1]
  
  assign("withna",withna,envir=.GlobalEnv)
  assign("VariableChoices",VariableChoices,envir=.GlobalEnv)
  assign("QualiChoice",QualiChoice,envir=.GlobalEnv)
  assign("rowna",rowna,envir=.GlobalEnv)
  assign("nom",nom,envir=.GlobalEnv)
  values$newdata<-newdata
  return(tagList(
    div(paste(gettext("CA on the "),nomData),style="color:#2A0A29"),
    sidebarLayout(
      sidebarPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
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
        )
        ,width=3),  
      mainPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
            tags$style(type = "text/css", "a{color: #2F0B3A;}"),
            tabsetPanel(id = "graph_sort_CA",
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
        )
        ,width=9)
    )
  )
  )
})




values_facto=reactive({
  if (input$selecactive==gettext("All")){
    data.selec=values$newdata[,VariableChoices]
  }
  else{
    validate(
      need(length(getactive()!=0), gettext("Please select at least one supplementary column"))
    )
    data.selec=values$newdata[,c(getactive())]
  }
  if(length(QualiChoice)==0){
    choixquali=NULL
  }
  else if (length(QualiChoice)==1){
    if(input$supquali==FALSE){
      choixquali=NULL
    }
    else{
      data.selec=cbind(data.selec,values$newdata[,QualiChoice])
      colnames(data.selec)[dim(data.selec)[2]]=QualiChoice
      choixquali=length(data.selec)
    }
  }
  else{
    if(length(input$supquali)==0){
      choixquali=NULL
    }
    else{
      data.selec=cbind(data.selec,values$newdata[,input$supquali])
      if(length(input$supquali)==1){
        choixquali=length(data.selec)
        colnames(data.selec)[choixquali]=input$supquali
      }
      else{
        choixquali=seq((dim(data.selec)[2]-length(input$supquali)+1),dim(data.selec)[2])
        colnames(data.selec)[choixquali]=input$supquali
      }
    }
  }
  if(length(input$supvar)==0){
    choixquanti=NULL
  }
  else {
    data.selec=cbind(data.selec,values$newdata[,input$supvar])
    if(length(input$supvar)==1){
      colnames(data.selec)[dim(data.selec)[2]]=input$supvar
      choixquanti=length(data.selec)
    }
    else{
      choixquanti=seq((dim(data.selec)[2]-length(input$supvar)+1),dim(data.selec)[2])
    }
  }
  if(length(input$rowsupl)!=0){
    # indexes=c()
    # for (i in 1:length(nom)){
    # if(nom[i]%in%input$rowsupl){
    # indexes=c(indexes,i)
    # }
    # }
    indexes=which(nom%in%input$rowsupl)
    if (length(indexes)==0) indexes=NULL
  }
  else{
    indexes=NULL
  }
  indexes=c(indexes,rowna)
  choixquanti2=NULL
  if(length(withna)!=0){
    data.selec=cbind(data.selec,values$newdata[,withna])
    if(length(withna)==1){
      colnames(data.selec)[dim(data.selec)[2]]=withna
      if(is.null(choixquanti)){
        choixquanti2=length(data.selec)
      }
      else{
        choixquanti2=c(choixquanti,length(data.selec))
      }
    }
    else{
      if(is.null(choixquanti)){
        choixquanti2=seq((dim(data.selec)[2]-length(withna)+1),dim(data.selec)[2])
      }
      else{
        choixquanti2=c(choixquanti,seq((dim(data.selec)[2]-length(withna)+1),dim(data.selec)[2]))
      }
    }
  }
  else{
    choixquanti2=choixquanti
  }
  list(res.CA=(CA(data.selec,quali.sup=choixquali,col.sup=choixquanti2,row.sup=indexes,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)))),DATA=(data.selec),CHOIXQUALI=(choixquali),CHOIXQUANTI=(choixquanti2),INDEXES=(indexes))
})


output$col1=renderUI({
  if(!is.null(values_facto()$res.CA$row)){
    return(colourpicker::colourInput("colrow",gettext("Colour of row points"),col1))
  }
})
output$col2=renderUI({
  if(!is.null(values_facto()$res.CA$col)){
    return(colourpicker::colourInput("colcol",gettext("Colour of column points"),col2))
  }
})
output$col3=renderUI({
  if(!is.null(values_facto()$res.CA$row.sup)){
    return(colourpicker::colourInput("colrowsup",gettext("Colour of supplementary row points"),col3))
  }
})
output$col4=renderUI({
  if(!is.null(values_facto()$res.CA$col.sup)){
    return(colourpicker::colourInput("colcolsup",gettext("Colour of supplementary column points"),col4))
  }
})

output$ellipses=renderUI({
  values_facto1=c()
  if(!is.null(values_facto()$res.CA$col)){
    values_facto1=c(values_facto1,gettext("Columns"))
  }
  if(!is.null(values_facto()$res.CA$row)){
    values_facto1=c(values_facto1,gettext("Rows"))
  }
  if(length(values_facto)!=0){
    if(is.null(ellipses)){
      return(checkboxGroupInput("ellip",h6(""),choices=values_facto1,selected=NULL,inline=TRUE))
    }else{
      return(checkboxGroupInput("ellip",h6(""),choices=values_facto1,selected=ellipses,inline=TRUE))
    }
  }
})

valeuretour=function(){
  res=list()
  res$data=newdata
  res$nomData=nomData
  # a : colonnes supplementaires
  res$a=input$supvar
  # b : lignes supplementaires
  res$b=input$rowsupl
  # c : colonnes quali
  choixquali=NULL
  if (length(QualiChoice)==1){
    if(input$supquali==TRUE){
      choixquali=QualiChoice
    }
  }
  else{
    if(length(input$supquali)!=0){
      choixquali=input$supquali
    }
  }
  res$c=choixquali
  # d et e : axes
  res$d=input$nb1
  res$e=input$nb2
  # f : invisible points 
  invisi=NULL
  if(length(input$invis)!=0){
    invisi=NULL
    if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
    if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
    if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
    if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
    if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
  }
  res$f=invisi
  res$type1=input$seleccol
  res$type2=input$selecrow
  res$selec1=NULL
  if(input$seleccol=="cos2"){
    res$selec1=input$slider3
  }
  if(input$seleccol=="contrib"){
    res$selec1=input$contrib1
  }
  res$selec2=NULL
  if(input$selecrow=="cos2"){
    res$selec2=input$slider4
  }
  if(input$seleccol=="contrib"){
    res$selec2=input$contrib2
  }
  res$taille=input$cex
  res$code1=Code()
  res$code2=CodeGraph()
  res$title1=input$title1
  res$anafact=values_facto()$res.CA
  if(is.null(input$colrow)){
    col1="blue"
  }else{
    col1=input$colrow
  }
  if(is.null(input$colcol)){
    col2="red"
  }else{
    col2=input$colcol
  }
  if(is.null(input$colrowsup)){
    col3="darkblue"
  }else{
    col3=input$colrowsup
  }
  if(is.null(input$colcolsup)){
    col4="darkred"
  }else{
    col4=input$colcolsup
  }
  res$col1=col1
  res$col2=col2
  res$col3=col3
  res$col4=col4
  res$ellip=input$ellip
  class(res) <- "CAshiny"
  return(res)
}


Code=function(){
  
  vecquant<-values_facto()$CHOIXQUANTI
  vecqual<-values_facto()$CHOIXQUALI
  Datasel<-values_facto()$DATA
  indexes<-values_facto()$INDEXES
  
  vec2 <- paste("'",paste(colnames(Datasel),collapse="','"),"'",sep="")
  vecfinal<-paste(nomData,"[,c(",vec2,")","]",sep="")
  
  vecquant1 <- paste("c(",paste(vecquant,collapse=","),")",sep="")
  vecquant2<-vecquant
  
  vecqual1 <- paste("c(",paste(vecqual,collapse=","),")",sep="")
  vecqual2<-vecqual
  
  indexes1 <- paste("c(",paste(indexes,collapse=","),")",sep="")
  indexes2<-indexes
  
  if(length(vecqual)==0){
    vecqual<-"NULL" 
  }
  else if(length(vecqual)==1){
    vecqual<-vecqual
  }
  else if(length(vecqual)>1){
    vecqual<-vecqual2
  }
  
  if(length(vecquant)==0){
    vecquant<-"NULL"  
  }
  else if(length(vecquant)==1){
    vecquant
  }
  else if(length(vecquant)>1){
    vecquant<-vecquant1
  }
  
  
  if(length(indexes)==0){
    indexes<-"NULL"  
  }
  else if(length(indexes)==1){
    indexes
  }
  else if(length(indexes)>1){
    indexes<-indexes1
  }
  Call1=as.name(paste("res.CA=CA(",vecfinal,",quali.sup=",vecqual,",col.sup=",vecquant,",row.sup=",indexes,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep=""))
  return(Call1)
}

CodeGraph=function(){
  sel="NULL"
  if(input$seleccol=="cos2"){
    if(input$slider3!=1){
      sel=paste("cos2 ",input$slider3)
    }
    else{
      sel="cos2 0.999"
    }
  }
  if(input$seleccol=="contrib"){
    sel=paste("contrib ",input$contrib1)
  }
  sel2="NULL"
  if(input$selecrow=="cos2"){
    if(input$slider4!=1){
      sel2=paste("cos2 ",input$slider4)
    }
    else{
      sel2="cos2 0.999"
    }
  }
  if(input$selecrow=="contrib"){
    sel2=paste("contrib ",input$contrib2)
  }
  if(is.null(input$colrow)){
    col1="blue"
  }else{
    col1=input$colrow
  }
  if(is.null(input$colcol)){
    col2="red"
  }else{
    col2=input$colcol
  }
  if(is.null(input$colrowsup)){
    col3="darkblue"
  }else{
    col3=input$colrowsup
  }
  if(is.null(input$colcolsup)){
    col4="darkred"
  }else{
    col4=input$colcolsup
  }
  if(is.null(input$ellip)||length(input$ellip)==0){
    Call2=paste('plot.CA(res.CA,axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectCol="',sel,'",selectRow="',sel2,'",unselect=0,cex=',input$cex,',title="',input$title1,'",col.row="',col1,'",col.col="',col2,'",col.row.sup="',col3,'",col.col.sup="',col4,'",invisible=',Plot1()$invisiText,')',sep='')
  }else{
    vect=c()
    if(gettext("Columns")%in%input$ellip){
      vect=c(vect,"col")
    }
    if(gettext("Rows")%in%input$ellip){
      vect=c(vect,"row")
    }
    myellip=paste(paste("'",vect,"'",sep=""),collapse=",")
    Call2=paste('ellipseCA(res.CA,ellipse=c(',myellip,'),axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectCol="',sel,'",selectRow="',sel2,'",unselect=0,cex=',input$cex,',title="',input$title1,'",col.row="',col1,'",col.col="',col2,'",col.row.sup="',col3,'",col.col.sup="',col4,'",invisible=',Plot1()$invisiText,')',sep='')
  }
  
  return(Call2)
}

Plot1=reactive({
  validate(
    need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
  )
  validate(
    need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more columns"))
  )
  if(length(input$invis)==0){
    invisi="none"
    invisiText=paste("'","none","'",sep="")
  }
  if(length(input$invis)!=0){
    invisi=NULL
    if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
    if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
    if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
    if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
    if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
    invisiText=invisi
    invisiText=paste("c(",paste(paste("'",invisi,"'",sep=""),collapse = ","),")",sep="")
  }
  sel=NULL
  if(input$seleccol=="cos2"){
    if(input$slider3!=1){
      sel=paste("cos2 ",input$slider3)
    }
    else{
      sel="cos2 0.999"
    }
  }
  if(input$seleccol=="contrib"){
    sel=paste("contrib ",input$contrib1)
  }
  sel2=NULL
  if(input$selecrow=="cos2"){
    if(input$slider4!=1){
      sel2=paste("cos2 ",input$slider4)
    }
    else{
      sel2="cos2 0.999"
    }
  }
  if(input$selecrow=="contrib"){
    sel2=paste("contrib ",input$contrib2)
  }
  values_facto2=c()
  if(!is.null(input$ellip)){
    if(gettext("Columns")%in%input$ellip){
      values_facto2=c(values_facto2,"col")
    }
    if(gettext("Rows")%in%input$ellip){
      values_facto2=c(values_facto2,"row")
    }
  }
  if(is.null(input$colrowsup)){
    colrowsup="darkblue"
  }else{
    colrowsup=input$colrowsup
  }
  if(is.null(input$colcolsup)){
    colcolsup="darkred"
  }else{
    colcolsup=input$colcolsup
  }
  list(PLOT1=(plot.CA(values_facto()$res.CA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,title=input$title1,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)),invisiText=(invisiText),
       PLOT2=(ellipseCA(values_facto()$res.CA,ellipse=values_facto2,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,title=input$title1,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)))
})

output$map <- renderPlot({
  if(is.null(input$ellip)||length(input$ellip)==0){
    p <- Plot1()$PLOT1
  }else{
    p=Plot1()$PLOT2
  }
})


getactive=function(){
  if(input$selecactive==gettext("Choose")){
    sup=NULL
    if(length(input$supvar)==0){
      activevar=VariableChoices
    }
    else{
      sup=which(VariableChoices%in%input$supvar)
      activevar=VariableChoices[-sup]
    }
    return(activevar)
  }
}

output$contribcol=renderUI({
  maxx=dim(values_facto()$res.CA$col$coord)[1]
  if(selec1=="contrib"){
    return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=values_factoelec2,step=1))
  }
  else{
    return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=maxx,step=1))
  }
  
})

output$contribrow=renderUI({
  maxx=dim(values_facto()$res.CA$row$coord)[1]
  if(selec2=="contrib"){
    return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=valueselec2,step=1))
  }
  else{
    return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=maxx,step=1))
  }
})

output$out22=renderUI({
  choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results for the columns"),gettext("Results for the rows"))
  if(!is.null(values_facto()$INDEXES)){
    choix=c(choix,gettext("Results for the supplementary rows"))
  }
  if(!is.null(values_facto()$CHOIXQUANTI)){
    choix=c(choix,gettext("Results for the supplementary columns"))
  }
  if(!is.null(values_facto()$CHOIXQUALI)){
    choix=c(choix,gettext("Results for the categorical variables"))
  }
  radioButtons("out",gettext("Which outputs do you want?"),
               choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
})

output$warn=renderPrint({
  if(length(withna)!=0){
    baba=paste(withna,collapse=", ")
    bibi=paste(nomrow,collapse=", ")
    a=paste0(gettext("Warning: "), baba, gettext(" have NA : they are considered as supplementary columns"))
    b=paste0(gettext("Warning: "), bibi, gettext(" have NA : they are considered as supplementary rows"))
    return(cat(a,b,sep="\n"))
  }
})

output$NB1=renderUI({
  validate(
    need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary column"))
  )
  if(input$selecactive==gettext("All") || length(getactive())>5){
    # return(selectInput("nb1", label = h6("x axis"), 
    # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe1,width='80%'))
    return(textInput("nb1", label = h6(gettext("x axis")), axe1,width='50%'))
  }
  else{
    baba=c(1:length(getactive()))
    return(selectInput("nb1",label=h6("x axis"), choices=baba,selected=axe1,width='80%'))
  }
})

output$NB2=renderUI({
  validate(
    need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary column"))
  )
  if(input$selecactive==gettext("All") || length(getactive())>5){
    # return(selectInput("nb2", label = h6("y axis"), 
    # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe2,width='80%'))
    return(textInput("nb2", label = h6(gettext("y axis")), axe2,width='50%'))
  }
  else{
    baba=c(1:length(getactive()))
    return(selectInput("nb2",label=h6("y axis"), choices=baba,selected=axe2,width='80%'))
  }
})


output$sorties=renderTable({
  return(as.data.frame(values_facto()$res.CA$eig))
},rownames=TRUE)

output$sorties1=renderTable({
  validate(
    need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
  )
  return(as.data.frame(values_facto()$res.CA$col$coord))
},rownames=TRUE)

output$sorties2=renderTable({
  validate(
    need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
  )
  return(as.data.frame(values_facto()$res.CA$col$cos2))
},rownames=TRUE)

output$sorties3=renderTable({
  validate(
    need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
  )
  return(as.data.frame(values_facto()$res.CA$col$contrib))
},rownames=TRUE)

output$sorties4=renderTable({
  return(as.data.frame(values_facto()$res.CA$row$coord))
},rownames=TRUE)

output$sorties5=renderTable({
  return(as.data.frame(values_facto()$res.CA$row$cos2))
},rownames=TRUE)

output$sorties6=renderTable({
  return(as.data.frame(values_facto()$res.CA$row$contrib))
},rownames=TRUE)

output$sorties7=renderTable({
  validate(
    need((length(input$rowsupl)>0), gettext("No supplementary rows selected"))
  )
  return(as.data.frame(values_facto()$res.CA$row.sup$coord))
},rownames=TRUE)

output$sorties8=renderTable({
  return(as.data.frame(values_facto()$res.CA$row.sup$cos2))
},rownames=TRUE)

output$sorties9=renderTable({
  return(as.data.frame(values_facto()$res.CA$col.sup$coord))
},rownames=TRUE)

output$sorties10=renderTable({
  return(as.data.frame(values_facto()$res.CA$col.sup$cos2))
},rownames=TRUE)

output$sorties11=renderTable({
  validate(
    need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
  )
  return(as.data.frame(values_facto()$res.CA$quali.sup))
},rownames=TRUE)



output$map3=renderPlot({
  return(barplot(values_facto()$res.CA$eig[,1],names.arg=rownames(values_facto()$res.CA$eig),las=2))
})

### Fonction permettant l'affichage du JDD sous la forme d'un DataTable, qui permet la recherche de donnes. 
output$JDD=renderDataTable({
  cbind(Names=rownames(newdata),newdata)},
  options = list(    "orderClasses" = TRUE,
                     "responsive" = TRUE,
                     "pageLength" = 10))

### Fonction permettant l'affichage du summary du JDD
output$summary=renderPrint({
  summary(newdata)
})


### Fonction permettant l'affichage du summary de la fonction CA sur le JDD
output$summaryCA=renderPrint({
  a<-values_facto()$res.CA
  a$call$call<-Code()
  summary.CA(a,nbelements=input$nbele)
})

output$summary2=downloadHandler(filename = function() { 
  paste('summaryofCA','.txt', sep='') 
},
content = function(file) {
  summary.CA(values_facto()$res.CA,nbelements=input$nbele,file=file)
},
contentType='text/csv')


## Creation des fonctions permettant l'enregistrement des graphs sous les formats : png, jpeg, pdf et emf
output$downloadData = downloadHandler(
  filename = function() { 
    paste('graph1','.png', sep='') 
  },
  content = function(file) {
    png(file)
    Plot11()
    dev.off()
  },
  contentType='image/png')

output$downloadData1 = downloadHandler(
  filename = function() { 
    paste('graph1','.jpg', sep='') 
  },
  content = function(file) {
    jpeg(file)
    Plot11()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData2 = downloadHandler(
  filename = function() { 
    paste('graph1','.pdf', sep='') 
  },
  content = function(file) {
    pdf(file)
    Plot11()
    dev.off()
  },
  contentType=NA)

Plot11=function(){
  if(length(input$invis)==0){
    invisi="none"
  }
  if(length(input$invis)!=0){
    invisi=NULL
    if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
    if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
    if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
    if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
    if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
  }
  sel=NULL
  if(input$seleccol=="cos2"){
    if(input$slider3!=1){
      sel=paste("cos2 ",input$slider3)
    }
    else{
      sel="cos2 0.999"
    }
  }
  if(input$seleccol=="contrib"){
    sel=paste("contrib ",input$contrib1)
  }
  sel2=NULL
  
  values_facto=reactive({
    if (input$selecactive==gettext("All")){
      data.selec=newdata[,VariableChoices]
    }
    else{
      validate(
        need(length(getactive()!=0), gettext("Please select at least one supplementary column"))
      )
      data.selec=newdata[,c(getactive())]
    }
    if(length(QualiChoice)==0){
      choixquali=NULL
    }
    else if (length(QualiChoice)==1){
      if(input$supquali==FALSE){
        choixquali=NULL
      }
      else{
        data.selec=cbind(data.selec,newdata[,QualiChoice])
        colnames(data.selec)[dim(data.selec)[2]]=QualiChoice
        choixquali=length(data.selec)
      }
    }
    else{
      if(length(input$supquali)==0){
        choixquali=NULL
      }
      else{
        data.selec=cbind(data.selec,newdata[,input$supquali])
        if(length(input$supquali)==1){
          choixquali=length(data.selec)
          colnames(data.selec)[choixquali]=input$supquali
        }
        else{
          choixquali=seq((dim(data.selec)[2]-length(input$supquali)+1),dim(data.selec)[2])
          colnames(data.selec)[choixquali]=input$supquali
        }
      }
    }
    if(length(input$supvar)==0){
      choixquanti=NULL
    }
    else {
      data.selec=cbind(data.selec,newdata[,input$supvar])
      if(length(input$supvar)==1){
        colnames(data.selec)[dim(data.selec)[2]]=input$supvar
        choixquanti=length(data.selec)
      }
      else{
        choixquanti=seq((dim(data.selec)[2]-length(input$supvar)+1),dim(data.selec)[2])
      }
    }
    if(length(input$rowsupl)!=0){
      # indexes=c()
      # for (i in 1:length(nom)){
      # if(nom[i]%in%input$rowsupl){
      # indexes=c(indexes,i)
      # }
      # }
      indexes=which(nom%in%input$rowsupl)
      if (length(indexes)==0) indexes=NULL
    }
    else{
      indexes=NULL
    }
    indexes=c(indexes,rowna)
    choixquanti2=NULL
    if(length(withna)!=0){
      data.selec=cbind(data.selec,newdata[,withna])
      if(length(withna)==1){
        colnames(data.selec)[dim(data.selec)[2]]=withna
        if(is.null(choixquanti)){
          choixquanti2=length(data.selec)
        }
        else{
          choixquanti2=c(choixquanti,length(data.selec))
        }
      }
      else{
        if(is.null(choixquanti)){
          choixquanti2=seq((dim(data.selec)[2]-length(withna)+1),dim(data.selec)[2])
        }
        else{
          choixquanti2=c(choixquanti,seq((dim(data.selec)[2]-length(withna)+1),dim(data.selec)[2]))
        }
      }
    }
    else{
      choixquanti2=choixquanti
    }
    list(res.CA=(CA(data.selec,quali.sup=choixquali,col.sup=choixquanti2,row.sup=indexes,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)))),DATA=(data.selec),CHOIXQUALI=(choixquali),CHOIXQUANTI=(choixquanti2),INDEXES=(indexes))
  })
  
  
  output$col1=renderUI({
    if(!is.null(values_facto()$res.CA$row)){
      return(colourpicker::colourInput("colrow",gettext("Colour of row points"),col1))
    }
  })
  output$col2=renderUI({
    if(!is.null(values_facto()$res.CA$col)){
      return(colourpicker::colourInput("colcol",gettext("Colour of column points"),col2))
    }
  })
  output$col3=renderUI({
    if(!is.null(values_facto()$res.CA$row.sup)){
      return(colourpicker::colourInput("colrowsup",gettext("Colour of supplementary row points"),col3))
    }
  })
  output$col4=renderUI({
    if(!is.null(values_facto()$res.CA$col.sup)){
      return(colourpicker::colourInput("colcolsup",gettext("Colour of supplementary column points"),col4))
    }
  })
  
  output$ellipses=renderUI({
    values_facto1=c()
    if(!is.null(values_facto()$res.CA$col)){
      values_facto1=c(values_facto1,gettext("Columns"))
    }
    if(!is.null(values_facto()$res.CA$row)){
      values_facto1=c(values_facto1,gettext("Rows"))
    }
    if(length(values_facto)!=0){
      if(is.null(ellipses)){
        return(checkboxGroupInput("ellip",h6(""),choices=values_facto1,selected=NULL,inline=TRUE))
      }else{
        return(checkboxGroupInput("ellip",h6(""),choices=values_facto1,selected=ellipses,inline=TRUE))
      }
    }
  })
  
  
  Code=function(){
    
    vecquant<-values_facto()$CHOIXQUANTI
    vecqual<-values_facto()$CHOIXQUALI
    Datasel<-values_facto()$DATA
    indexes<-values_facto()$INDEXES
    
    vec2 <- paste("'",paste(colnames(Datasel),collapse="','"),"'",sep="")
    vecfinal<-paste(nomData,"[,c(",vec2,")","]",sep="")
    
    vecquant1 <- paste("c(",paste(vecquant,collapse=","),")",sep="")
    vecquant2<-vecquant
    
    vecqual1 <- paste("c(",paste(vecqual,collapse=","),")",sep="")
    vecqual2<-vecqual
    
    indexes1 <- paste("c(",paste(indexes,collapse=","),")",sep="")
    indexes2<-indexes
    
    if(length(vecqual)==0){
      vecqual<-"NULL" 
    }
    else if(length(vecqual)==1){
      vecqual<-vecqual
    }
    else if(length(vecqual)>1){
      vecqual<-vecqual2
    }
    
    if(length(vecquant)==0){
      vecquant<-"NULL"  
    }
    else if(length(vecquant)==1){
      vecquant
    }
    else if(length(vecquant)>1){
      vecquant<-vecquant1
    }
    
    
    if(length(indexes)==0){
      indexes<-"NULL"  
    }
    else if(length(indexes)==1){
      indexes
    }
    else if(length(indexes)>1){
      indexes<-indexes1
    }
    Call1=as.name(paste("res.CA=CA(",vecfinal,",quali.sup=",vecqual,",col.sup=",vecquant,",row.sup=",indexes,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep=""))
    return(Call1)
  }
  
  CodeGraph=function(){
    sel="NULL"
    if(input$seleccol=="cos2"){
      if(input$slider3!=1){
        sel=paste("cos2 ",input$slider3)
      }
      else{
        sel="cos2 0.999"
      }
    }
    if(input$seleccol=="contrib"){
      sel=paste("contrib ",input$contrib1)
    }
    sel2="NULL"
    if(input$selecrow=="cos2"){
      if(input$slider4!=1){
        sel2=paste("cos2 ",input$slider4)
      }
      else{
        sel2="cos2 0.999"
      }
    }
    if(input$selecrow=="contrib"){
      sel2=paste("contrib ",input$contrib2)
    }
    if(is.null(input$colrow)){
      col1="blue"
    }else{
      col1=input$colrow
    }
    if(is.null(input$colcol)){
      col2="red"
    }else{
      col2=input$colcol
    }
    if(is.null(input$colrowsup)){
      col3="darkblue"
    }else{
      col3=input$colrowsup
    }
    if(is.null(input$colcolsup)){
      col4="darkred"
    }else{
      col4=input$colcolsup
    }
    if(is.null(input$ellip)||length(input$ellip)==0){
      Call2=paste('plot.CA(res.CA,axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectCol="',sel,'",selectRow="',sel2,'",unselect=0,cex=',input$cex,',title="',input$title1,'",col.row="',col1,'",col.col="',col2,'",col.row.sup="',col3,'",col.col.sup="',col4,'",invisible=',Plot1()$invisiText,')',sep='')
    }else{
      vect=c()
      if(gettext("Columns")%in%input$ellip){
        vect=c(vect,"col")
      }
      if(gettext("Rows")%in%input$ellip){
        vect=c(vect,"row")
      }
      myellip=paste(paste("'",vect,"'",sep=""),collapse=",")
      Call2=paste('ellipseCA(res.CA,ellipse=c(',myellip,'),axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectCol="',sel,'",selectRow="',sel2,'",unselect=0,cex=',input$cex,',title="',input$title1,'",col.row="',col1,'",col.col="',col2,'",col.row.sup="',col3,'",col.col.sup="',col4,'",invisible=',Plot1()$invisiText,')',sep='')
    }
    
    return(Call2)
  }
  
  Plot1=reactive({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
    )
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more columns"))
    )
    if(length(input$invis)==0){
      invisi="none"
      invisiText=paste("'","none","'",sep="")
    }
    if(length(input$invis)!=0){
      invisi=NULL
      if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
      if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
      if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
      if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
      if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
      invisiText=invisi
      invisiText=paste("c(",paste(paste("'",invisi,"'",sep=""),collapse = ","),")",sep="")
    }
    sel=NULL
    if(input$seleccol=="cos2"){
      if(input$slider3!=1){
        sel=paste("cos2 ",input$slider3)
      }
      else{
        sel="cos2 0.999"
      }
    }
    if(input$seleccol=="contrib"){
      sel=paste("contrib ",input$contrib1)
    }
    sel2=NULL
    if(input$selecrow=="cos2"){
      if(input$slider4!=1){
        sel2=paste("cos2 ",input$slider4)
      }
      else{
        sel2="cos2 0.999"
      }
    }
    if(input$selecrow=="contrib"){
      sel2=paste("contrib ",input$contrib2)
    }
    values_facto2=c()
    if(!is.null(input$ellip)){
      if(gettext("Columns")%in%input$ellip){
        values_facto2=c(values_facto2,"col")
      }
      if(gettext("Rows")%in%input$ellip){
        values_facto2=c(values_facto2,"row")
      }
    }
    if(is.null(input$colrowsup)){
      colrowsup="darkblue"
    }else{
      colrowsup=input$colrowsup
    }
    if(is.null(input$colcolsup)){
      colcolsup="darkred"
    }else{
      colcolsup=input$colcolsup
    }
    list(PLOT1=(plot.CA(values_facto()$res.CA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,title=input$title1,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)),invisiText=(invisiText),
         PLOT2=(ellipseCA(values_facto()$res.CA,ellipse=values_facto2,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,title=input$title1,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)))
  })
  
  output$map <- renderPlot({
    if(is.null(input$ellip)||length(input$ellip)==0){
      p <- Plot1()$PLOT1
    }else{
      p=Plot1()$PLOT2
    }
  })
  
  
  getactive=function(){
    if(input$selecactive==gettext("Choose")){
      sup=NULL
      if(length(input$supvar)==0){
        activevar=VariableChoices
      }
      else{
        sup=which(VariableChoices%in%input$supvar)
        activevar=VariableChoices[-sup]
      }
      return(activevar)
    }
  }
  
  output$contribcol=renderUI({
    maxx=dim(values_facto()$res.CA$col$coord)[1]
    if(selec1=="contrib"){
      return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=values_factoelec2,step=1))
    }
    else{
      return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=maxx,step=1))
    }
    
  })
  
  output$contribrow=renderUI({
    maxx=dim(values_facto()$res.CA$row$coord)[1]
    if(selec2=="contrib"){
      return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=valueselec2,step=1))
    }
    else{
      return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=maxx,step=1))
    }
  })
  
  output$out22=renderUI({
    choix=list(gettext("Summary of outputs"),gettext("Eigenvalues_facto"),gettext("Results for the columns"),gettext("Results for the rows"))
    if(!is.null(values_facto()$INDEXES)){
      choix=c(choix,gettext("Results for the supplementary rows"))
    }
    if(!is.null(values_facto()$CHOIXQUANTI)){
      choix=c(choix,gettext("Results for the supplementary columns"))
    }
    if(!is.null(values_facto()$CHOIXQUALI)){
      choix=c(choix,gettext("Results for the categorical variables"))
    }
    radioButtons("out",gettext("Which outputs do you want?"),
                 choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
  })
  
  output$warn=renderPrint({
    if(length(withna)!=0){
      baba=paste(withna,collapse=", ")
      bibi=paste(nomrow,collapse=", ")
      a=paste0(gettext("Warning: "), baba, gettext(" have NA : they are considered as supplementary columns"))
      b=paste0(gettext("Warning: "), bibi, gettext(" have NA : they are considered as supplementary rows"))
      return(cat(a,b,sep="\n"))
    }
  })
  
  output$NB1=renderUI({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary column"))
    )
    if(input$selecactive==gettext("All") || length(getactive())>5){
      # return(selectInput("nb1", label = h6("x axis"), 
      # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe1,width='80%'))
      return(textInput("nb1", label = h6(gettext("x axis")), axe1,width='50%'))
    }
    else{
      baba=c(1:length(getactive()))
      return(selectInput("nb1",label=h6("x axis"), choices=baba,selected=axe1,width='80%'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary column"))
    )
    if(input$selecactive==gettext("All") || length(getactive())>5){
      # return(selectInput("nb2", label = h6("y axis"), 
      # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe2,width='80%'))
      return(textInput("nb2", label = h6(gettext("y axis")), axe2,width='50%'))
    }
    else{
      baba=c(1:length(getactive()))
      return(selectInput("nb2",label=h6("y axis"), choices=baba,selected=axe2,width='80%'))
    }
  })
  
  
  output$sorties=renderTable({
    return(as.data.frame(values_facto()$res.CA$eig))
  },rownames=TRUE)
  
  output$sorties1=renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
    )
    return(as.data.frame(values_facto()$res.CA$col$coord))
  },rownames=TRUE)
  
  output$sorties2=renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
    )
    return(as.data.frame(values_facto()$res.CA$col$cos2))
  },rownames=TRUE)
  
  output$sorties3=renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
    )
    return(as.data.frame(values_facto()$res.CA$col$contrib))
  },rownames=TRUE)
  
  output$sorties4=renderTable({
    return(as.data.frame(values_facto()$res.CA$row$coord))
  },rownames=TRUE)
  
  output$sorties5=renderTable({
    return(as.data.frame(values_facto()$res.CA$row$cos2))
  },rownames=TRUE)
  
  output$sorties6=renderTable({
    return(as.data.frame(values_facto()$res.CA$row$contrib))
  },rownames=TRUE)
  
  output$sorties7=renderTable({
    validate(
      need((length(input$rowsupl)>0), gettext("No supplementary rows selected"))
    )
    return(as.data.frame(values_facto()$res.CA$row.sup$coord))
  },rownames=TRUE)
  
  output$sorties8=renderTable({
    return(as.data.frame(values_facto()$res.CA$row.sup$cos2))
  },rownames=TRUE)
  
  output$sorties9=renderTable({
    return(as.data.frame(values_facto()$res.CA$col.sup$coord))
  },rownames=TRUE)
  
  output$sorties10=renderTable({
    return(as.data.frame(values_facto()$res.CA$col.sup$cos2))
  },rownames=TRUE)
  
  output$sorties11=renderTable({
    validate(
      need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
    )
    return(as.data.frame(values_facto()$res.CA$quali.sup))
  },rownames=TRUE)
  
  
  
  output$map3=renderPlot({
    return(barplot(values_facto()$res.CA$eig[,1],names.arg=rownames(values_facto()$res.CA$eig),las=2))
  })
  
  ### Fonction permettant l'affichage du JDD sous la forme d'un DataTable, qui permet la recherche de donnes. 
  output$JDD=renderDataTable({
    cbind(Names=rownames(newdata),newdata)},
    options = list(    "orderClasses" = TRUE,
                       "responsive" = TRUE,
                       "pageLength" = 10))
  
  ### Fonction permettant l'affichage du summary du JDD
  output$summary=renderPrint({
    summary(newdata)
  })
  
  
  ### Fonction permettant l'affichage du summary de la fonction CA sur le JDD
  output$summaryCA=renderPrint({
    a<-values_facto()$res.CA
    a$call$call<-Code()
    summary.CA(a,nbelements=input$nbele)
  })
  
  output$summary2=downloadHandler(filename = function() { 
    paste('summaryofCA','.txt', sep='') 
  },
  content = function(file) {
    summary.CA(values_facto()$res.CA,nbelements=input$nbele,file=file)
  },
  contentType='text/csv')
  
  
  ## Creation des fonctions permettant l'enregistrement des graphs sous les formats : png, jpeg, pdf et emf
  output$downloadData = downloadHandler(
    filename = function() { 
      paste('graph1','.png', sep='') 
    },
    content = function(file) {
      png(file)
      Plot11()
      dev.off()
    },
    contentType='image/png')
  
  output$downloadData1 = downloadHandler(
    filename = function() { 
      paste('graph1','.jpg', sep='') 
    },
    content = function(file) {
      jpeg(file)
      Plot11()
      dev.off()
    },
    contentType='image/jpg')
  
  output$downloadData2 = downloadHandler(
    filename = function() { 
      paste('graph1','.pdf', sep='') 
    },
    content = function(file) {
      pdf(file)
      Plot11()
      dev.off()
    },
    contentType=NA)
  
  Plot11=function(){
    if(length(input$invis)==0){
      invisi="none"
    }
    if(length(input$invis)!=0){
      invisi=NULL
      if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
      if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
      if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
      if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
      if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
    }
    sel=NULL
    if(input$seleccol=="cos2"){
      if(input$slider3!=1){
        sel=paste("cos2 ",input$slider3)
      }
      else{
        sel="cos2 0.999"
      }
    }
    if(input$seleccol=="contrib"){
      sel=paste("contrib ",input$contrib1)
    }
    sel2=NULL
    if(input$selecrow=="cos2"){
      if(input$slider4!=1){
        sel2=paste("cos2 ",input$slider4)
      }
      else{
        sel2="cos2 0.999"
      }
    }
    if(input$seleccol=="contrib"){
      sel2=paste("contrib ",input$contrib2)
    }
    if(is.null(input$colrowsup)){
      colrowsup="darkblue"
    }else{
      colrowsup=input$colrowsup
    }
    if(is.null(input$colcolsup)){
      colcolsup="darkred"
    }else{
      colcolsup=input$colcolsup
    }
    plot.CA(values_facto()$res.CA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,title=input$title1,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)
  }
  
  
  if(input$selecrow=="cos2"){
    if(input$slider4!=1){
      sel2=paste("cos2 ",input$slider4)
    }
    else{
      sel2="cos2 0.999"
    }
  }
  if(input$seleccol=="contrib"){
    sel2=paste("contrib ",input$contrib2)
  }
  if(is.null(input$colrowsup)){
    colrowsup="darkblue"
  }else{
    colrowsup=input$colrowsup
  }
  if(is.null(input$colcolsup)){
    colcolsup="darkred"
  }else{
    colcolsup=input$colcolsup
  }
  plot.CA(values_facto()$res.CA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,title=input$title1,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)
}

