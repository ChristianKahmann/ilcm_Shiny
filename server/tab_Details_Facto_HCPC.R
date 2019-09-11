observe({
  validate(
    need(!is.null(values$Details_Data_FA),message=FALSE)
  )
  load(paste0(values$Details_Data_FA,"/dtm.RData"))
  load(paste0(values$Details_Data_FA,"/info.RData"))
  load(paste0(values$Details_Data_FA,"/meta.RData"))
  
  values$facto_meta<-meta
  values$facto_dtm<-dtm
  values$facto_info<-info
})


output$facto_meta_hcpc_ui<-renderUI({
  values$Details_Data_FA
  return(
    selectInput(inputId = "facto_meta_hcpc",label = "Choose meta data field",choices = colnames(values$facto_meta))
  )
})

observe({
  validate(
    need(!is.null(input$facto_meta_hcpc),message=F)
  )
  x<-as.data.frame((as.matrix(values$facto_dtm)))
  x<-cbind(isolate(values$facto_meta[,input$facto_meta_hcpc]),x)
  colnames(x)[1]<-"X1"
  attach(x)
  x<-as.data.frame(aggregate(x[,2:dim(x)[2]],by=list(X1),FUN = sum))
  colnames(x)[1]<-input$facto_meta_hcpc
  rownames(x)<-x[,1]
  #x<-children
  if(is.data.frame(x)==TRUE){
    quanti_HCPC=c()
    quali_HCPC=c()
    posi=c()
    for ( i in 1:dim(x)[2]){
      if (is.numeric(x[,i])==TRUE){
        quanti_HCPC=c(quanti_HCPC,colnames(x)[i])
      }
      else{
        quali_HCPC=c(quali_HCPC,colnames(x)[i])
      }
    }
    VariableChoices_HCPC<-quanti_HCPC
    assign("nom",rownames(x),envir=.GlobalEnv)
    assign("nbindiv",length(nom),envir=.GlobalEnv)
    assign("num",c(1:length(nom)),envir=.GlobalEnv)
    assign("Qualichoice_HCPC",quali_HCPC,envir=.GlobalEnv)
    assign("IdChoices",c(1:length(VariableChoices_HCPC)),envir=.GlobalEnv)
    assign("Idqualisup",c(1:length(Qualichoice_HCPC)),envir=.GlobalEnv)
    assign("consolidf",FALSE,envir=.GlobalEnv)
    assign("metricdf",gettext("Euclidean"),envir=.GlobalEnv)
    assign("drawdf",FALSE,envir=.GlobalEnv)
    assign("df",FALSE,envir=.GlobalEnv)
    assign("centerdf",FALSE,envir=.GlobalEnv)
    assign("numdf",60,envir=.GlobalEnv)
    assign("nb1df",1,envir=.GlobalEnv)
    assign("nb2df",2,envir=.GlobalEnv)
    
    
    assign("nomData",paste0("Collection ",isolate(values$facto_info[[5]])),envir=.GlobalEnv)
    assign("newdata_HCPC",x,envir=.GlobalEnv)
    
    assign("title1",gettext("Hierarchical tree on the factor map"),envir=.GlobalEnv)
    assign("title2",gettext("Factor map"),envir=.GlobalEnv)
    assign("title3",gettext("Hierarchical tree"),envir=.GlobalEnv)
    values$Facto_ready_HCPC<-TRUE
    values$Facto_new_Hcpc<-runif(n = 1,min = 0,max = 1) 
  }
})





output$Details_Facto_HCPC_UI<-renderUI({
  validate(
    need(values$Facto_ready_HCPC==TRUE,message=FALSE)
  )
  values$Facto_new_Hcpc
  all=colnames(newdata_HCPC)
  quanti_HCPC=names(which(sapply(newdata_HCPC,is.numeric)))
  quali_HCPC=names(which(!(sapply(newdata_HCPC,is.numeric))))
  VariableChoices_HCPC=quanti_HCPC
  nom=rownames(newdata_HCPC)
  num=c(1:length(nom))
  Qualichoice_HCPC=quali_HCPC
  IdChoices=c(1:length(VariableChoices_HCPC))
  Idqualisup=c(1:length(Qualichoice_HCPC))
  Idall=c(1:length(all))
  nomData=unlist(strsplit(as.character(nomData),"\\["))[1]
  assign("quanti_HCPC",quanti_HCPC,envir=.GlobalEnv)
  assign("quali_HCPC",quali_HCPC,envir=.GlobalEnv)
  assign("VariableChoices_HCPC",VariableChoices_HCPC,envir = .GlobalEnv)
  
  values$newdata_HCPC<-newdata_HCPC
  return(tagList(
    div(paste(gettext("HCPC on the "),nomData),style="color:#2A0A29"),
    sidebarLayout(
      sidebarPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
            tags$head(
              tags$style("body {background-color: #D2FAE5; }"),
              tags$style(type='text/css', "#title1 { height: 25px; }"),
              tags$style(type='text/css', "#title2 { height: 25px; }"),
              tags$style(type='text/css', "#title3 { height: 25px; }")
            ),
            wellPanel(
              div(align="center",checkboxInput("hcpcparam",gettext("Show HCPC parameters"),FALSE)),
              conditionalPanel(
                condition="input.hcpcparam==true",
                uiOutput("clusters"),
                hr(),
                checkboxInput("consoli","Consolidation",consolidf),
                hr(),
                radioButtons("metric",gettext("Which metric would you like to use?"),choices=list(gettext("Euclidean"),"Manhattan"),inline=TRUE,select=metricdf)
              )),
            wellPanel(
              div(align="center",checkboxInput("graph_HCPC",gettext("Show graphs options"),FALSE)),
              conditionalPanel(
                condition="input.graph_HCPC==true",
                fluidRow(
                  column(5,selectInput("nb1_HCPC", label = h6(gettext("x axis")), 
                                       choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = nb1df,width='80%')),
                  column(5,selectInput("nb2_HCPC", label =h6(gettext("y axis")), 
                                       choices = list("1" = 1, "2" = 2,"3" = 3,"4"= 4,"5" =5), selected = nb2df,width='80%'))),
                hr(),
                radioButtons("HCPCgraph",h6(gettext("Which graph do you want to modify?")),
                             choices=list(gettext("Hierarchical tree"),gettext("Factorial map_HCPC"),gettext("3D plot")),inline=TRUE),
                hr(),
                conditionalPanel(
                  #        condition="input.HCPCgraph=='3D'",
                  condition=paste("input.HCPCgraph=='",gettext("3D plot"),"'",sep=''),
                  textInput("title1_HCPC",h6(gettext("Title of the graph: ")), title1),
                  checkboxInput("nom3D",gettext("Names on 3D plot"),df),
                  checkboxInput("center",gettext("Draw centers of clusters"),centerdf),
                  hr(),
                  sliderInput("num_HCPC",gettext("Angle (in degrees)"),value=numdf,min=0,max=360,step=1)
                ),
                conditionalPanel(
                  #        condition="input.HCPCgraph=='ind'",
                  condition=paste("input.HCPCgraph=='",gettext("Factorial map_HCPC"),"'",sep=''),
                  textInput("title2_HCPC",h6(gettext("Title of the graph: ")), title2),
                  checkboxInput("drawtree",gettext("Draw tree"),drawdf)
                ),
                conditionalPanel(
                  #        condition="input.HCPCgraph=='tree'",
                  condition=paste("input.HCPCgraph=='",gettext("Hierarchical tree"),"'",sep=''),
                  textInput("title3_HCPC",h6(gettext("Title of the graph: ")), title3))
              )),
            wellPanel(
              h5(gettext("Save graphs as:"),align="center"),
              radioButtons("paramdown_HCPC","",
                           choices=list("PNG"="png","JPG"="jpg","PDF"="pdf"),selected="png")
            )
        )
        ,width=3),
      mainPanel(
        div(style = 'height: 60vh; overflow-y: auto;',
            tags$style(type = "text/css", "a{color: #0B6121;}"),
            tabsetPanel(id = "graph_sort_HCPC",
                        tabPanel(gettext("Graphs"),
                                 fluidRow(
                                   br(),
                                   column(width = 6,plotOutput("map_HCPC4", width = "500", height="500"),
                                          #                             div(align="center",plotOutput("map_HCPC4_HCPC",width = 650, height=500)),
                                          br(),
                                          conditionalPanel(
                                            condition="input.paramdown_HCPC=='jpg'",
                                            p(downloadButton("downloadData6_HCPC",gettext("Download as jpg")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_HCPC=='png'",
                                            p(downloadButton("downloadData7_HCPC",gettext("Download as png")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_HCPC=='pdf'",
                                            p(downloadButton("downloadData8_HCPC",gettext("Download as pdf")),align="center")),
                                          br(),
                                          align="center"),
                                   column(width = 6,plotOutput("map_HCPC", width = "500", height="500"),
                                          #                             div(align="center",plotOutput("map_HCPC",width = 500, height=500)),
                                          br(),
                                          conditionalPanel(
                                            condition="input.paramdown_HCPC=='jpg'",
                                            p(downloadButton("downloadData1_HCPC",gettext("Download as jpg")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_HCPC=='png'",
                                            p(downloadButton("downloadData_HCPC",gettext("Download as png")),align="center")),
                                          conditionalPanel(
                                            condition="input.paramdown_HCPC=='pdf'",
                                            p(downloadButton("downloadData2_HCPC",gettext("Download as pdf")),align="center")),
                                          br(),
                                          align="center")),
                                 div(align="center",plotOutput("map2_HCPC",width=750,height=500)),
                                 br(),
                                 conditionalPanel(
                                   condition="input.paramdown_HCPC=='jpg'",
                                   p(downloadButton("downloadData4_HCPC",gettext("Download as jpg")),align="center")),
                                 conditionalPanel(
                                   condition="input.paramdown_HCPC=='png'",
                                   p(downloadButton("downloadData3_HCPC",gettext("Download as png")),align="center")),
                                 conditionalPanel(
                                   condition="input.paramdown_HCPC=='pdf'",
                                   p(downloadButton("downloadData5_HCPC",gettext("Download as pdf")),align="center")),
                                 br()
                        ),
                        
                        tabPanel(gettext("Values"),
                                 br(),
                                 radioButtons("out_HCPC",gettext("Which outputs do you want?"),
                                              choices=list(gettext("Description of classes by variables"),gettext("Description of classes by axes"),gettext("Parangons")),selected=gettext("Description of classes by variables"),inline=TRUE),
                                 conditionalPanel(
                                   #                               condition="input.out_HCPC=='var'",
                                   condition=paste("input.out_HCPC=='",gettext("Description of classes by variables"),"'",sep=''),
                                   div(align="center",tableOutput("descript_HCPC"))
                                 ),
                                 conditionalPanel(
                                   #                               condition="input.out_HCPC=='para'",
                                   condition=paste("input.out_HCPC=='",gettext("Parangons"),"'",sep=''),
                                   div(align="center",tableOutput("parangons_HCPC"))),
                                 conditionalPanel(
                                   #                               condition="input.out_HCPC=='axe'",
                                   condition=paste("input.out_HCPC=='",gettext("Description of classes by axes"),"'",sep=''),
                                   div(align="center",tableOutput("axes_HCPC")))
                        ),
                        
                        tabPanel(gettext("Data"),
                                 dataTableOutput("JDD_HCPC")
                        )
            )
        )
        ,width=9)
    )
  ))
})




values_HCPC=reactive({
  data.selec=values$newdata_HCPC[,VariableChoices_HCPC]
  choixquali=NULL
  if(length(quali_HCPC)!=0){
    data.selec=cbind(data.selec,values$newdata_HCPC[,Qualichoice_HCPC])
    choixquali=seq((length(data.selec)-length(quali_HCPC)),length(data.selec))
  }
  
  list(res.PCA=(PCA(data.selec,quali.sup=choixquali,scale.unit=FALSE,graph=FALSE,ncp=Inf)),DATA=(data.selec))
})

res.HCPCdef=reactive({
  (HCPC(values_HCPC()$res.PCA,nb.clust=-1,graph=FALSE)$call$t$nb.clust)
})

# res.HCPC=reactive({
# (HCPC(values_HCPC()$res.PCA,nb.clust=input$clust,consol=input$consoli,graph=FALSE,metric=input$metric))
# })
res.HCPC=reactive({
  (if (input$metric=="Manhattan") HCPC(values_HCPC()$res.PCA,nb.clust=input$clust,consol=input$consoli,graph=FALSE,metric="manhattan")
   else HCPC(values_HCPC()$res.PCA,nb.clust=input$clust,consol=input$consoli,graph=FALSE,metric="euclidean"))
})





Plot1Code <- function(){
  Call2=paste("plot.HCPC(res.HCPC,choice='map_HCPC',draw.tree=",input$drawtree,",title='",input$title2_HCPC,"',axes=c(",as.numeric(input$nb1_HCPC),",",as.numeric(input$nb2),"))",sep="") 
  return(Call2)
}

Plot2Code <- function(){
  Call3=paste("plot.HCPC(res.HCPC,choice='3D.map_HCPC',ind.names=",input$nom3D,",centers.plot=",input$center,",title='",input$title,"',angle=",input$num_HCPC,",axes=c(",as.numeric(input$nb1_HCPC),",",as.numeric(input$nb2_HCPC),"))",sep="") 
  return(Call3)
}

Plot3Code <- function(){
  Call4=paste("plot.HCPC(res.HCPC,choice='tree',title='",input$title3_HCPC,"')",sep="")
  return(Call4)
}

getactive_HCPC=function(){
  if(input$quantisup==TRUE){
    sup=NULL
    if(length(input$supvar)==0){
      activevar=VariableChoices_HCPC
    }
    else{
      # for (i in 1:length(VariableChoices_HCPC)){
      # if(VariableChoices_HCPC[i]%in%input$supvar){
      # sup=c(sup,i)
      # }
      # }
      sup=which(VariableChoices_HCPC%in%input$supvar)
      activevar=VariableChoices_HCPC[-sup]
    }
    return(activevar)
  }
}

Plot1_HCPC <- function(){
  if(is.null(input$clust)){
    return()
  }
  else{
    return(plot.HCPC(res.HCPC(),choice="map",draw.tree=input$drawtree,title=input$title2_HCPC,axes=c(as.numeric(input$nb1_HCPC),as.numeric(input$nb2_HCPC))))
  }
}
output$map_HCPC <- renderPlot({
  p <- Plot1_HCPC()
})


Plot2_HCPC <- function(){
  if(is.null(input$clust)){
    return()
  }
  else{
    return(plot.HCPC(res.HCPC(),choice="3D.map",ind.names=input$nom3D,title=input$title1_HCPC,centers.plot=input$center,axes=c(as.numeric(input$nb1_HCPC),as.numeric(input$nb2_HCPC)),angle=input$num_HCPC))
  }
}

output$map2_HCPC <- renderPlot({
  p <- Plot2_HCPC()
})

Plot4_HCPC=function(){
  if(is.null(input$clust)){
    return()
  }
  else{
    return(plot.HCPC(res.HCPC(),choice="tree",title=input$title3_HCPC))
  }
}

output$map_HCPC4=renderPlot({
  p=Plot4_HCPC()
})

#output$sorties=renderTable({
#  if(input$out_HCPC=="axe"){
#    return(as.data.frame(res.HCPC()$desc.axes))
#  }
#  if(input$out_HCPC=="para"){
#    return(as.data.frame(res.HCPC()$ind.desc))
#  }
#},rownames=TRUE)


output$clusters=renderUI({
  choix=res.HCPCdef()
  if(is.data.frame(values$newdata_HCPC)==TRUE){
    if(nbindiv<=11){
      sliderInput("clust","Number of clusters",min=2,max=(nbindiv-1),value=choix,step=1)
    }
    else{
      sliderInput("clust","Number of clusters",min=2,max=10,value=choix,step=1)
    }
  }
  else{
    if(nbindiv<=11){
      sliderInput("clust","Number of clusters",min=2,max=(nbindiv-1),value=clustdf,step=1)
    }
    else{
      sliderInput("clust","Number of clusters",min=2,max=10,value=clustdf,step=1)
    }
  }
})

output$JDD_HCPC=renderDataTable({
  cbind(Names=rownames(values$newdata_HCPC),values$newdata_HCPC)},
  options = list(    "orderClasses" = TRUE,
                     "responsive" = TRUE,
                     "pageLength" = 10))


output$downloadData_HCPC = downloadHandler(
  filename = function() { 
    paste('graph1','.png', sep='') 
  },
  content = function(file) {
    png(file)
    Plot1_HCPC()
    dev.off()
  },
  contentType='image/png')

output$downloadData1_HCPC = downloadHandler(
  filename = function() { 
    paste('graph1','.jpg', sep='') 
  },
  content = function(file) {
    jpeg(file)
    Plot1_HCPC()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData2_HCPC = downloadHandler(
  filename = function() { 
    paste('graph1','.pdf', sep='') 
  },
  content = function(file) {
    pdf(file)
    Plot1_HCPC()
    dev.off()
  },
  contentType=NA)

output$downloadData3_HCPC = downloadHandler(
  filename = function() { 
    paste('graph2','.png', sep='') 
  },
  content = function(file) {
    png(file)
    Plot2_HCPC()
    dev.off()
  },
  contentType='image/png')

output$downloadData4_HCPC = downloadHandler(
  filename = function() { 
    paste('graph2','.jpg', sep='') 
  },
  content = function(file) {
    jpeg(file)
    Plot2_HCPC()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData5_HCPC = downloadHandler(
  filename = function() { 
    paste('graph2','.pdf', sep='') 
  },
  content = function(file) {
    pdf(file)
    Plot2_HCPC()
    dev.off()
  },
  contentType=NA)

output$downloadData6_HCPC = downloadHandler(
  filename = function() { 
    paste('graph3','.png', sep='') 
  },
  content = function(file) {
    png(file)
    Plot4_HCPC()
    dev.off()
  },
  contentType='image/png')

output$downloadData7_HCPC = downloadHandler(
  filename = function() { 
    paste('graph3','.jpg', sep='') 
  },
  content = function(file) {
    jpeg(file)
    Plot4_HCPC()
    dev.off()
  },
  contentType='image/jpg')

output$downloadData8_HCPC = downloadHandler(
  filename = function() { 
    paste('graph3','.pdf', sep='') 
  },
  content = function(file) {
    pdf(file)
    Plot4_HCPC()
    dev.off()
  },
  contentType=NA)




### Fonction permettant d'afficher la description des classes par les variables
output$descript_HCPC=renderTable({ 
  write.infile(X=res.HCPC()$desc.var$quanti_HCPC,file=paste(getwd(),"essai.csv"),sep=";")
  baba=read.csv(paste(getwd(),"essai.csv"),sep=";",header=FALSE)
  colnames(baba)=NULL
  b=which(baba[,1]=="format non affichable")
  file.remove(paste(getwd(),"essai.csv")) 
  baba[,-ncol(baba)]
},
rownames=FALSE)

### Fonction permettant d'afficher les parangons des classes
output$parangons_HCPC=renderTable({
  browser()
  bibi=list()
  for (i in 1:input$clust){
    bibi[[i]]=rbind(colnames(res.HCPC()$desc.ind$para[[i]]),res.HCPC()$desc.ind$para[[i]])
    rownames(bibi[[i]])="Distance"
  }
  write.infile(X=bibi,file=paste(getwd(),"essai3.csv"),sep=";",nb.dec=8)
  baba=read.csv(paste(getwd(),"essai3.csv"),sep=";",header=FALSE)
  colnames(baba)=NULL
  file.remove(paste(getwd(),"essai3.csv"))
  baba[,-ncol(baba)] 
},
rownames=FALSE)

### Fonction permettant d'afficher la description des classes par les axes 
output$axes_HCPC=renderTable({
  write.infile(X=res.HCPC()$desc.axes$quanti_HCPC,file=paste(getwd(),"essai2.csv"),sep=";",nb.dec=8)
  baba=read.csv(paste(getwd(),"essai2.csv"),sep=";",header=FALSE)
  colnames(baba)=NULL
  file.remove(paste(getwd(),"essai2.csv"))
  baba[,-ncol(baba)]
},
rownames=FALSE)
