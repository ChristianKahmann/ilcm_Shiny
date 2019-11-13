


#reload avaiable collections
observe({
  input$collections_Reset
  values$num_collections <-
    length(list.files("collections/collections/"))
})


output$collections <- DT::renderDataTable({
  validate(
    need(values$num_collections > 0, "No collections found")
  )
  collection_names<-data.frame(name=stringr::str_replace_all(
    string =  list.files("collections/collections/"),
    pattern = ".RData",
    replacement = ""
  ),stringsAsFactors = F)
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  queries=RMariaDB::dbGetQuery(mydb,paste0("SELECT name,query FROM ilcm.Collections where name in ('",paste0(collection_names$name,collapse="', '"),"');"))
  RMariaDB::dbDisconnect(mydb)
  #queries<-queries[order(queries[,1]),2]
  #collection_names<-sort(collection_names)
  values$collection_names<-collection_names$name

  queries<-plyr::join(x = collection_names,y = queries,by="name")
  queries[which(is.na(queries[,2])),2]<-"unknown"
  too_long_to_display<-which(nchar(queries[,2])>249)
  if(length(too_long_to_display)>0){
    queries[too_long_to_display,2]<-paste0(substr(x = queries[too_long_to_display,2],start = 1,stop = 250),"...")
  }
  queries[,1]<-unlist(lapply(1:dim(queries)[1],FUN = function(x){
    return(as.character(tags$span(id=paste0("collection_name_",x),tags$b(queries[x,1]))))
  }))

  
  
  df <- reactiveValues(
    data = data.frame(
      Collections = queries[,1],
      Queries =queries[,2],
      Delete = shinyInput(
        shinyBS::bsButton,
        length(list.files("collections/collections/")),
        'delete_button_',
        label = "",
        style="danger",
        icon=icon("trash"),
        onclick = 'Shiny.onInputChange(\"delete_button\",  this.id)'
      ),
      See = shinyInput(
        shinyBS::bsButton,
        length(list.files("collections/collections/")),
        'see_button_',
        label = "",
        style="success",
        icon=icon("eye"),
        onclick = 'Shiny.onInputChange(\"see_button\",  this.id)'
      ),
      stringsAsFactors = FALSE,
      row.names = 1:length(list.files("collections/collections/"))
    )
  )
  DT::datatable(
    df$data,
    selection = 'single',
    escape = FALSE,
    options = list(dom = 'tp',ordering=F,columnDefs=list(list(className="no_select",targets=c(1,3)),
                                                         list(visible=FALSE,targets=1)),
                   rowCallback=JS(
                     "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                     "var full_text = aData[1]",
                     "$('td:eq(0)', nRow).attr('title', full_text);",
                     "}"
                   )
    ),
    class = "compact",
    rownames = F,
    callback = JS('table.on("click", "td.no_select", function(e) {
                  e.stopPropagation()
});')
  )
},server = F)

proxy_collections = DT::dataTableProxy('collections')


#add an reactive value saving the current collection selected
observe({
  row<-input$collections_rows_selected
  if(is.null(row)){
    values$collection_selected<-NULL
  }
  else{
    values$collection_selected<-isolate(values$collection_names)[row]
  }
})




#if button is clicked delete collection
observeEvent(input$delete_button, {
  selectedRow <-
    as.numeric(strsplit(input$delete_button, "_")[[1]][3])
  if(selectedRow>0){
    shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete_coll",type = "warning",title = "Are you sure you want to delete this collection",danger_mode = T)
  }
})

observeEvent(input$confirm_delete_coll,{
  if(isTRUE(input$confirm_delete_coll)){
    name<-list.files("collections/collections/", full.names = F)[ as.numeric(strsplit(input$delete_button, "_")[[1]][3])]
    name<-stringr::str_replace(string = name,pattern = ".RData","")
    #delete colelction from database
    delete_collection_from_db(name)
    #delete collection from solr
    host<-values$update_solr_url
    port<-values$update_solr_port
    try({future::future(expr = {
      load(list.files("collections/collections/", full.names = T)[ as.numeric(strsplit(input$delete_button, "_")[[1]][3])])
      body<-create_body_solr_update_remove(ids = info[[3]][,1],field_name = "collections",values = rep(name,length(info[[3]][,1])))
      conn<-solrium::SolrClient$new(host = host,port = port,path="search")
      try(silent = T,{
        rm(solr_update_working)
        conn$update_atomic_json(name = "iLCM",body = body)->solr_update_working
      })
      if(!exists("solr_update_working")){
        conn$update_atomic_json(name = "iLCM",body = body)
      }
      solrium::commit(conn = conn,name="iLCM")
    }) %...>% future:::ClusterRegistry("stop")
    })
    #delete collection from disk
    file.remove(list.files("collections/collections/", full.names = T)[ as.numeric(strsplit(input$delete_button, "_")[[1]][3])])
    values$num_collections <-
      length(list.files("collections/collections/"))
  }
  shinyjs::useShinyjs()
  isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_button\",  "delete_button_0")'))
}
)

#if see button is clicked, show time series + other metadata for selected collection
observeEvent(input$see_button, {
  selectedRow <- as.numeric(strsplit(input$see_button, "_")[[1]][3])
  if(selectedRow>0){
    load(list.files("collections/collections/", full.names = T)[[selectedRow]])
    values$collection_time_series <- info[[6]]
    values$collection_time_series_name <- info[[5]]
    showModal(
      modalDialog(
        size = "l",
        title = paste0("Information for Collection: ", info[[5]]),
        selectInput(
          inputId = "Collection_TS_timeintervall",
          choices = c("day", "month", "year"),
          multiple = F,
          selected = "day",
          label = NULL,
          width = "30%"
        ),
        plotlyOutput(outputId = "Collection_TS"),
        tags$h5(paste0(
          "Size of Collection: ", dim(info[[6]])[1], " Documents"
        )),
        column(
          3,
          downloadButton(outputId = "Collection_TS_download_dates", label = "csv")
        ),
        column(3#,
               # downloadButton(outputId = "TS_download_memory",label = "csv"))
        )
      )
    )
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"see_button\",  "see_button_0")'))
  }
})

#render time series plot for selected collection
output$Collection_TS <- renderPlotly({
  validate(need(
    !is.null(values$collection_time_series),
    "no date information to plot"
  ))
  dates <- values$collection_time_series
  missing_dates <-
    seq.Date(from = min(as.Date(dates[, 1])),
             to = max(as.Date(dates[, 1])),
             by = "day")
  if (input$Collection_TS_timeintervall == "day") {
    dates[, 1] <- substr(dates[, 1], 1, 10)
    missing_dates <- substr(as.character(missing_dates), 1, 10)
  }
  if (input$Collection_TS_timeintervall == "month") {
    dates[, 1] <- substr(dates[, 1], 1, 7)
    missing_dates <- substr(as.character(missing_dates), 1, 7)
  }
  if (input$Collection_TS_timeintervall == "year") {
    dates[, 1] <- substr(dates[, 1], 1, 4)
    missing_dates <- substr(as.character(missing_dates), 1, 4)
  }
  missing_dates <- missing_dates[-which(missing_dates %in% dates[, 1])]
  missing_dates <- as.matrix(table(missing_dates))
  missing_dates[, 1] <- 0
  missing_dates <- cbind(rownames(missing_dates), missing_dates[, 1])
  dates <- as.matrix(table(dates))
  dates <- cbind(rownames(dates), dates[, 1])
  dates <- rbind(dates, missing_dates)
  #order data by date
  dates <- dates[order(dates[, 1]), ]
  dates <- matrix(dates, ncol = 2)
  isolate(values$Collection_TS_dates <- matrix(dates, ncol = 2))
  isolate(colnames(values$Collection_TS_dates) <- c("date", "counts"))
  p <-plot_ly(
    x = dates[, 1],
    y = as.numeric(dates[, 2]),
    type = "scatter",
    mode = "lines+markers"
  )
  p <-layout(
    p,
    paper_bgcolor = 'rgb(255,255,255)',
    legend = list(
      orientation = "h",
      yanchor = "bottom",
      xanchor = "center",
      x = 0.5,
      y = 1
    ),
    plot_bgcolor = 'rgb(229,229,229)',
    xaxis = list(
      autotick = T,
      showgrid = T,
      showgrid = TRUE,
      showline = FALSE,
      showticklabels = TRUE,
      tickcolor = 'rgb(127,127,127)',
      ticks = 'outside',
      zeroline = FALSE,
      side = "bottom"
    )
    ,
    margin = list(b = 100),
    yaxis = list(
      rangemode = "tozero",
      title = "Frequency",
      type = "linear",
      showgrid = T,
      showgrid = TRUE,
      showline = FALSE,
      showticklabels = TRUE,
      tickcolor = 'rgb(127,127,127)',
      ticks = 'outside',
      zeroline = T
    )
  )
  return(p)
})

#render download button for Collection time series andstart download when clicked
output$Collection_TS_download_dates <- downloadHandler(
  filename = function() {
    paste0(isolate(values$collection_time_series_name), ".csv")
  },
  content = function(con) {
    write.csv(isolate(values$Collection_TS_dates), con, row.names = FALSE)
  }
)



