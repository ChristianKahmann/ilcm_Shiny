

output$datasets_avaiable<-renderUI({
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  RMariaDB::dbBegin(conn = mydb)
  RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  RMariaDB::dbSendStatement(mydb, 'SET SQL_SAFE_UPDATES = 0;')
  datasets=RMariaDB::dbGetQuery(mydb,"SELECT DISTINCT dataset FROM ilcm.metadata_names;")
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
  
  values$update_datasets_avaiable  
  
  tags$div(shinysky::select2Input(inputId = "dataset",label = tags$p("which corpus?",style="color:white;"),choices = datasets[,1],width = "100%",multiple=T,selected=datasets[1,1],
                                  drag.and.drop = F,type = "select"),id="select_sidebar")
})


output$dropdown_info<-renderMenu({
  validate(
    need(!is.null(values$user),message=FALSE)
  )
  return(dropdownMenu(type = "notifications",badgeStatus = NULL,icon=icon("info"),
                      notificationItem(
                        text = paste("You are logged in as: ",values$user),
                        icon=icon("user")
                      ),
                      notificationItem(
                        text =  paste("Version:",version),
                        icon = icon("tachometer")
                      )
  )
  )
})