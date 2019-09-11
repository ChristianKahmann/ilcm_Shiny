rv <- reactiveValues(cachedTbl = NULL)

output$tbl <- renderHtable({

 
    if(as.integer(input$categories_number)==5){
    tbl <- matrix(c("Ort","Person","Firma","Adjektiv","Verb","red","blue","green","brown","orange"), nrow=as.integer(input$categories_number), ncol=2,byrow = F)
    }
    else{
      tbl <- matrix(c(character(0)), nrow=as.integer(input$categories_number), ncol=2,byrow = F) 
    }
    rv$cachedTbl <<- tbl
    return(tbl)

   
  
})  

output$tblNonEdit <- renderTable({
  
  #add dependence on button
  input$actionButtonID
  
  #isolate the cached table so it only responds when the button is pressed
  isolate({
    rv$cachedTbl
  })
})    