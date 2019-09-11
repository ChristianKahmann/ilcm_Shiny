output$output_ace <- renderPrint({
  if(input$eval){
    values$newscheme<-runif(1)
    return(isolate(eval(parse(text=input$code))))
  }
  else{
    return(NULL)
  }
})  
