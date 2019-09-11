output$ace_script_iA <- renderPrint({
  
  if(input$eval_script_iA){
    result<-try({isolate(eval(parse(text=input$R_Script_iA)))})
    return(result)
  }
  else{
    return(NULL)
  }
})  