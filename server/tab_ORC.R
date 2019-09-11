output$frame <- renderUI({
  my_test <- tags$iframe(src="https://notebooks.gesis.org/",width=1600,height=700)
  print(my_test)
  my_test
})


########################

