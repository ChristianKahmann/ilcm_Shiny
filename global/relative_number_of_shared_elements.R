relative_number_of_shared_elements<-function(a,b){
  shared<-intersect(a,b)
  return(length(shared)/length(a))
}