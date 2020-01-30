check_pruning_parameters<-function(min_t_c,max_t_c,min_t_p,max_t_p,min_t_r,max_t_r,min_t_q,max_t_q,min_d_c,max_d_c,min_d_p,max_d_p,min_d_r,max_d_r,min_d_q,max_d_q){
  valid=T
  if(!is.na(min_t_c) && !is.na(max_t_c)){
    if (min_t_c>max_t_c) {
      valid=F
      return(valid)
    }
  }
  if(!is.na(min_d_c) && !is.na(max_d_c)){
    if (min_d_c>max_d_c) {
      valid=F
      return(valid)
    }     
  }
  if(!is.na(min_t_p) && !is.na(max_t_p)){
    if (min_t_p>max_t_p) {
      valid=F
      return(valid)
    }
  }
  if(!is.na(min_d_p) && !is.na(max_d_p)){
    if (min_d_p>max_d_p) {
      valid=F
      return(valid)
    } 
  }
  if(!is.na(min_t_r) && !is.na(max_t_r)){
    if (min_t_r<max_t_r) {
      valid=F
      return(valid)
    }
  }
  if(!is.na(min_d_r) && !is.na(max_d_r)){
    if (min_d_r<max_d_r) {
      valid=F
      return(valid)
    } 
  }
  if(!is.na(min_t_q) && !is.na(max_t_q)){
    if (min_t_q<max_t_q) {
      valid=F
      return(valid)
    }
  }
  if(!is.na(min_d_q) && !is.na(max_d_q)){
    if (min_d_q<max_d_q) {
      valid=F
      return(valid)
    } 
  }
  if(!is.na(min_t_p)){
    if(min_t_p>1){
      valid=F
    }
  }
  if(!is.na(max_t_p)){
    if(max_t_p>1){
      valid=F
    }
  }
  if(!is.na(min_d_p)){
    if(min_d_p>1){
      valid=F
    }
  }
  if(!is.na(max_d_p)){
    if(max_d_p>1){
      valid=F
    }
  }
     
  return(valid)
}