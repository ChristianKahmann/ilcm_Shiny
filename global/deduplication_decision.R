#' decide_which_document_to_delete
#' @param pair
#' @param strategy
#'  
#' @return id_del (id of element you wish to delete)
#' 
#' @export
#' @example 
decide_which_document_to_delete<-function(pair,strategy){
  if(strategy=="latest"){
    if(pair[,"date_a"]>pair[,"date_b"]){
      id_del=pair[,"b"]
    }
    else{
      id_del=pair[,"a"]
    }
  }
  if(strategy=="earliest"){
    if(pair[,"date_a"]<pair[,"date_b"]){
      id_del=pair[,"b"]
    }
    else{
      id_del=pair[,"a"]
    }
  }
  if(strategy=="longest"){
    if(pair[,"token_a"]>pair[,"token_b"]){
      id_del=pair[,"b"]
    }
    else{
      id_del=pair[,"a"]
    }
  }
  if(strategy=="shortest"){
    if(pair[,"token_a"]<pair[,"token_b"]){
      id_del=pair[,"b"]
    }
    else{
      id_del=pair[,"a"]
    }
  }
  if(strategy=="maximum node degree"){
    if(pair[,"Freq.x"]>pair[,"Freq.y"]){
      id_del=pair[,"a"]
    }
    else{
      id_del=pair[,"b"]
    }
  }
  if(strategy=="random"){
    id_del<-sample(x = c(pair[1,"a"],pair[1,"b"]),size = 1)
  }
  return(id_del)
}