#' get_quantile_belonging
#' @param all_data
#' @param sample
#' 
#' @return sample
#' 
#' @export
#' @example 
get_quantile_belonging<-function(all_data,sample){
  quantiles<-quantile(as.numeric(all_data),na.rm = T)
  sample<-as.numeric(sample)
  ids_quant1<-intersect(which(sample>=quantiles[1]),which(sample<=quantiles[2]))
  ids_quant2<-intersect(which(sample>quantiles[2]),which(sample<=quantiles[3]))
  ids_quant3<-intersect(which(sample>quantiles[3]),which(sample<=quantiles[4]))
  ids_quant4<-intersect(which(sample>quantiles[4]),which(sample<=quantiles[5]))
  sample[ids_quant1]<-"[0, 0.25]"
  sample[ids_quant2]<-"[0.25, 0.5]"
  sample[ids_quant3]<-"[0.5, 0.75]"
  sample[ids_quant4]<-"[0.75, 1]"
  return(sample)
}