#' match_language_udpipe
#' @param lang_abbr
#' 
#' @return lang (selected language)
#' 
#' @export
#' @example
match_language_udpipe<-function(lang_abbr){
  if(lang_abbr=="de"){
    lang<-"german"
  }
  if(lang_abbr=="en"){
    lang<-"english"
  }
  if(lang_abbr=="es"){
    lang<-"spanish"
  }
  if(lang_abbr=="fr"){
    lang<-"french"
  }
  if(lang_abbr=="it"){
    lang<-"italian"
  }
  if(lang_abbr=="nl"){
    lang<-"dutch"
  }
  if(lang_abbr=="pt"){
    lang<-"portugese"
  }
  if(lang_abbr=="el"){
    lang<-"greek"
  }
return(lang)
}