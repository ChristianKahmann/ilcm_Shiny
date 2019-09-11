highlight<-function (q, hl.fl = NULL, hl.snippets = NULL, hl.fragsize = NULL, 
                     hl.q = NULL, hl.mergeContiguous = NULL, hl.requireFieldMatch = NULL, 
                     hl.maxAnalyzedChars = NULL, hl.alternateField = NULL, hl.maxAlternateFieldLength = NULL, 
                     hl.preserveMulti = NULL, hl.maxMultiValuedToExamine = NULL, 
                     hl.maxMultiValuedToMatch = NULL, hl.formatter = NULL, hl.simple.pre = NULL, 
                     hl.simple.post = NULL, hl.fragmenter = NULL, hl.fragListBuilder = NULL, 
                     hl.fragmentsBuilder = NULL, hl.boundaryScanner = NULL, hl.bs.maxScan = NULL, 
                     hl.bs.chars = NULL, hl.bs.type = NULL, hl.bs.language = NULL, 
                     hl.bs.country = NULL, hl.useFastVectorHighlighter = NULL, 
                     hl.usePhraseHighlighter = NULL, hl.highlightMultiTerm = NULL, 
                     hl.regex.slop = NULL, hl.regex.pattern = NULL, hl.regex.maxAnalyzedChars = NULL, 
                     start = 0, rows = NULL, wt = "json", raw = FALSE, key = NULL, 
                     base = NULL, callopts = list(), fl = "DOES_NOT_EXIST", fq = NULL, 
                     parsetype = "list", verbose = TRUE,sort=NULL,facet.field=NULL) 
{
  if (is.null(base)) {
    stop("You must provide a url, e.g., http://api.plos.org/search or http://localhost:8983/solr/select")
  }
  if (!is.null(hl.fl)) 
    names(hl.fl) <- rep("hl.fl", length(hl.fl))
  args <- plyr::compact(list(wt = wt, q = q, start = start, rows = rows, 
                       hl = "true", hl.snippets = hl.snippets, hl.fragsize = hl.fragsize, 
                       fl = fl, fq = fq, hl.mergeContiguous = hl.mergeContiguous, 
                       hl.requireFieldMatch = hl.requireFieldMatch, hl.maxAnalyzedChars = hl.maxAnalyzedChars, 
                       hl.alternateField = hl.alternateField, hl.maxAlternateFieldLength = hl.maxAlternateFieldLength, 
                       hl.preserveMulti = hl.preserveMulti, hl.maxMultiValuedToExamine = hl.maxMultiValuedToExamine, 
                       hl.maxMultiValuedToMatch = hl.maxMultiValuedToMatch, 
                       hl.formatter = hl.formatter, hl.simple.pre = hl.simple.pre, 
                       hl.simple.post = hl.simple.post, hl.fragmenter = hl.fragmenter, 
                       hl.fragListBuilder = hl.fragListBuilder, hl.fragmentsBuilder = hl.fragmentsBuilder, 
                       hl.boundaryScanner = hl.boundaryScanner, hl.bs.maxScan = hl.bs.maxScan, 
                       hl.bs.chars = hl.bs.chars, hl.bs.type = hl.bs.type, hl.bs.language = hl.bs.language, 
                       hl.bs.country = hl.bs.country, hl.useFastVectorHighlighter = hl.useFastVectorHighlighter, 
                       hl.usePhraseHighlighter = hl.usePhraseHighlighter, hl.highlightMultiTerm = hl.highlightMultiTerm, 
                       hl.regex.slop = hl.regex.slop, hl.regex.pattern = hl.regex.pattern, 
                       hl.regex.maxAnalyzedChars = hl.regex.maxAnalyzedChars,sort=sort))
  args <- c(args, hl.fl)
  tt <- httr::GET(base, query = args, callopts)
  if (verbose) 
    message(URLdecode(tt$url))
  httr::stop_for_status(tt)
  out <- httr::content(tt, as = "text")
  class(out) <- "sr_high"
  attr(out, "wt") <- wt
  if (raw) {
    return(out)
  }
  else {
    return(solr_parse(out, parsetype))
  }
}




facet_date<-function (q = "*:*", fq=NULL,facet.query = NA, facet.field = NA, facet.prefix = NA, 
          facet.sort = NA, facet.limit = NA, facet.offset = NA, facet.mincount = NA, 
          facet.missing = NA, facet.method = NA, facet.enum.cache.minDf = NA, 
          facet.threads = NA, facet.date = NA, facet.date.start = NA, 
          facet.date.end = NA, facet.date.gap = NA, facet.date.hardend = NA, 
          facet.date.other = NA, facet.date.include = NA, facet.range = NA, 
          facet.range.start = NA, facet.range.end = NA, facet.range.gap = NA, 
          facet.range.hardend = NA, facet.range.other = NA, facet.range.include = NA, 
           key = NA, base = NA, wt = "json", 
          raw = FALSE, callopts = list(),fl=NULL,facet=NULL, verbose = TRUE, ...) 
{
  if (is.na(base)) {
    stop("You must provide a url, e.g., http://api.plos.org/search or http://localhost:8983/solr/select")
  }

  args <- plyr::compact(list(wt = wt, q = q,facet.limit=facet.limit,facet.range.gap=facet.range.gap,  
                                                    fq=fq, facet.field=facet.field,facet="on",fl=fl))
                          
  tt <- httr::GET(base, query = args, callopts)
  if (verbose) 
    message(URLdecode(tt$url))
  httr::stop_for_status(tt)
  out <- httr::content(tt, as = "text")
  class(out) <- "sr_facet"
  attr(out, "wt") <- wt
  if (raw) {
    return(out)
  }
  else {
    solr::solr_parse(out)
  }
}

makemultiargs <- function(x){
  value <- get(x, envir = parent.frame(n = 2))
  if( length(value) == 0 ){ NULL } else {
    if( any(sapply(value, is.na)) ){ NULL } else {
      if( !is.character(value) ){ 
        value <- as.character(value)
      }
      names(value) <- rep(x, length(value))
      value
    }
  }
}


collectargs <- function(x){
  outlist <- list()
  for(i in seq_along(x)){
    outlist[[i]] <- makemultiargs(x[[i]])
  }
  as.list(unlist(compact(outlist)))
}


facet_meta<-function (q = "*:*",fq=NA, facet.query = NA, facet.field = NA, facet.prefix = NA, 
          facet.sort = NA, facet.limit = NA, facet.offset = NA, facet.mincount = NA, 
          facet.missing = NA, facet.method = NA, facet.enum.cache.minDf = NA, 
          facet.threads = NA, facet.date = NA, facet.date.start = NA, 
          facet.date.end = NA, facet.date.gap = NA, facet.date.hardend = NA, 
          facet.date.other = NA, facet.date.include = NA, facet.range = NA, 
          facet.range.start = NA, facet.range.end = NA, facet.range.gap = NA, 
          facet.range.hardend = NA, facet.range.other = NA, facet.range.include = NA, 
          start = NA, rows = NA, key = NA, base = NA, wt = "json", 
          raw = FALSE, callopts = list(), verbose = TRUE, ...) 
{
  if (is.na(base)) {
    stop("You must provide a url, e.g., http://api.plos.org/search or http://localhost:8983/solr/select")
  }
  todonames <- c("q", "fq","facet.query", "facet.field", "facet.prefix", 
                 "facet.sort", "facet.limit", "facet.offset", "facet.mincount", 
                 "facet.missing", "facet.method", "facet.enum.cache.minDf", 
                 "facet.threads", "facet.date", "facet.date.start", "facet.date.end", 
                 "facet.date.gap", "facet.date.hardend", "facet.date.other", 
                 "facet.date.include", "facet.range", "facet.range.start", 
                 "facet.range.end", "facet.range.gap", "facet.range.hardend", 
                 "facet.range.other", "facet.range.include", "start", 
                 "rows", "key", "wt")
  args <- collectargs(todonames)
  args$fl <- "DOES_NOT_EXIST"
  args$facet <- "true"
  args <- c(args, list(...))
  tt <- httr::GET(base, query = args, callopts)
  if (verbose) 
    message(URLdecode(tt$url))
  httr::stop_for_status(tt)
  out <- httr::content(tt, as = "text")
  class(out) <- "sr_facet"
  attr(out, "wt") <- wt
  if (raw) {
    return(out)
  }
  else {
    solr::solr_parse(out)
  }
}





solr_suggest<-function (q = "*:*", base=NULL,raw=FALSE,wt="json") 
{
  if (is.na(base)) {
    stop("You must provide a url, e.g., http://api.plos.org/search or http://localhost:8983/solr/select")
  }
  
  args <- plyr::compact(list(q=q,wt=wt))
  
  tt <- httr::GET(base, query = args)
 
 httr::stop_for_status(tt)
  out <- httr::content(tt, as = "text")
  class(out) <- "sr_facet"
  attr(out, "wt") <- wt
  if (raw) {
    return(out)
  }
  else {
    result<-do.call(c,unlist(jsonlite::fromJSON(out)$suggest$mySuggester,recursive=F))
    if(length(result)==1){
      return(q)
    }
    else{
    return(result[[2]])
    }
  }
}


solr_search_advanced<-function (q = "*:*", sort = NULL, start = 0, rows = NULL, pageDoc = NULL, 
                                pageScore = NULL, fq = NULL, fl = NULL, defType = NULL, timeAllowed = NULL, 
                                qt = NULL, wt = "json", NOW = NULL, TZ = NULL, echoHandler = NULL, 
                                echoParams = NULL, key = NULL, base = NULL, callopts = list(), 
                                raw = FALSE, parsetype = "df", concat = ",", ..., verbose = TRUE) 
{
  if (is.null(base)) {
    stop("You must provide a url, e.g., http://api.plos.org/search or http://localhost:8983/solr/select")
  }
  if (!is.null(fl)) 
    names(fl) <- rep("fl", length(fl))
  args <- plyr::compact(list(q = q, sort = sort, start = start, rows = rows, 
                       pageDoc = pageDoc, pageScore = pageScore, fq = fq, defType = defType, 
                       timeAllowed = timeAllowed, qt = qt, wt = wt, NOW = NOW, 
                       TZ = TZ, echoHandler = echoHandler, echoParams = echoParams))
  args <- c(args, fl)
  args <- c(args, list(...))
  tt <- httr::GET(base, query = args, callopts)
  if (verbose) 
    message(URLdecode(tt$url))
  httr::stop_for_status(tt)
  out <- httr::content(tt, as = "text")
  class(out) <- "sr_search"
  attr(out, "wt") <- wt
  if (raw) {
    return(list(out,URLdecode(tt$url)))
  }
  else {
    
    return(list(solr::solr_parse(out, parsetype, concat),URLdecode(tt$url)))
  }
}


solr_custom<-function(url,start=0){
  if(stringr::str_detect(string = url,pattern = "&start")==FALSE){
    url<-paste0(url,"&start=",start)
  }
  parsetype = "df"
  concat = ","
  tt <- httr::GET(url = url)
  httr::stop_for_status(tt)
  out <- httr::content(tt, as = "text")
  data<-RJSONIO::fromJSON(content = out)
  return(data)
}
  
  
  




