source("global/text_functions.R")
source("global/log_to_file.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")

error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  
  #load collection 
  log_to_file(message = "<b>Step 1/9: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/9: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/9: Sanity check</b>",file = logfile)
  #token object not empty
  log_to_file(message = "&emsp; token object not empty?",logfile)
  if(dim(db_data$token)[1]>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No documents were found in the database for the specified collection.</b>",logfile)
    stop("Token empty")
  }
  #specified language has quanteda stopwordlist?
  if(parameters$remove_stopwords==T){
    log_to_file(message = "&emsp; stopwords available for specified language?",logfile)
    if(!(db_data$language%in%stopwords::stopwords_getlanguages(source="stopwords-iso"))){
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; The specified language is not included in stop word list. You can maybe specify a stopwordlist using the blacklist functionality.</b>",file = logfile)
      stop("Stopwords not available for found language")
    }
    else{
      log_to_file(message = "&emsp; ✔",logfile)
    }
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/9: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #preparing token object
  log_to_file(message = "<b>Step 5/9: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token,parameters=parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  #calculating dtm
  log_to_file(message = "<b>Step 6/9: Calculating DTM</b>",file = logfile)
  dtm<-calculate_dtm_for_dictionary_extraction(token = db_data$token,parameters = parameters,lang = db_data$language)
  db_data$meta<-db_data$meta[which(db_data$meta[,1]%in%rownames(dtm)),]
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  

  #parse dictionaries  
  log_to_file(message = "<b>Step 7/9: Parsing dictionary</b>",file = logfile)
  if(isFALSE(parameters$de_use_reg_exp)){
    load(paste0("collections/dictionaries/",parameters$Dictionary,".RData"))
    dicts<-dict
    conceptnames<-names(dicts)
  }
  else{
    dict<-colnames(dtm)[stringr::str_detect(string = colnames(dtm),pattern = parameters$de_reg_exp)]
    if(length(dict)==0){
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; Your regular expression did not match any word of the dtm. 
                  Maybe you need to reset your pre-processing settings or check the regular expression</b>",file = logfile)
      stop("empty dictionary")
    }
    else{
    conceptnames<-parameters$de_reg_exp
    dicts<-quanteda::dictionary(x = list(conceptnames=dict))
    }
  }
  bin_dtm<-tmca.util::make_binary(dtm = dtm)
  vocab<-colnames(dtm)
  dict_terms<-c(unlist(dicts))
  dict_terms<-intersect(dict_terms,vocab)
  dicts_available<-lapply(1:length(dicts),function(x){
    return(intersect(vocab,dicts[[x]]))
  })
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished parsing the dictionary",file = logfile)
  
  
  #parse dictionaries  
  log_to_file(message = "<b>Step 8/9: Calculate frequencies</b>",file = logfile)
  frequencies<-calculate_dictioanry_frequencies(meta =db_data$meta,dtm = dtm,dict_terms = dict_terms,conceptnames = conceptnames,dicts_available = dicts_available,bin_dtm = bin_dtm )
  doc_freqs_year_dict<-frequencies$doc_freqs_year_dict
  doc_freqs_month_dict<-frequencies$doc_freqs_month_dict
  doc_freqs_week_dict<-frequencies$doc_freqs_week_dict
  doc_freqs_day_dict<-frequencies$doc_freqs_day_dict
  freqs_year_dict<-frequencies$freqs_year_dict
  freqs_month_dict <-frequencies$freqs_month_dict
  freqs_week_dict<-frequencies$freqs_week_dict
  freqs_day_dict<-frequencies$freqs_day_dict
  rel_doc_freqs_year_dict<-frequencies$rel_doc_freqs_year_dict
  rel_doc_freqs_month_dict<-frequencies$rel_doc_freqs_month_dict
  rel_doc_freqs_week_dict<-frequencies$rel_doc_freqs_week_dict
  rel_doc_freqs_day_dict<-frequencies$rel_doc_freqs_day_dict
  rel_freqs_year_dict<-frequencies$rel_freqs_year_dict
  rel_freqs_month_dict<-frequencies$rel_freqs_month_dict
  rel_freqs_week_dict<-frequencies$rel_freqs_week_dict
  rel_freqs_day_dict<-frequencies$rel_freqs_day_dict
  
  vocab<-unlist(conceptnames)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating frequencies",file = logfile)
  

  
  
  #Saving results
  log_to_file(message = "<b>Step 9/9: Saving results</b>",file = logfile)
  path<-paste(parameters$id,parameters$collection,sep = "_")
  path0<-paste0("collections/results/dictionary-extraction/",path,"/")
  dir.create(path0)
  use_reg_exp_as_input<-parameters$de_use_reg_exp
  save(freqs_day_dict,freqs_week_dict,freqs_month_dict,freqs_year_dict,
       doc_freqs_day_dict,doc_freqs_week_dict,doc_freqs_month_dict,doc_freqs_year_dict,
       rel_freqs_day_dict,rel_freqs_week_dict,rel_freqs_month_dict,rel_freqs_year_dict,
       rel_doc_freqs_day_dict,rel_doc_freqs_week_dict,rel_doc_freqs_month_dict,rel_doc_freqs_year_dict,
       dict_terms,use_reg_exp_as_input,file = paste0(path0,"frequencies.RData"))
  save(dtm,dicts,file=paste0(path0,"extra_information.RData"))
  save(vocab,file=paste0(path0,"vocab.RData"))
  save(info,file=paste0(path0,"info.RData"))
  parameters<-parameters_original
  save(parameters,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  
  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Term Frequency Extraction </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}



