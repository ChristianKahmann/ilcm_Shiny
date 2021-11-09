source("global/text_functions.R")
source("global/log_to_file.R")
source("config_file.R")
source("global/TextProcessingBackend.R")
source("global/TextObjectWrapper.R")
source("global/rbind_huge_sparse_Matrix.R")
source("global/functions_used_in_scripts.R")
source("global/utils.R")
source("global/functions_for_classification.R")
source("global/classification_randomForest.R")
source("global/classification_xgBoost.R")
source("global/classification_decissionTree.R")
error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  library(magrittr)
  library(randomForest)
  require(LiblineaR)
  require(SparseM)
  require(xgboost)
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  
  
  
  #load collection 
  log_to_file(message = "<b>Step 1/13: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/13: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_language=T,get_global_doc_ids=T,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #getting annotations for chosen collection,project and category
  log_to_file(message = "<b>Step 3/13: Loading annotations from database</b>",file = logfile)
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  annotations<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where Anno_set='",parameters$Project,"';",sep=""))
  classifications_approved<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",parameters$Project,"';"))
  RMariaDB::dbDisconnect(mydb)
####check for document/sentece level annotations
  updated_values<-check_annotations(parameters, annotations, classifications_approved)
  annotations<-updated_values$annotations
  classifications_approved<-updated_values$classifications_approved
####
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading annotations from database",file = logfile)
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/13: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #sanity check
  log_to_file(message = "<b>Step 5/13: Sanity check</b>",file = logfile)
  #token object not empty
  log_to_file(message = "&emsp; token object not empty?",logfile)
  if(dim(db_data$token)[1]>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No documents were found in the database for the specified collection.</b>",logfile)
    stop("Token empty")
  }
####annotations for documents in collection?
  anno_ids<-unique(c(paste0(annotations[,"dataset"],"_",annotations[,"id"]),paste0(classifications_approved[,"dataset"],"_",classifications_approved[,"doc_id"])))
  anno_ids_in_token<-annotation_in_collection(anno_ids,db_data, parameters)
####
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
####get original documents 
  log_to_file(message = "<b>Step 5/13: Create original documents</b>",file = logfile)
  documents_original<-get_og_doc(parameters, db_data)
####
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  #load dictionary
  log_to_file(message = "<b>Step 6/13: Try loading dictionary</b>",file = logfile)
  try({
    if(parameters$use_dictionary==TRUE){
      load(paste0("collections/dictionaries/",parameters$Dictioanry,".RData"))
      log_to_file(message = "  <b style='color:green'> ✔ </b>  Dictionary loaded",file = logfile)
    }
    else{
      dict<-""
      log_to_file(message = "  <b style='color:green'> ✔ </b>  Not using a dictionary in this task",file = logfile)
    }
  })
  

  #load annotation set according to chosen project
  log_to_file(message = "<b>Step 7/13: Loading annotation set</b>",file = logfile)
  load(paste0("collections/annotation_schemes/",parameters$Project,".RData"))
  names_overall<-names(unlist(anno))
  anno<-unlist(anno)
  name_tag<-names_overall[which(grepl(pattern = paste0(NULL,".name"),x = names_overall))]
  names<-anno[name_tag]
  color_tag<-names_overall[which(grepl(pattern = paste0(NULL,".color"),x = names_overall))]
  colors<-anno[color_tag]
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Annotationset loaded",file = logfile)
  
  #set path right for saving results according to chosen mode
  if(parameters$cl_Mode=="Evaluate Training Set"){
    path0<-paste0("collections/results/classification/","evaluateTraining/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  if(parameters$cl_Mode=="Produce 50 new active learning examples"){
    path0<-paste0("collections/results/classification/","activeLearning/",parameters$Project,"/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  if(parameters$cl_Mode=="Classify on entire collection"){
    path0<-paste0("collections/results/classification/","classifyCollection/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  if(parameters$cl_Mode=="Active learning on whole documents"){
    path0<-paste0("collections/results/classification/","activeLearning_documents/",parameters$Project,"/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  
  
  #preparing token object
  log_to_file(message = "<b>Step 8/13: Preparing token object</b>",file = logfile)
  datasets<-db_data$token[,1]
  db_data$token<-prepare_token_object(token = db_data$token,parameters=parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  #getting original documents
  log_to_file(message = "<b>Step 9/13: Getting original documents/sentences</b>",file = logfile)
  original_text<-get_original_documents(token=db_data$token)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished building original documents/sentences",file = logfile)
  
  #formatting classification input as a tibble
  log_to_file(message = "<b>Step 10/13: Formatting classification input</b>",file = logfile)
  #feature_list<-calculate_dtm(tibble=T)
  dtm<-calculate_dtm(tibble=F,token = db_data$token,parameters = parameters,lang = db_data$language)
  #get_annotated_doc_ids
  log_to_file(message = paste0("&emsp; Reduce feature space"),logfile)
  pos_ident<-NULL
  #anno_pos<-annotations[which(annotations[,"Annotation"]==parameters$cl_Category),]
  anno_pos<-annotations
####
  if(parameters$cooc_window=="Sentence"){
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
    sum_sent<-formatting_sentence(dtm, anno_pos, pos_ident, classifications_approved)
    dtm<-sum_sent$dtm
    ign<-sum_sent$ign
  }
  if(parameters$cooc_window=="Document"){
    sum_doc<-formatting_document(dtm, anno_pos, pos_ident, classifications_approved)
    dtm<-sum_doc$dtm
    ign<-sum_doc$ign
  }
######  
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished formatting classification input",file = logfile)
  
  #insert made annotations and approved classifications
  log_to_file(message = "<b>Step 11/13: Inserting made annotations and approved classifications as training input</b>",file = logfile)
  gold_table<-matrix(c(0),0,4)
  if(parameters$cooc_window=="Sentence"){
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
    count=0
    try({
      for(i in 1:dim(annotations)[1]){
        id_ws<-paste(annotations[i,"from"]:annotations[i,"to"],collapse = ", ")
        identifier<-paste(annotations[i,"dataset"],annotations[i,"id"],unique(RMariaDB::dbGetQuery(mydb, paste("select sid from token where dataset='",annotations[i,"dataset"],"' and id= ",annotations[i,"id"],";",sep=""))[annotations[i,"from"]:annotations[i,"to"],1]),sep="_")
        try({
          # my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=as.character(annotations[i,"Annotation"]),coder=as.character(annotations[i,"User"]),timestamp=as.character(annotations[i,"Annotation_Date"])))
          count=count+1
          gold_table<-rbind(gold_table,cbind(identifier,as.character(annotations[i,"Annotation"]),as.character(annotations[i,"User"]),as.character(annotations[i,"Annotation_Date"])))
        })
      }
      #for approved classifications
      if(length(ign)>1){
        class_appr<-classifications_approved[-ign,]
      }
      else{
        class_appr<-classifications_approved
      }
###
      check_sent<-check_classification_sentence(class_appr, gold_table)
      gold_table<-check_sent$gold_table
      count<-check_sent$count
###
    })
    RMariaDB::dbDisconnect(mydb)
  }
  if(parameters$cooc_window=="Document"){
    count=0
    try({
###
      check_doc<-check_classification_document(class_appr, gold_table, count)
      gold_table<-check_doc$gold_table
      count<-check_doc$count
###
    })
    
  }
  if(count<1 && parameters$use_dictionary==F){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No annotation was found for your specified collection and annotation set.
              Also you chose not to use a dictionary. The system can not calculate classifications without any training examples generated by annotations or using a dictionary.</b>",logfile)
    stop("No Training data")
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  #ensure c parameter is set // for older versions
  if(is.null(parameters$cl_c)){
    parameters$cl_c=1
  }
  
  #######learning examples#########################################################################
  if(parameters$cl_Mode=="Produce 50 new active learning examples"){
    ############################################
    #           learning example               #
    ############################################
    log_to_file(message = "<b>Step 12/13: Producing 50 new active learning examples</b>",file = logfile)
    log_to_file(message = paste("&emsp;",count, "Training sets (annotations and approved classifications) were found and used"),file = logfile)
    if(parameters$CL_Method == "SVM"){
    set_learning_samples_svm(parameters, gold_table, dtm)
    print("SVM selected")
    }
    if(parameters$CL_Method == "randomForest"){
      set_learning_samples_rF(parameters, gold_table, dtm)
      print("random Forest selected")
    }
    if(parameters$CL_Method == "XGBoost"){
      set_learning_samples_xgb(parameters, gold_table, dtm)
      print("XGBoost selected")
    }
    if(parameters$CL_Method == "decissionTree"){
      set_learning_samples_dT(parameters, gold_table, dtm)
    }
  }
  if(parameters$cl_Mode=="Evaluate Training Set"){
    ############################################
    #           Training Set Evaluation        #
    ############################################
    log_to_file(message = "<b>Step 12/13: Evaluating Training Set</b>",file = logfile)
    set_training_eval(parameters, gold_table, dtm)
  }
  
  if(parameters$cl_Mode=="Active learning on whole documents"){
    ############################################
    #   Active Learning on whole Documents     #
    ############################################
    log_to_file(message = "<b>Step 12/13: Active learning on whole documents</b>",file = logfile)
    #add negative examples if no other categories tagged or no NEG examples given
    if(parameters$CL_Method == "SVM"){
    set_active_learning_whole_svm(parameters, gold_table, dtm)
    }
    if(parameters$CL_Method == "randomForest"){
      set_active_learning_whole_rF(parameters, gold_table, dtm)
    }
    if(parameters$CL_Method == "XGBoost"){
      set_active_learning_whole_xgb(parameters, gold_table, dtm)
    }
    if(parameters$CL_Method == "decissionTree"){
      set_active_learning_whole_dT(parameters, gold_table, dtm)
    }
    
  }
  if(parameters$cl_Mode=="Classify on entire collection"){
    ############################################
    #       Classify on entire collection      #
    ############################################
    log_to_file(message = "<b>Step 12/13: Classification on entire collection</b>",file = logfile)
    if(parameters$CL_Method == "SVM"){
    classify_whole_collection_svm(parameters, gold_table, dtm)
    }
    if(parameters$CL_Method == "randomForest"){
      classify_whole_collection_rF(parameters, gold_table, dtm)
    }
    if(parameters$CL_Method == "XGBoost"){
      classify_whole_collection_xgb(parameters, gold_table, dtm)
    }
    if(parameters$CL_Method == "decissionTree"){
      classify_whole_collection_dT(parameters, gold_table, dtm)
    }
  }
  
  
  log_to_file(message = " <b style='color:green'>Process finished successfully.</b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  RMariaDB::dbDisconnect(mydb)
}) 

if(class(error)=="try-error"){
  try({
    system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  })
  try({
    RMariaDB::dbDisconnect(mydb)
  })
  log_to_file(message = "&emsp;<b style='color:red'>An error has occurred: </b>",file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}

