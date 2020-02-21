get_task_id_counter<-function(){
  load("global/task_id_counter.RData")
  return(task_id_counter)
}

set_task_id_counter<-function(id){
  task_id_counter<-id
  save(task_id_counter,file = "global/task_id_counter.RData")
}