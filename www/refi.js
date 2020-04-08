Shiny.addCustomMessageHandler("getTopics", getTopics);
Shiny.addCustomMessageHandler("getSelectedTopicModel", getSelectedTopicModel);
Shiny.addCustomMessageHandler("getSelectedAnnotationScheme", getSelectedAnnotationScheme);
Shiny.addCustomMessageHandler("getSelectedAnnotationSchemeImport", getSelectedAnnotationSchemeImport);

function getClassification(button){
  var classification = button.getAttribute("data-classification");
  Shiny.onInputChange("classification", classification);
}

function getCollection(button){
  console.log(button);
  var collection = button.getAttribute("data-collection");
  Shiny.onInputChange("collection", collection);
}

function getCollectionAnnoScheme(button){
  var collectionAnnoScheme = button.getAttribute("data-anno-scheme");
  Shiny.onInputChange("collectionAnnoScheme", collectionAnnoScheme);
}

function getTopicModel(button){
  var topicModel = button.getAttribute("data-analysis");
  Shiny.onInputChange("topicModel", topicModel);
}

function getAnnotationScheme(button){
  var annotationScheme = button.getAttribute("data-anno-scheme");
  Shiny.onInputChange("annotationScheme", annotationScheme);
}

function getTopics(message){
  var divs = refi_export_topic_model_number_of_topics_table.querySelectorAll("tr[class*='selected'] > td > div");
  var selectedTopics = [];
  divs.forEach(function(topic){
   selectedTopics.push(topic.getAttribute("data-topic-rank")); 
  });
  Shiny.onInputChange("selectedTopics", selectedTopics);
}

function getSelectedTopicModel(message){
  var selectedTopicModel = refi_export_topic_model_table.querySelector("tr[class*='selected'] > td > button").getAttribute("data-analysis");
  Shiny.onInputChange("selectedTopicModel", selectedTopicModel);
}

function getSelectedAnnotationScheme(message){
  var selectedAnnotationScheme = refi_export_table_annotation_scheme.querySelector("tr[class*='selected'] > td > button").getAttribute("data-anno-scheme");
  Shiny.onInputChange("selectedAnnotationScheme", selectedAnnotationScheme);
}

function getSelectedAnnotationSchemeImport(message){
  console.log("hier");
  console.log(message);
  var selectedAnnotationSchemeImport = refi_import_table_annotation_scheme.querySelector("tr[class*='selected'] > td > span").getAttribute("data-anno-scheme");
  Shiny.onInputChange("selectedAnnotationSchemeImport", selectedAnnotationSchemeImport);
}
