tabPanel("ShinyAce",
         div(class="span6",
             h2("Source Code"),  
             aceEditor("code",theme ="chaos"  ,mode="r", value='anno<-matrix(c(
    "Ort","red",
    "Person","blue",
    "Firma","orange",
    "Adjektiv","green",
    "Verb","brown"),nrow=5,ncol=2,byrow=T)
  

annotation_scheme<-"anno"
save(anno,file=paste("collections/annotation_schemes/",annotation_scheme,".RData",sep = ""))
print(anno)
                    ')),
             actionButton("eval", "Evaluate"),
             #actionButton("save_anno","Save Annotation Scheme"),

div(class="span6",
    h2("Annotation scheme"),
    verbatimTextOutput("output_ace")
),
colourInput("col", "Select colour", "red")

)