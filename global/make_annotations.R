
getElementsForEntry <- function(annotations, level,edit_id = NULL) {
  gray_out_class <- ""
  
  if(!is.null(edit_id))
  {
    
    gray_out_class <- "gray_out_icon"
  }
  
  
  lapply(names(annotations), function(x) {
    el <- annotations[[x]]
    
    class <- paste0("Order", level, " ", "clickableAnnotation")
    content <- el[["name"]]
    
    if(el[["isDocumentAnnotation"]])
    {
      class <- paste0(class," documentAnnotation")
      content <- tagList(el$name,
                         icon("certificate"),
                         tags$span(id = paste0(x,"_","info"), icon("info",class=paste("category-info",gray_out_class)),title=el[["description"]]))
    }
    else
    {
      class <- paste0(class," regularAnnotation")
      content<-tagList(
      el$name,
      tags$span(id = paste0(x,"_","info"), icon("info",class=paste("category-info",gray_out_class)),title=el[["description"]])
      )
    }
    
    if (length(el[["sublist"]]) < 1)
    {
      new_div <-
        div(
          class = class,
          id = x,
          content,
          style = paste0("background-color:", el[["color"]])
        )
      return(new_div)
    }
    else
    {
      new_div <-
        div(
          class = class,
          id = x,
          tagList(content, getElementsForEntry(el[["sublist"]], level + 1)),
          style = paste0("background-color:", el[["color"]])
        )
      return(new_div)
    }
  })
}

annotateTextComponent_div <- function(anno) {
  
  elements <- getElementsForEntry(anno, 1)
  
  
  myStyle <- tags$style(
    "
    #document p {
    font-family:Droid Serif, serif;
    display:inline-block;
    margin:0em 0 1em 0;
    font-size:1.1em;
    letter-spacing:0.03em;
    min-width:95%;
    
    
}

i.gray_out_icon{
pointer-events: none;
opacity: 0.5;
}

#document p::selection{
text-shadow:1px 1px 0 #27ae60;
font-weight:bold;
color:rgb(10, 165, 225);

}

#document p span::selection{
text-shadow:1px 1px 0 #27ae60;
font-weight:bold;
color:rgb(10, 165, 225);

}

#document p span.marked{
text-shadow:1px 1px 0 #27ae60;
color:rgb(255, 255, 100);
}

#document p span.annoHighlight{
border-radius:3px !important;
box-shadow:#9ecaed 0 0 7px ;
color:rgb(255, 255, 100);
background-color:#9ecaed;
}

#document p span{
border-bottom:medium none;

}

#document p span:hover {
outline:none;
border-color:#9ecaed;
box-shadow:#9ecaed 0 0 7px;
}

#annotationContainer {
color:white;
font-size:1em;
font-weight:bold;
border: 1px solid #DDD;
box-shadow: 3px 3px 0px rgba(0,0,0, .2);
}

#annotationContainer div.annotationLegend{
color:black;
padding:0.2em;
}

#annotationContainer div.Order1{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

#annotationContainer div.unsetAnnotation{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
text-transform: uppercase;
}

#annotationContainer div.Order2{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

#annotationContainer div.Order3{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

.clickableAnnotation:hover{
background: rgb(106, 156, 237) !important; /* make this whatever you want */
/*content:\"Annotate\" !important;*/
}
"
  )
  
  myJavaScript <- 
    "
  $(document).ready(function() {
  
  $.selected = {};
  
  $('#annotationContainer div.regularAnnotation').click(function(e) {
  

  Shiny.onInputChange('anno_tag',$(e.target).attr('id'));
  Shiny.onInputChange('anno_start',$($.selected.start).attr('span_nr'));
  Shiny.onInputChange('anno_end',$($.selected.end).attr('span_nr'));
  Shiny.onInputChange('anno_id',Math.random().toString(36).substr(2, 9));
  //alert('Handler for A.click() called. Text is:\\n' + $.selected.text + '\\n' + 'Span Elements $.selected: ' + '');
  e.stopPropagation();
  $.deleteHighlight();
  });
  

  $('#annotationContainer div.documentAnnotation').click(function(e) {
  
  //alert('Handler for DA.click() called. Text is:\\n' + $.selected.text + '\\n' + 'Span Elements $.selected: ' + '');
  Shiny.onInputChange('anno_tag',$(e.target).attr('id'));
  Shiny.onInputChange('anno_start',$($.selected.start).attr('span_nr'));
  Shiny.onInputChange('anno_end',$($.selected.end).attr('span_nr'));
  Shiny.onInputChange('anno_id',Math.random().toString(36).substr(2, 9));
  e.stopPropagation();
  $.deleteHighlight();
  });
  
  $.deleteHighlight = function() {
  if($('#document span.annoHighlight').length > 0)
  {
  annotationSpan = $('#document span.annoHighlight')[0]
  inner =  $(annotationSpan).html(); 
  $(annotationSpan).replaceWith(inner);
  $('span.marked').removeClass('marked');
  $.selected = {};
  }
  
  }   
  
  $('#annotationContainer div.unsetAnnotation').click(function(e) {
  
  e.stopPropagation();
  $.deleteHighlight();
  
  });
  
  });
  "
  
  
  
  delete_div <-
    div(
      class = "unsetAnnotation",
      tagList("Unset Annotation",icon("eraser")),
      style = paste0("background-color:", "#FC2847")
    )
  myTags <- tagList(elements,delete_div)
  annotation <- div(id = "annotationContainer",
                    tagList(tags$script(HTML(myJavaScript)), myStyle, myTags, div(tagList(icon("certificate"),"=Document-wide annotation"),class="annotationLegend")))
  
  return(annotation)
}


annotateTextComponent_div2 <- function(anno) {
 
    elements <- getElementsForEntry(anno, 1)
  
  
  myStyle <- tags$style(
    "
    #Doc_document p {
    font-family:Droid Serif, serif;
    display:inline-block;
    margin:0em 0 1em 0;
    font-size:1.1em;
    letter-spacing:0.03em;
    min-width:95%;
    
    
}


#Doc_document p::selection{
text-shadow:1px 1px 0 #27ae60;
font-weight:bold;
color:rgb(10, 165, 225);

}

#Doc_document p span::selection{
text-shadow:1px 1px 0 #27ae60;
font-weight:bold;
color:rgb(10, 165, 225);

}

#Doc_document p span.marked{
text-shadow:1px 1px 0 #27ae60;
color:rgb(255, 255, 100);
}

#Doc_document p span.annoHighlight{
border-radius:3px !important;
box-shadow:#9ecaed 0 0 7px ;
color:rgb(255, 255, 100);
background-color:#9ecaed;
}

#Doc_document p span{
border-bottom:medium none;

}

#Doc_document p span:hover {
outline:none;
border-color:#9ecaed;
box-shadow:#9ecaed 0 0 7px;
}

#annotationContainer2 {
color:white;
font-size:1em;
font-weight:bold;
border: 1px solid #DDD;
box-shadow: 3px 3px 0px rgba(0,0,0, .2);
}

#annotationContainer2 div.annotationLegend{
color:black;
padding:0.2em;
}

#annotationContainer2 div.Order1{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

#annotationContainer2 div.unsetAnnotation{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
text-transform: uppercase;
}

#annotationContainer2 div.Order2{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);

#annotationContainer2 div.Order3{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}
}
.clickableAnnotation:hover{
background: rgb(106, 156, 237) !important; /* make this whatever you want */
/*content:\"Annotate\" !important;*/
}
"
  )
  
  myJavaScript <- 
    "
  $(document).ready(function() {
  
  $.selected = {};
  
  $('#annotationContainer2 div.regularAnnotation').click(function(e) {
  
  
  Shiny.onInputChange('Doc_anno_tag',$(e.target).attr('id'));
  Shiny.onInputChange('Doc_anno_start',$($.selected.start).attr('span_nr'));
  Shiny.onInputChange('Doc_anno_end',$($.selected.end).attr('span_nr'));
  Shiny.onInputChange('Doc_anno_id',Math.random().toString(36).substr(2, 9));
  console.log($(e.target).attr('id'));
  console.log($($.selected.start).attr('span_nr'));


  e.stopPropagation();
  $.Doc_deleteHighlight();
  });
  
  $('#annotationContainer2 div.documentAnnotation').click(function(e) {
  
 
  Shiny.onInputChange('Doc_anno_tag',$(e.target).attr('id'));
  Shiny.onInputChange('Doc_anno_start',$($.selected.start).attr('span_nr'));
  Shiny.onInputChange('Doc_anno_end',$($.selected.end).attr('span_nr'));
  Shiny.onInputChange('Doc_anno_id',Math.random().toString(36).substr(2, 9));
  e.stopPropagation();
  $.Doc_deleteHighlight();
  });
  
  $.Doc_deleteHighlight = function() {
  if($('#Doc_document span.annoHighlight').length > 0)
  {
  annotationSpan = $('#Doc_document span.annoHighlight')[0]
  inner =  $(annotationSpan).html(); 
  $(annotationSpan).replaceWith(inner);
  $('span.marked').removeClass('marked');
  $.selected = {};
  }
  
  }   
  
  $('#annotationContainer2 div.unsetAnnotation').click(function(e) {
  
  e.stopPropagation();
  $.Doc_deleteHighlight();
  
  });
  
  });
  "
  
  
  
  delete_div <-
    div(
      class = "unsetAnnotation",
      tagList("Unset Annotation",icon("eraser")),
      style = paste0("background-color:", "#FC2847")
    )
  myTags <- tagList(elements,delete_div)
  annotation <- div(id = "annotationContainer2",
                    tagList(tags$script(HTML(myJavaScript)), myStyle, myTags, div(tagList(icon("certificate"),"=Document-wide annotation"),class="annotationLegend")))
  
  return(annotation)
}



annotateTextComponent_div3 <- function(anno) {
 
  elements <- getElementsForEntry(anno, 1)
  
  
  myStyle <- tags$style(
    "
    #Anno_document p {
    font-family:Droid Serif, serif;
    display:inline-block;
    margin:0em 0 1em 0;
    font-size:1.1em;
    letter-spacing:0.03em;
    min-width:95%;
    
    
}


#Anno_document p::selection{
text-shadow:1px 1px 0 #27ae60;
font-weight:bold;
color:rgb(10, 165, 225);

}

#Anno_document p span::selection{
text-shadow:1px 1px 0 #27ae60;
font-weight:bold;
color:rgb(10, 165, 225);

}

#Anno_document p span.marked{
text-shadow:1px 1px 0 #27ae60;
color:rgb(255, 255, 100);
}

#Anno_document p span.annoHighlight{
border-radius:3px !important;
box-shadow:#9ecaed 0 0 7px ;
color:rgb(255, 255, 100);
background-color:#9ecaed;
}

#Anno_document p span{
border-bottom:medium none;

}

#Anno_document p span:hover {
outline:none;
border-color:#9ecaed;
box-shadow:#9ecaed 0 0 7px;
}

#annotationContainer3 {
color:white;
font-size:1em;
font-weight:bold;
border: 1px solid #DDD;
box-shadow: 3px 3px 0px rgba(0,0,0, .2);
}

#annotationContainer3 div.annotationLegend{
color:black;
padding:0.2em;
}

#annotationContainer3 div.Order1{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

#annotationContainer3 div.unsetAnnotation{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
text-transform: uppercase;
}

#annotationContainer3 div.Order2{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

#annotationContainer3 div.Order3{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

.clickableAnnotation:hover{
background: rgb(106, 156, 237) !important; /* make this whatever you want */
/*content:\"Annotate\" !important;*/
}
"
  )
  
  myJavaScript <- 
    "
  $(document).ready(function() {
  
  $.selected = {};
  
  $('#annotationContainer3 div.regularAnnotation').click(function(e) {
  
  
  Shiny.onInputChange('Anno_anno_tag',$(e.target).attr('id'));
  Shiny.onInputChange('Anno_anno_start',$($.selected.start).attr('span_nr'));
  Shiny.onInputChange('Anno_anno_end',$($.selected.end).attr('span_nr'));
  Shiny.onInputChange('Anno_anno_id',Math.random().toString(36).substr(2, 9));
  
  
  e.stopPropagation();
  $.Anno_deleteHighlight();
  });
  
  $('#annotationContainer3 div.documentAnnotation').click(function(e) {
  
  
  Shiny.onInputChange('Anno_anno_tag',$(e.target).attr('id'));
  Shiny.onInputChange('Anno_anno_start',$($.selected.start).attr('span_nr'));
  Shiny.onInputChange('Anno_anno_end',$($.selected.end).attr('span_nr'));
  Shiny.onInputChange('Anno_anno_id',Math.random().toString(36).substr(2, 9));
  e.stopPropagation();
  $.Anno_deleteHighlight();
  });
  
  $.Anno_deleteHighlight = function() {
  if($('#Anno_document span.annoHighlight').length > 0)
  {
  annotationSpan = $('#Anno_document span.annoHighlight')[0]
  inner =  $(annotationSpan).html(); 
  $(annotationSpan).replaceWith(inner);
  $('span.marked').removeClass('marked');
  $.selected = {};
  }
  
  }   
  
  $('#annotationContainer3 div.unsetAnnotation').click(function(e) {
  
  e.stopPropagation();
  $.Anno_deleteHighlight();
  
  });
  
  });
  "
  
  
  
  delete_div <-
    div(
      class = "unsetAnnotation",
      tagList("Unset Annotation",icon("eraser")),
      style = paste0("background-color:", "#FC2847")
  )
  myTags <- tagList(elements,delete_div)
  annotation <- div(id = "annotationContainer3",
                    tagList(tags$script(HTML(myJavaScript)), myStyle, myTags, div(tagList(icon("certificate"),"=Document-wide annotation"),class="annotationLegend")))
  return(annotation)
  }






annotateTextComponent <- function() {
  script <- "
  
  $.annoStart = null;
  $.annoEnd = null;
  
  $(document).on('mousedown', '#document span', function (e) {
  
  $.deleteHighlight();
  $.selected = {};
  $.annoStart = $(e.target); 
  });
  
  $(document).on('mouseup', '#document span', function (e) {
  
  if($.annoStart === null)
  return;
  
  $.annoEnd = $(e.target);
  
  if (window.getSelection) {
  sel = window.getSelection();
  sel.removeAllRanges();
  }
  else if ( (sel = document.selection)) {
  sel.removeAllRanges();
  }
  
  posStart = $('#document p').contents().index($.annoStart);
  posEnd = $('#document p').contents().index($.annoEnd);
  
  if(posStart < posEnd)
  {
  
  if(!$.annoStart.parent().is($.annoEnd.parent()))
  {
  $.annoEnd = $.annoStart.parent().children('span').last();
  posEnd = $('#document p').contents().index($.annoEnd);
  }
  
  tmp = $('#document p').contents().slice(posStart, posEnd + 1);
  tmp.addClass('marked');
  $.selected.text = $.annoStart.text();
  $.selected.start = $.annoStart;
  $.selected.end = $.annoEnd;
  tmp.wrapAll(\"<span class='annoHighlight' />\");
  }else if(posEnd < posStart)
  {
  
  if(!$.annoEnd.parent().is($.annoStart.parent()))
  {
  $.annoEnd = $.annoStart.parent().children('span').first();
  posEnd = $('#document p').contents().index($.annoEnd);
  }
  
  tmp = $('#document p').contents().slice(posEnd, posStart + 1);
  tmp.addClass('marked');
  $.selected.text = $.annoStart.text();
  $.selected.start = $.annoStart;
  $.selected.end = $.annoEnd;
  tmp.wrapAll(\"<span class='annoHighlight' />\");
        } else
        {
          $.annoStart.addClass('marked');
          $.selected.text = $.annoStart.text();
          $.selected.start = $.annoStart;
          $.selected.end = $.annoEnd;
          $.annoStart.wrapAll(\"<span class='annoHighlight' />\");
        }

        $.annoStart = null;
        $.annoEnd = null;
        
       
  });    
  
  "
  
  return(tagList(tags$script(HTML(script)), htmlOutput("document")))
  
}



annotateTextComponent2 <- function() {
  script <- "
  
  $.annoStart = null;
  $.annoEnd = null;
  
  $(document).on('mousedown', '#Doc_document span', function (e) {
  
  $.Doc_deleteHighlight();
  $.selected = {};
  $.annoStart = $(e.target); 
  });
  
  $(document).on('mouseup', '#Doc_document span', function (e) {
  
  if($.annoStart === null)
  return;
  
  $.annoEnd = $(e.target);
  
  if (window.getSelection) {
  sel = window.getSelection();
  sel.removeAllRanges();
  }
  else if ( (sel = document.selection)) {
  sel.removeAllRanges();
  }
  
  posStart = $('#Doc_document p').contents().index($.annoStart);
  posEnd = $('#Doc_document p').contents().index($.annoEnd);
  
  if(posStart < posEnd)
  {
  
  if(!$.annoStart.parent().is($.annoEnd.parent()))
  {
  $.annoEnd = $.annoStart.parent().children('span').last();
  posEnd = $('#Doc_document p').contents().index($.annoEnd);
  }
  
  tmp = $('#Doc_document p').contents().slice(posStart, posEnd + 1);
  tmp.addClass('marked');
  $.selected.text = $.annoStart.text();
  $.selected.start = $.annoStart;
  $.selected.end = $.annoEnd;
  tmp.wrapAll(\"<span class='annoHighlight' />\");
  }else if(posEnd < posStart)
  {
  
  if(!$.annoEnd.parent().is($.annoStart.parent()))
  {
  $.annoEnd = $.annoStart.parent().children('span').first();
  posEnd = $('#Doc_document p').contents().index($.annoEnd);
  }
  
  tmp = $('#Doc_document p').contents().slice(posEnd, posStart + 1);
  tmp.addClass('marked');
  $.selected.text = $.annoStart.text();
  $.selected.start = $.annoStart;
  $.selected.end = $.annoEnd;
  tmp.wrapAll(\"<span class='annoHighlight' />\");
        } else
        {
          $.annoStart.addClass('marked');
          $.selected.text = $.annoStart.text();
          $.selected.start = $.annoStart;
          $.selected.end = $.annoEnd;
          $.annoStart.wrapAll(\"<span class='annoHighlight' />\");
        }

        $.annoStart = null;
        $.annoEnd = null;
        
       
  });    
  
  "
  
  return(tagList(tags$script(HTML(script)), htmlOutput("Doc_document")))
  
}




annotateTextComponent3 <- function() {
  script <- "
  
  $.annoStart = null;
  $.annoEnd = null;
  
  $(document).on('mousedown', '#Anno_document span', function (e) {
  
  $.Anno_deleteHighlight();
  $.selected = {};
  $.annoStart = $(e.target); 
  });
  
  $(document).on('mouseup', '#Anno_document span', function (e) {
  
  if($.annoStart === null)
  return;
  
  $.annoEnd = $(e.target);
  
  if (window.getSelection) {
  sel = window.getSelection();
  sel.removeAllRanges();
  }
  else if ( (sel = document.selection)) {
  sel.removeAllRanges();
  }
  
  posStart = $('#Anno_document p').contents().index($.annoStart);
  posEnd = $('#Anno_document p').contents().index($.annoEnd);
  
  if(posStart < posEnd)
  {
  
  if(!$.annoStart.parent().is($.annoEnd.parent()))
  {
  $.annoEnd = $.annoStart.parent().children('span').last();
  posEnd = $('#Anno_document p').contents().index($.annoEnd);
  }
  
  tmp = $('#Anno_document p').contents().slice(posStart, posEnd + 1);
  tmp.addClass('marked');
  $.selected.text = $.annoStart.text();
  $.selected.start = $.annoStart;
  $.selected.end = $.annoEnd;
  tmp.wrapAll(\"<span class='annoHighlight' />\");
  }else if(posEnd < posStart)
  {
  
  if(!$.annoEnd.parent().is($.annoStart.parent()))
  {
  $.annoEnd = $.annoStart.parent().children('span').first();
  posEnd = $('#Anno_document p').contents().index($.annoEnd);
  }
  
  tmp = $('#Anno_document p').contents().slice(posEnd, posStart + 1);
  tmp.addClass('marked');
  $.selected.text = $.annoStart.text();
  $.selected.start = $.annoStart;
  $.selected.end = $.annoEnd;
  tmp.wrapAll(\"<span class='annoHighlight' />\");
        } else
        {
          $.annoStart.addClass('marked');
          $.selected.text = $.annoStart.text();
          $.selected.start = $.annoStart;
          $.selected.end = $.annoEnd;
          $.annoStart.wrapAll(\"<span class='annoHighlight' />\");
        }

        $.annoStart = null;
        $.annoEnd = null;
        
       
  });    
  
  "
  
  return(tagList(tags$script(HTML(script)), htmlOutput("Anno_document")))
  
}











