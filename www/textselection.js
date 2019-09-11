

window.onload = function(){
  document.getElementById('annotate').addEventListener('click', addMarkup);
  document.getElementById('Legend').addEventListener("mousedown",addMarkup);
  document.getElementById('Doc_annotate').addEventListener('click', addMarkup_Doc);
  document.getElementById('Doc_Legend').addEventListener("mousedown",addMarkup_Doc);
}


function addMarkup(){
  var el = document.getElementById("document").innerHTML;
  var selection="";
  if(window.getSelection){
    selection = window.getSelection().toString();
  }
  else if(document.selection && document.selection.type != "Control"){
    selection = document.selection.createRange().text;
  }
  if(selection.length === 0){
    return;
  }
  marked = "<ma>".concat(selection).concat("</ma>");  
  var sel, range;
    if (window.getSelection) {
        sel = window.getSelection();
        if (sel.rangeCount) {
            range = sel.getRangeAt(0);
            range.deleteContents();
            range.insertNode(document.createTextNode(marked));
        }
    } else if (document.selection && document.selection.createRange) {
        range = document.selection.createRange();
        range.text = marked;
    }

  Shiny.onInputChange("textresult",document.getElementById("document").innerHTML);
}



function addMarkup_Doc(){

  var el = document.getElementById("Doc_document").innerHTML;
  var selection="";
  if(window.getSelection){
    selection = window.getSelection().toString();
  }
  else if(document.selection && document.selection.type != "Control"){
    selection = document.selection.createRange().text;
  }
  if(selection.length === 0){
    return;
  }
  marked = "<ma>".concat(selection).concat("</ma>");  


  var sel, range;
    if (window.getSelection) {
        sel = window.getSelection();
        if (sel.rangeCount) {
            range = sel.getRangeAt(0);
            range.deleteContents();
            range.insertNode(document.createTextNode(marked));
        }
    } else if (document.selection && document.selection.createRange) {
        range = document.selection.createRange();
        range.text = marked;
    }

  Shiny.onInputChange("Doc_textresult",document.getElementById("Doc_document").innerHTML);
}

















function replaceSelectedText(replacementText) {
    var sel, range;
    if (window.getSelection) {
        sel = window.getSelection();
        if (sel.rangeCount) {
            range = sel.getRangeAt(0);
            range.deleteContents();
            range.insertNode(document.createTextNode(replacementText));
        }
    } else if (document.selection && document.selection.createRange) {
        range = document.selection.createRange();
        range.text = replacementText;
    }
}






