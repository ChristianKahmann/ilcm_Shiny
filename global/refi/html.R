#' HTML - get annotation scheme
#' @param anno_file
#' 
#' @return document root
HTML_get_annotation_scheme <- function(anno_file){
  load(anno_file)
  doc <- xml_new_root("ul")
  HTML_get_nested_ul(anno, parent = NULL, doc)
  return(toString(xml_root(doc)))
}

#' HTML - get nested ul
#' @param anno
#' @param parent
#' @param doc
#' 
#' @return parent
#' 
#' @export
#' @example
HTML_get_nested_ul <- function(anno, parent, doc){
  ids <- names(anno)
  if (length(ids) > 0) {
    if (is.null(parent)){
      ul <- xml_root(doc)
    } else {
      ul <- xml_add_child(parent, "ul")
    }
    for (id in ids) {
      xml_add_child(ul, "li", anno[[id]]$name)
      HTML_get_nested_ul(anno[[id]]$sublist, parent = ul, doc = doc)
    }
  }
  return(parent)
}