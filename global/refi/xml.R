#' XML - validate project scheme
#' @param xml_doc
#' 
#' @return xml_validate 
#' @export
#' @example 
XML_validate_project_scheme <- function(xml_doc) {
  scheme <- read_xml(file.path(REFI_HOME, "xsd", "refi", "project-exchange.xsd"))
  v <- xml_validate(xml_doc, scheme)
  return(v)
}

#' XML validate codebook scheme 
#' @param xml_doc
#' 
#' @return xml_validate
#' 
#' @export
#' @example 
XML_validate_codebook_scheme <- function(xml_doc) {
  scheme <- read_xml(file.path(REFI_HOME, "xsd", "refi", "codebook-exchange.xsd"))
  v <- xml_validate(xml_doc, scheme)
  return(v)
}

#' XML has plain text content
#' @param text_source_node
#' 
#' @return TRUE (if ptc empty) or FALSE otherwise
XML_has_plain_text_content <- function(text_source_node) {
  ptc <- xml_find_all(text_source_node, "./d1:PlainTextContent")
  if (length(ptc) != 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
