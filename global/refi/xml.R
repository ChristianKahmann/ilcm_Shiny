XML_validate_project_scheme <- function(xml_doc) {
  scheme <- read_xml(file.path(REFI_HOME, "xsd", "refi", "project-exchange.xsd"))
  v <- xml_validate(xml_doc, scheme)
  return(v)
}

XML_validate_codebook_scheme <- function(xml_doc) {
  scheme <- read_xml(file.path(REFI_HOME, "xsd", "refi", "codebook-exchange.xsd"))
  v <- xml_validate(xml_doc, scheme)
  return(v)
}

XML_has_plain_text_content <- function(text_source_node) {
  ptc <- xml_find_all(text_source_node, "./d1:PlainTextContent")
  if (length(ptc) != 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
