##' Get the Complete list of authors for a Publication
##'
##' @param id a Google Scholar ID
##' @param pubid a Publication ID from a giving google Schalar ID
##' @return a string containing the complete list of authors

##' @export
getCompleteAuthors = function(id, pubid)
{
  auths = ""
  url_template = "http://scholar.google.com/citations?view_op=view_citation&citation_for_view=%s:%s"
  url = sprintf(url_template, id, pubid)

  print("parsing html")

  url1<-xml2::read_html(url)
  auths=as.character(rvest::html_node(url1,".gsc_vcd_value") %>% rvest::html_text())
  return(auths)

}
