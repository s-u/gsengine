#' @importFrom htmltools htmlDependency
html_dependency_gsengine = function() {
  htmltools::htmlDependency(
    name = "gsengine-styles",
    version = as.character(utils::packageVersion("gsengine")),
    src = list(
      file = system.file("resources", package = "gsengine"),  
      href = "resources"                                      
    ),
    stylesheet = "Genstat.css"
  )
}
