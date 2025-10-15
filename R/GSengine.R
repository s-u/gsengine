GSengine = function(options = NULL, 
                    host = Sys.getenv("GENSTAT_HOST", "localhost"),
                    port = as.integer(Sys.getenv("GENSTAT_PORT", "8085")),
                    mode = c("text", "html")){
  connection = gsio.connect(host = host, port = port)
  if(!is.null(options)){
    code = options$code
    output = gsio.recv(code)
    knitr::engine_output(options, code, output)
  } 
}