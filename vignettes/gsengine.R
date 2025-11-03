## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{r}
# knitr::knit_engines$set(gs = gsengine::gs.engine())
# ```

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{gs}
# ```

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{r}
# Sys.setenv(GENSTAT_HOST = "sc389508.UoA.auckland.ac.nz")
# Sys.setenv(GENSTAT_PORT = "8085")
# knitr::knit_engines$set(gs = gsengine::gs.engine())
# ```

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{r tidy=FALSE}
# knitr::knit_engines$set(gs = gsengine::gs.engine(host = "sc389508.UoA.auckland.ac.nz",
#                                                  port = 8085))
# ```

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{gs}
# ```

## ----tidy=FALSE, echo=FALSE---------------------------------------------------
knitr::knit_engines$set(gs = gsengine::gs.engine(host = "sc389508.UoA.auckland.ac.nz"))

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{gs, eval=FALSE}
# ```

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{gs, eval=FALSE}
# PRINT 'This should not run.'
# ```

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{gs, echo=FALSE}
# ```

## ----eval=FALSE, highlight=FALSE----------------------------------------------
# ```{gs, echo=FALSE}
# PRINT 'This should run.'
# ```

## -----------------------------------------------------------------------------
sessionInfo()

