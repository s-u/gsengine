#' Genstat Socket Engine for knitr
#'
#' This function creates a custom knitr engine for executing Genstat code by connecting 
#' to a Genstat server over a socket. The engine sends Genstat code, receives output, 
#' and renders it in R Markdown documents with support for formatted tables, warnings, 
#' and embedded plots.
#' 
#' @param host A character string specifying the host name or IP address of the Genstat server. 
#'             Defaults to `"localhost"` or the environment variable `GENSTAT_HOST`.
#' @param port An integer specifying the port number to connect to on the Genstat server. 
#'             Defaults to `8085` or the environment variable `GENSTAT_PORT`.
#'
#' @return A function that can be registered as a knitr engine (e.g., via `knitr::knit_engines$set(gs = gs.engine())`)
#'
#' @examples
#' \dontrun{
#' knitr::knit_engines$set(gs = gs.engine())
#' }
#' @importFrom base64enc base64decode
#' @export
#'
gs.engine <- function (host = Sys.getenv("GENSTAT_HOST", "localhost"),
                       port = as.integer(Sys.getenv("GENSTAT_PORT", "8085"))) {
  local({
    # Establish a socket connection to the Genstat server
    connection <- socketConnection(host = host, port = port, blocking = TRUE, open = "r+")
    
    img_counter <- 0  # Counter for images received from Genstat
    complex_tables_list <- list()
    simple_tables_list <- list()
    complex_tables_all_chunks <- list()
    simple_tables_all_chunks <- list()
    
    make_red <- function(...) {
      text <- paste(...)
      if (knitr::is_html_output()) {
        paste0("<span style='color:red;'>", text, "</span>")
      } else {
        paste0("\\textcolor{red}{", text, "}")
      }
    }
    
    # Function to process output, highlighting warnings and formatting tables
    highlight_warning_and_tables <- function(output_lines, store_complex_tables = FALSE, store_simple_tables = FALSE) {
      oli <- 1
      nextLine <- function() {
        if (oli > length(output_lines)) return(NULL)
        res <- output_lines[oli]
        oli <<- oli + 1
        res
      }
      
      final_output <- character()
      
      output <- function(...) {
        final_output <<- c(final_output, ...)
      }
      
      make_table_md <- function(header, df) {
        md <- c(
          paste("|", paste(header, collapse = " | "), "|"),
          paste0("|", paste(rep("------", length(header)), collapse = "|"), "|"),
          apply(df, 1, function(x) paste("|", paste(x, collapse = " | "), "|"))
        )
        return(md)
      }
      
      l <- nextLine()
      in_warning <- FALSE
      empty_line_count <- 0
      in_simple_table <- FALSE
      simple_table_rows <- character()
      
      while (!is.null(l)) {
        ## --- Section Break ---
        if (grepl("^\\d+\\.*\\.*$", l)) {
          output("\n---\n")
          l <- nextLine()
          next
        }
        
        ## --- Major Heading ====
        if (grepl("^=+$", l)) {
          prev_line <- final_output[length(final_output)]
          final_output <- final_output[-length(final_output)]
          output(paste0("\n**", prev_line, "**\n"))
          l <- nextLine()
          next
        }
        
        ## --- Minor Heading ----
        if (grepl("^-+$", l)) {
          prev_line <- final_output[length(final_output)]
          final_output <- final_output[-length(final_output)]
          output(paste0("\n_*", prev_line, "*_\n"))
          l <- nextLine()
          next
        }
        
        ## --- Warnings ---
        if (in_warning || grepl("^\\*+ (Warning|Fault)", l)) {
          if (!in_warning) {
            in_warning <- TRUE
            empty_line_count <- 0
          }
          
          if (nzchar(l)) {
            empty_line_count <- 0
            output(make_red(l))
          } else {
            empty_line_count <- empty_line_count + 1
            if (empty_line_count >= 2) in_warning <- FALSE
          }
          
          l <- nextLine()
          next
        }
        
        ## --- Complex Tables ---
        if (grepl("^Sheet (Title|Type):", l)) {
          sheet_metadata <- l
          while (length(sheet_metadata) < 4) {
            l <- nextLine()
            if (is.null(l)) break
            sheet_metadata <- c(sheet_metadata, l)
          }
          
          ## NOTE: This seems very arbitrary. Ask David whether there will always
          ## be 4 rows for the metadata
          l <- nextLine()
          header <- unlist(strsplit(trimws(l, which = "both"), split = "\\s+"))
          
          table_rows <- character()
          l <- nextLine()
          while (!is.null(l) && grepl("^\\s*\\d+\\s+(factor|variate)", l)) {
            table_rows <- c(table_rows, l)
            l <- nextLine()
          }
          
          df <- do.call(rbind, lapply(table_rows, function(row) {
            parts <- unlist(strsplit(trimws(row), "\\s+"))
            data.frame(Index = parts[1], Type = parts[2], Nval = parts[3], Name = paste(parts[-(1:3)], collapse = " "))
          }))
          
          if (store_complex_tables) {
            complex_tables_list[[length(complex_tables_list) + 1]] <- df
          }
          
          output(sheet_metadata, "", make_table_md(header, df), "")
          next
        }
        
        ## --- Simple Tables ---
        if (in_simple_table || grepl("^\\s*\\w+\\s+[0-9]+\\.?[0-9]*\\s+[a-z]+$", l)) {
          if (!in_simple_table) {
            in_simple_table <- TRUE
            simple_table_rows <- character()
          }
          
          if (nzchar(l) && grepl("^\\s*\\w+\\s+[0-9]+\\.?[0-9]*\\s+[a-z]+$", l)) {
            simple_table_rows <- c(simple_table_rows, l)
            l <- nextLine()
            next
          } else {
            # Simple table ends
            header <- c("Item", "Value", "Rank")
            df <- do.call(rbind, lapply(simple_table_rows, function(row) {
              parts <- unlist(strsplit(trimws(row), "\\s+"))
              data.frame(Item = parts[1], Value = parts[2], Rank = parts[3])
            }))
            if (store_simple_tables) {
              simple_tables_list[[length(simple_tables_list) + 1]] <- df
            }
            output(make_table_md(header, df), "")
            in_simple_table <- FALSE
            simple_table_rows <- character()
            next
          }
        }
        
        # Regular lines
        output(l)
        l <- nextLine()
      }
      
      return(list(
        output = paste(final_output, collapse = "\n"),
        complex = complex_tables_list,
        simple = simple_tables_list
      ))
    }
    
    # Function to process Genstat code execution
    function(options) {
      response <- character()
      user_input <- options$code
      flush(connection)
      
      store_complex_tables <- options$store_complex_tables %||% FALSE
      store_simple_tables <- options$store_simple_tables %||% FALSE
      
      # Handle case where evaluation is disabled
      if (options$eval == FALSE) {
        if (options$echo == TRUE) {
          response <- c(response, "```gs", user_input, "```")  
        }
        return(paste(response, collapse="\n"))
      }
      
      # Append code block to response
      if (options$echo == TRUE) {
        response <- c(response, "```gs", user_input, "```")
      }
      
      writeLines(user_input, connection)
      while (TRUE) {
        line <- readLines(connection, n = 1)
        if (length(line) == 0 || line == "") break  
        if (grepl("^#\\s+STAT", line)) {
          break
        }
        if (grepl("^#\\s+\\S+\\s+\\S+\\s+\\d+$", line)) {
          header_parts <- strsplit(line, " ")[[1]]
          command <- header_parts[2]
          payload_type <- header_parts[3]
          num_lines <- as.numeric(header_parts[4])
          
          if (command == "OUT") {
            output <- readLines(connection, n = num_lines)
            res <- highlight_warning_and_tables(output, store_complex_tables, store_simple_tables)
            response <- c(response, res$output)
            
            # Storage tables for each chunk
            if (store_complex_tables) {
              if (!exists("complex_tables_all_chunks", envir = .GlobalEnv)) {
                assign("complex_tables_all_chunks", list(), envir = .GlobalEnv)
              }
              complex_tables_all_chunks <- get("complex_tables_all_chunks", envir = .GlobalEnv)
              complex_tables_all_chunks[[length(complex_tables_all_chunks) + 1]] <- res$complex
              assign("complex_tables_all_chunks", complex_tables_all_chunks, envir = .GlobalEnv)
            }
            if (store_simple_tables) {
              if (!exists("simple_tables_all_chunks", envir = .GlobalEnv)) {
                assign("simple_tables_all_chunks", list(), envir = .GlobalEnv)
              }
              simple_tables_all_chunks <- get("simple_tables_all_chunks", envir = .GlobalEnv)
              simple_tables_all_chunks[[length(simple_tables_all_chunks) + 1]] <- res$simple
              assign("simple_tables_all_chunks", simple_tables_all_chunks, envir = .GlobalEnv)
            }
          } else if (command == "GRAPH" && payload_type == "PNG") { 
            img_data <- readLines(connection, n = num_lines)
            img_counter <<- img_counter + 1  
            img_filename <- paste0("img_", img_counter, ".png")
            response <- c(response, paste0("![](", img_filename, ")\n"))
            writeBin(base64decode(img_data), img_filename)  
          } else {
            readLines(connection, n = num_lines)  
          }
        }
      }
      
      flush(connection)
      paste(response, collapse="\n")
    }
  })
}
