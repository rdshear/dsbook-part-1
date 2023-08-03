# transform_screenshots
# ./temptools/transform_screenshots.R
library(tidyverse)



walkCode <- function (e)
{
  if (typeof(e) == "language" && mode(e) == "call") {
    
    # Transform kniter::include_graphics() calls
    if (identical(e[[1]], quote(knitr::include_graphics))) {
      return(e)
    }
    
    # capture assigments to variable named "img_path"
    if (mode(e[[1]]) == "name" && e[[1]] == "<-" && mode(e[[2]]) == "name" &&
      e[[2]] == "img_path") {
        return(e)
    }

    # No special intervention required, continue processing the parse tree
    if (!(typeof(e[[1]]) %in% c("symbol", "character"))) {
      for (ee in as.list(e)) {
        if (!missing(ee))
          walkCode(ee)
      }
    }
  }
  return(NULL)
}

loc <- "~/Projects/dsbook-part-1/R/getting-started.qmd"

target_env <- new.env()

src <- readLines(loc)

# ignore quadtick chunks
quadticks <- cumsum(str_starts(src, '````')) %% 2 == 1
quadticks <- !(quadticks | lag(quadticks, default = FALSE))
if (!tail(quadticks, 1)) {
  stop("Unbalanced quadticks (````)")
}

fences <-  which(str_starts(src, '```') & quadticks)
if (length(fences) %% 2 != 0) {
  stop("Unbalanced chunk fences")
}
chunks <- matrix(fences, ncol = 2, byrow = TRUE)
colnames(chunks) <- c("start", "end")

chunks <- as_tibble(chunks) |>
  mutate(
    header = str_split(str_split_i(
      str_split_i(substring(src[start], 4), fixed("}"), 1),
      fixed("{"),-1
    ), "[,]\\s*"),
    lan =  sapply(header, (\(u) {
      result <- u[[1]][1]
      if (result == "")
        result = "r"
      result
    })),
    params = sapply(header, (\(u) u[-1]))
  )

apply(chunks[chunks$lan == "r", ], 1, (\(df) {
  code <- src[(df$start + 1):(df$end - 1)]
  cat(sprintf("Parsing lines %d-%d\n", df$start, df$end))
  
  ptree <- tryCatch(
    parse(text = code, keep.source = TRUE),
    error = function(e) {
      print(e)
      return(NULL)
    }
  )
  if (is.null(ptree)) {
    cat(sprintf("Failure in lines %d-%d\n", df$start, df$end))
  }
  else {
    target <- lapply(ptree, (\(u) walkCode(u)))
    print(target)
    cat(rlang::expr_deparse(ptree))
    browser()
  }
}))
