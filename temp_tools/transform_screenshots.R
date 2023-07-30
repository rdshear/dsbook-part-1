# transform_screenshots
# ./temptools/transform_screenshots.R
library(tidyverse)
library(codetools)

cw <- codetools::makeCodeWalker(call = function (e, w) {
    result <- NULL
    if (mode(e) == "call" && identical(e[[1]], target_call)) {
      result <- deparse(e)
    }
    for (ee in as.list(e)) {
      if (!missing(ee)) 
        walkCode(ee, w)
    }
    return(result)
  },
  leaf = function(e, w) NULL)


loc <- "~/Projects/dsbook-part-1/R/getting-started.qmd"

src <- readLines(loc)
fences <-  which(str_starts(src, '```'))
if (length(fences) %% 2 != 0) {
  stop("Unbalanced chunk fences")
}
chunks <- matrix(fences, ncol = 2, byrow = TRUE)
colnames(chunks) <- c("start", "end")

chunks <- as_tibble(chunks) |>
   mutate(header = str_split(str_split_i(
              str_split_i(substring(src[start], 4), fixed("}"), 1), 
              fixed("{"), -1), "[,]\\s*"),
          lan =  sapply(header, (\(u) {
              result <- u[[1]][1]
              if (result == "")
                result = "r"
              result
              })),
          params = sapply(header, (\(u) u[-1]))
          )

target_call = quote(knitr::include_graphics)

apply(chunks[chunks$lan == "r",], 1, (\(df) {
  code <- src[(df$start + 1):(df$end - 1)]
  cat(sprintf("Parsing lines %d-%d\n", df$start, df$end))
  ptree <- tryCatch(parse(text = code, keep.source = TRUE),
                error = function(e) {
                  print(e)
                  return(NULL)
                  }
        )
  if (is.null(ptree)) {
      cat(sprintf("Failure in lines %d-%d\n", df$start, df$end))
  } else
  {
    walk_result <- codetools::walkCode(ptree[[1]], cw)
    if (!is.null(walk_result)) {
      # TODO: NEW STUFF GOES HERE
      # browser()
      cat("Walk:",walk_result, "\n")
    }
  }
  # cat(sprintf("----%d,%d ... %s\n", df$start, df$end, df$params))
  # cat(print(lines))
}))
