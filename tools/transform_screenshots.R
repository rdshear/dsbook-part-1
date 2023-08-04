# transform_screenshots
# ./temptools/transform_screenshots.R
library(tidyverse)
library(rlang)
#library(parsermd)

## TODO parsermd...Q: quad-ticks?

# Table of transformations to apply to the code
# name - short name of the transformation
# condition - the selection criterion for the node (boolean expression)
# operation - the function to be performed on the result (function)
# removeSource - if true, remove the source code
# postBlockText - TRUE => place the result after the code chunk

xformTabEntry <- function(condition = function(e) FALSE,
  operation = function(e) "",
  removeSource = FALSE,
  postBlockText = FALSE
)
{
  list(condition = condition, 
       operation = operation, 
       removeSource = removeSource, 
       postBlockText = postBlockText)
}

xformTab <- list(
  # capture assigments to variable named "img_path"
  getImgPath = xformTabEntry(
    condition = function(e) {
      mode(e[[1]]) == "name" && e[[1]] == "<-" && 
        mode(e[[2]]) == "name" && e[[2]] == "img_path"
    },
    # TODO reduce the image path and store
    operation = function(e) {
      "img_path+TODO PUSH TO environment"
    },
    removeSource = TRUE,
    postBlockText = FALSE
  ),
  
  # Transform kniter::include_graphics() calls
  renderImage = xformTabEntry(
    condition = function(e) {
      identical(e[[1]], quote(knitr::include_graphics))
    },
    operation = function(e) {
      "()[location]" #TODO make it real
    },
    removeSource = TRUE,
    postBlockText = TRUE
  ),
  
  default = xformTabEntry()
)


walkCode <- function (e)
{
  if (typeof(e) == "language" && mode(e) == "call") {
    for (xform in xformTab) {
      if (xform$condition(e))
        return(list(xform = xform, e = e))
    }
    # No special intervention required, continue processing the parse tree
    if (!(typeof(e[[1]]) %in% c("symbol", "character"))) {
      for (ee in as.list(e)) {
        if (!missing(ee))
          walkCode(ee)
      }
    }
  }
  return(list(xform = xformTab$default, e = e))
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
    params = sapply(header, (\(u) u[-1])),
    postChunkText = list(character())
  )

chunks$code = mapply((\(s, e) src[s:e]), chunks$start + 1, chunks$end - 1)

# TODO process chunk parameters

for (i in seq(nrow(chunks))) {
  if (chunks$lan[i] == "r") {
    ptree <- tryCatch(
    parse(text = chunks$code[i][[1]], keep.source = TRUE),
    error = function(e) {
      print(e)
      return(NULL)
    }
  )
  if (is.null(ptree)) {
    cat(sprintf("Failure in lines %d-%d\n", chunks$start[i], chunks$end[i]))
  }
  else {
    target <- lapply(ptree, (\(u) walkCode(u)))
    # remove expressions if necessary
    mask <- sapply(target, (\(u) u$xform$removeSource))
    print(mask)
    if (any(mask)) {
        chunks$code[i] <- list(deparse(as.list(target[!mask])))
    }
    
    # add post-chunk text
    chunks$postChunkText[i] <- list(lapply(target, (\(u) ifelse(u$xform$postBlockText, 
                                  u$xform$operation(u$e), ""))))
  }

    # TODO remove empty chunks
    
    
  }
}

