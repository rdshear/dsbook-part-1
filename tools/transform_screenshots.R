# transform_screenshots
# ./temptools/transform_screenshots.R
library(tidyverse) # TODO need tidyverse?
library(rlang)
library(glue)
library(formatR)

base_dir <- "~/Projects/dsbook-part-1/R/"
target_dir <- "~/temp/"
file_name <- "getting-started.qmd"
source_location <- file.path(base_dir, file_name)
target_location <- file.path(target_dir, file_name)



#library(parsermd) TODO Q: quad-ticks?


# Transform certain screenshot file locations
# screenshots <- list.files(file.path(img_path, "windows-screenshots"))

# screenshots[30] to -> {img_path}/win/win-img-30.png
# mac_screenshots[30] to {img_path}/mac/mac-img-30.png

# Table of transformations to apply to the code
# name - short name of the transformation
# condition - the selection criterion for the node (boolean expression)
# operation - the function to be performed on the result (function)
# removeSource - if true, remove the source code
# postBlockText - TRUE => place the value of operation(e) after the code chunk

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
    # evaluate and store the path
    operation = function(e) {
      eval(e, envir = parser_env)
      # save the first definition to index the former screenshot references
      if (!("screenshot_path" %in% names(parser_env))) {
        parser_env$screenshot_path <- parser_env$img_path
      }
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
      path1 <- parser_env$screenshot_path
      e1 <- e[[2]]
      # TODO: Deal with screenshots[5:6]
      if (e1[[1]] == '[' && is.symbol(e1[[2]])) {
        vname <- as.character(e1[[2]])
        if (endsWith(as.character(vname), "screenshots")) {
        path2 <-  ifelse(startsWith(vname, "mac"), "mac", "win")
        e[[2]] <- glue("{path1}/{path2}/{path2}-img-{e1[[3]]}.png")
        
        }
      }
      u <- absolute_to_relative(eval(e[[2]], envir = as.list(parser_env)), 
                                base_dir)
      str_glue("()[{u}]")
      
    }
    ,
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

# utility function to create normalized path relative to qmd location
absolute_to_relative <- function(absolute_path, working_dir = getwd()) {
  
  # temporily go to the reference working directory for file exsitence check
  wd <- getwd()
  tryCatch({
  setwd(working_dir)
  absolute_path <- normalizePath(absolute_path)
  working_dir <- normalizePath(working_dir)
  }, finally = setwd(wd))
  
  if (startsWith(absolute_path, working_dir)) {
    return(substring(absolute_path, nchar(working_dir) + 2))
  } else {
    return(absolute_path)
  }
}

# For realized values from the parse tree available to downstream nodes
parser_env <- new.env()

src <- readLines(source_location)

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
    postChunkText = list(character()),
    codeRemoved = FALSE
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
    if (any(mask)) {
        chunks$code[i] <- str_split(tidy_source(text = deparse(as.list(target$e[!mask])),
            output = FALSE, width.cutoff = 20, args.newline = TRUE), "\\n")
    }
    
    # execute operations
    pt_ops <- lapply(target, (\(u) u$xform$operation(u$e)))
    # add post-chunk text
    pct_masks <- sapply(target, (\(u) u$xform$postBlockText))
    if (any(pct_masks)) {
      chunks$codeRemoved[i] <- TRUE
      chunks$postChunkText[i] <- pt_ops[pct_masks]
    }
  }

    # TODO remove empty chunks
    
    
    
  }
}


