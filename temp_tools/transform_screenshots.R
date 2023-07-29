# transform_screenshots
# ~/Desktop/transform_screenshots.R
library(tidyverse)


getSource <- funcion(loc) {
  loc <- "~/Projects/dsbook-part-1/R/getting-started.qmd"

  a <- data.frame(src = readLines(loc))
  fences <- str_starts(a$src, '```')
  if (sum(fences) %%2 != 0)
    error("Unbalanced chunk boundries")

x <- tibble(src = readLines(loc)) |>
  mutate(fences =  str_starts(a$src, '```'), 
          chunk_mask = cumsum(fences)%%2 != 0,
          is_code = chunk_mask & !fences,
          is_header =!lag(fences, 1, default = FALSE) & 
            fences & !lag(is_code, 1, default = FALSE),
          chunk_parameter = 
            str_split(str_split_i(
              str_split_i(src, fixed("}"), 1), 
              fixed("```{"), -1), "[,]\\s*"),
          language = chunk_parameter[[1]][1]
         )
# TODO DEBUG

  assign_in(is_header, str_split(str_split_i(
    str_split_i(a$src[a$is_header], fixed("}"), 1), fixed("```{"), -1), 
    "[,]\\s*"))
,
  a$chunk_param[a$is_header] <- str_split(str_split_i(
        str_split_i(a$src[a$is_header], fixed("}"), 1), fixed("```{"), -1), 
      "[,]\\s*")
  a$chunk_param <- if_else(a$chunk_param == '```', 
                           rep_len(list(), nrow(a)), a$chunk_param)

  
  x <- header_values[1]

    tokens <- str_split(x, pattern = "\\s+")[[1]]
  
  print(tokens)
  
}


fn <- "~/Projects/dsbook-part-1/R/getting-started.qmd"



targets <- grep("knitr::include_graphics", src, fixed = TRUE)

lno <- targets[1]
line <- src[lno]
line
t <- str2lang(line)
as.list(t)
t[1]
length(t)

for (line_num in targets) {

  line <- src[line_num]
  t <- str2lang(line)
  if (length(t) != 2) {
    browser()
  }
 
  print(t)
  print(pryr::call_tree(t))
  
}
