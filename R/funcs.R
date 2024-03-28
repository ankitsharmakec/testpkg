


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a PARAM_DESCRIPTION
#' @param b PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname multiply
#' @export

multiply <- function(a, b) {
  return(a * b)
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a PARAM_DESCRIPTION
#' @param b PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname subtract
#' @export

subtract <- function(a, b) {
  return(a - b)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a PARAM_DESCRIPTION
#' @param b PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add
#' @export

add <- function(a, b) {
  return(a + b)
}

library(dplyr)

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname summary_stats
#' @export
#' @import dplyr
#' @importFrom stats median

summary_stats <- function(data, column) {
  stats <- data %>%
    summarise(
      mean = mean({{ column }}),
      median = median({{ column }}),
      min = min({{ column }}),
      max = max({{ column }})
    )
  return(stats)
}


library(stringr)

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param text PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname count_words
#' @export
#' @import stringr

count_words <- function(text) {
  words <- str_split(text, "\\s+")
  return(length(words[[1]]))
}
