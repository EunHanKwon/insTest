#' Title
#'
#' @param PT
#'
#' @return exp table
#' @export
#'
#' @examples pt2exp(PT)
pt2exp <- function(PT) {

  PT %>% filter(코드 %in% cov) %>% mutate(seq = row_number()) %>% select(seq, contains("exp_")) %>% rename_with(~stringr::str_remove(., "exp_"))

}

