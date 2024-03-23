#' Title Convert the base information from P table mapped with risk rate table to Gisu
#'
#' @param base3 table with required variables of seq, 나이, fo.##x, 보험가입금액, Q1000
#' @param r interest rate
#'
#' @return Gisu table with generated variables of t, ##x such as Dx, Nx, Cx, Mx
#' @export
#'
#' @examples
#'
base2gisu  <- function(base3, r, simdo_lim) {
  library(data.table)
  library(dplyr)
  library(reshape2)
  library(fmsb)
  library(factorial2x2)

  # 기본정보
  base3[base3==""] = NA
  col_base2gisu <- base3 %>% head(1) %>% select(-contains(c("f", "Q", "q"))) %>% colnames
  # Nested Functions used inside of base2gisu function
  subfo <- function(x) {
    sub("fo.", "", x)
  }

  dda  <- function(n, r) {
    v  <- 1/(1+r)
    (1-v^n)/(1-v)
  }

  base4  <- base3 %>%
    group_by(seq) %>%
    arrange(seq, 나이) %>%
    mutate(t = row_number() - 1) %>%

    # fo.lx 산식 : qx
    mutate(across(.cols = contains("fo.l"), function(x) ifelse(is.na(x), 0, 1)*lag(cumprod(eval(parse(text = as.character(x)))), default = 1), .names = "{subfo(.col)}")) %>%

    # fo.Cx 산식 : lx, qx, 보험가입금액, v^(t+0.5)의 선형관계
    mutate(Cx = rowSums(across(.cols = contains("fo.C"), function(x) ifelse(is.na(x), 0, 1)*eval(parse(text = as.character(x)))*(1+r)^(-t-0.5), .names = "{subfo(.col)}"), na.rm=T)) %>%
    mutate(Cx = ifelse(Cx > simdo_lim*(1+r)^(-t-0.5) , Cx, Cx*보험가입금액)) %>%

    mutate(Dx = lx*(1+r)^(-t)) %>%
    mutate(DDx = llx*(1+r)^(-t)) %>%
    mutate(Nx = rev(cumsum(rev(Dx))), .after = DDx) %>%
    mutate(NNx = rev(cumsum(rev(DDx))), .after = Nx) %>%
    mutate(Mx = rev(cumsum(rev(Cx)))) %>%

    ################### 정기보험기수 (정기사망률 Q1000) ########################
  mutate(lxr = lag(cumprod((1- Q1000)), default = 1), .after = Mx) %>%
    mutate(Dxr = lxr*(1+r)^(-t), .after = lxr) %>%
    mutate(Nxr = rev(cumsum(rev(Dxr))), .after = Dxr) %>%
    mutate(Cxr = Q1000*보험가입금액*lxr*(1+r)^(-t-0.5), .after = Nxr) %>%
    mutate(Mxr = rev(cumsum(rev(Cxr))), .after = Cxr) %>%
    ############################################################################

 # arrange(성별구분, 차량용도, 보상한도) %>%
    group_by(seq) %>%
    select(seq, all_of(col_base2gisu), any_of(c("t")), starts_with(c("D", "N", "C", "M"))) # 사업비에 걸리는 구분자 및 기수표
  #  select(seq, 코드, 단일률구분, 급수, 차량용도, 유형, 보험기간, 납입기간, 급수, 성별구분, t, 나이, 만기종류, 보상한도, 기타코드, 연세만기.구분, 보험가입금액, starts_with(c("l", "D", "N", "C", "M"))) # 사업비에 걸리는 구분자 및 기수표

  return(base4)
}

# 기수 %>% select(차량용도) %>% unique
# 기수 %>% filter(코드 == 40173) %>% head
