#' Title Convert/calculate Premiums of Gisu-table based on informations called base
#'
#' @param 기수표 Gisu-table
#' @param exp expense table
#'
#' @return Premiums
#' @export
#'
#' @examples
gisu2prem   <- function(기수표, exp) {

  col_gisu2prem <- 기수표 %>% data.frame() %>% head(1) %>% select(seq:t, -seq, -t) %>% colnames()
  #col_gisu2prem <- 기수표 %>% head(1) %>% select(-contains(c("f", "Q", "q", "D", "N", "C", "M", "t", "나이"))) %>% colnames

  기수표 %>%
    #       filter(seq %in% c(1)) %>%
    group_by(seq) %>%
    summarise(

      across(col_gisu2prem, first),

      # 기준 연납순보험료 구송요소
      #            담보명 = first(담보명),
      #           유형 = first(유형),
      #          단일률구분 = min(단일률구분),
      #         납입기간 = min(납입기간),
      #        보상한도 = min(보상한도),
      #       보험기간 = min(보험기간),
      MxMx_s = list(Mx[t==0] - Mx[t==보험기간]),
      NNxNNx_s =list(NNx[t==0] - NNx[t==pmin(보험기간,20)]),

      # 정기월납순보험료 구성요소 #
      #            코드 = min(코드),
      #           급수 = min(급수),
      #          차량용도 = min(차량용도),
      #         성별구분 = min(성별구분),
      #        나이 = min(나이),
      #       만기종류 = min(만기종류),
      #      기타코드 = min(기타코드),
      #     연세만기구분 = min(연세만기.구분),
      보험가입금액 = min(보험가입금액),
      DxDx_r = list(Dxr[t==0] - Dxr[t==납입기간]),
      MxMx_r = list(Mxr[t==0] - Mxr[t==보험기간]),
      NxNx_r =list(Nxr[t==0] - Nxr[t==납입기간]),

      # FPI (준비금 구송요소)
      FPI = list(Nx[t==납입기간] - Nx[t==보험기간]),
      FPII = list(NNx[t==납입기간] - NNx[t==보험기간]),

      # 순보험료 구성요소

      NxNx = list(Nx[t==납입기간] - Nx[t==납입기간]),
      MxMx =  list(Mx[t==0] - Mx[t==보험기간]),
      DxDx = list(Dx[t==0] - Dx[t==보험기간]),
      DDxDDx = list(DDx[t==0] - DDx[t==납입기간]),
      NNxNNx = list(NNx[t==0] - NNx[t==납입기간]),
      Dx = list(Dx[t==0]),

      # beta 순보험료 구성요소
      NNxNNx_납후 = list(NNx[t==납입기간] - NNx[t==보험기간]),
    ) %>%
    merge(exp) %>%

    rowwise() %>%
    mutate(
      # 선형근사
      NNxNNx_연납 = (NNxNNx - (1-1)/(2*1)*DDxDDx),
      NNxNNx_6월납 = (NNxNNx - (2-1)/(2*2)*DDxDDx),
      NNxNNx_3월납 = (NNxNNx - (4-1)/(2*4)*DDxDDx),
      NNxNNx_2월납 = (NNxNNx - (6-1)/(2*6)*DDxDDx),
      NNxNNx_월납 = (NNxNNx - (12-1)/(2*12)*DDxDDx),

      # 보험료
      ## 기준연납순보험료
      기준연납순보험료 = as.character(ifelse(납입기간 == 0, 0, MxMx_s/NNxNNx_s)),

      ## 순보험료
      순보험료_연납 = ifelse(납입기간 == 0, MxMx/Dx, MxMx/NNxNNx_연납/1),
      순보험료_6월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_6월납/2),
      순보험료_3월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_3월납/4),
      순보험료_2월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_2월납/6),
      순보험료_월납 = ifelse(납입기간 == 0, 0, MxMx/NNxNNx_월납/12),

      ## beta 순보험료
      cePrime = tryCatch(cePrime,error = function(z) return(0)),
      betaPrime = tryCatch(betaPrime,error = function(z) return(0)),
      ce = tryCatch(ce,error = function(z) return(0)),
      alpha2 = tryCatch(alpha2,error = function(z) return(0)),
      gamma = tryCatch(gamma,error = function(z) return(0)),
      alpha1 = tryCatch(alpha1,error = function(z) return(0)),
      alphaPrime = tryCatch(alphaPrime,error = function(z) return(0)),
      beta1 = tryCatch(beta1,error = function(z) return(0)),

      베타순보험료_연납 = ifelse(납입기간 == 0, 순보험료_연납, 순보험료_연납 + (cePrime + alphaPrime + betaPrime)*NNxNNx_납후/NNxNNx_연납),
      베타순보험료_6월납 = ifelse(납입기간 == 0, 0, 순보험료_6월납 + (cePrime + alphaPrime + betaPrime)*NNxNNx_납후/NNxNNx_6월납),
      베타순보험료_3월납 = ifelse(납입기간 == 0, 0, 순보험료_3월납 + (cePrime + alphaPrime + betaPrime)*NNxNNx_납후/NNxNNx_3월납),
      베타순보험료_2월납 = ifelse(납입기간 == 0, 0, 순보험료_2월납 + (cePrime + alphaPrime + betaPrime)*NNxNNx_납후/NNxNNx_2월납),
      베타순보험료_월납 = ifelse(납입기간 == 0, 0, 순보험료_월납 + (cePrime + alphaPrime + betaPrime)*NNxNNx_납후/NNxNNx_월납),

      # 영업보험료
      영업보험료_연납 = ifelse(납입기간 == 0, 순보험료_연납/(1-alpha2-beta1-ce), 순보험료_연납/(1-beta1-ce-gamma -alpha2*Dx/NNxNNx_연납)),
      영업보험료_6월납 = 순보험료_6월납/(1-beta1-ce-gamma -alpha2*Dx/NNxNNx_6월납),
      영업보험료_3월납 = 순보험료_3월납/(1-beta1-ce-gamma -alpha2*Dx/NNxNNx_3월납),
      영업보험료_2월납 = 순보험료_2월납/(1-beta1-ce-gamma -alpha2*Dx/NNxNNx_2월납),
      영업보험료_월납 = 순보험료_월납/(1-beta1-ce-gamma -alpha2*Dx/NNxNNx_월납),

      # 정기월납순보험료 (정기사망)
      ## 선형근사
      NxNx_r = (NxNx_r - (12-1)/(2*12)*DxDx_r),
      ## 순보험료
      정기월납순보험료 = as.character(ifelse(납입기간 == 0, MxMx_r/Dxr, MxMx_r/NxNx_r/12)),
      ## S값
      S값A = as.character(순보험료_월납/as.numeric(정기월납순보험료)),

      # 표준해약공제액
      ## 단일률
      표준해약공제액_단일률 = as.character(factorial2x2::roundDown(0.05*as.numeric(기준연납순보험료)*pmin(보험기간, 20) + 0.45*as.numeric(기준연납순보험료), 2))

    ) %>%
    select(seq, all_of(col_gisu2prem), contains("보험료"), S값A, 표준해약공제액_단일률) %>%
    #select(seq, 코드, 단일률구분, 급수, 차량용도, 성별구분, 만기종류, 보험기간, 납입기간, 기타코드, 보상한도, 연세만기구분, 보험가입금액, 나이, contains("보험료"), S값A, 표준해약공제액_단일률) %>%
    select(-contains("베타")) %>%
    mutate_if(is.numeric, round) %>%
    mutate(across(c(where(is.character), -contains("유형")), as.numeric))
  #mutate(across(contains("보험료"), as.numeric)) %>% # 정기보험료 및 기준연납순보험료 숫자화
  # mutate(across(contains("S값"), as.numeric)) %>% # S값 숫자화
  # mutate(across(contains("표준해약공제액"), as.numeric)) # 표준해약공제액 숫자화
  # mutate(across(contains("표준해약공제액"), as.numeric)) # 표준해약공제액 숫자화
}
