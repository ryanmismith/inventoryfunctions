#' Largest Crown Width
#'
#' This function returns an estimate for the widest crown diameter or radius for a stand grown tree.
#'
#' ## Metric
#' This function uses metric units.
#'
#' @details SPP included: BF, BS, EH, WP, NC, RS, WS, AB, GB, RB, RO, PB, QA, RM, SM, YB.
#' SPP that are not included can be enter as 'OH' for hardwoods and 'OS' for softwoods.
#' If no SPP is entered a standard coefficient will be used but may not reflect the
#' crown conditions accurately - please enter 'OS' or 'OH' if species is not included.
#'
#' @details Species should be entered as a character vector.
#'
#' @details MCW value needed to calculate LCW, please refer to MCW function in see also.
#'
#' @family Crown Functions
#' @seealso [inventoryfunctions::MCW]
#'
#' @param SPP Tree Species: use the FVS code
#' @param DBH Diameter at breast height in cm.
#' @param MCW Maximum Crown Width in meters.
#'
#' @return Numeric. Widest crown diameter or radius for a stand grown tree in meters.
#'
#' @examples
#'
#' LCW("BF", 8.63, 25)
#' # Tibble %>% mutate(LCW = LCW("SPP Variable", "MCW Variable", "DBH Variable"))
#'
#' @export


# These are the parm ests for estimating largest crown width (lcw)
LCW <- function(SPP, MCW, DBH){
  SPcodes <- c('BF','BS','EH','WP','NC','RS','WS','AB','GB','RB','RO','PB','QA',
            'RM','SM','YB','OH','OS','99')
  coefs <- matrix(c(
    # b1           b2
    1.49   , 0.105       ,   # BF
    1      , 0.174       ,   # BS
    1.90   , -0.057      ,   # EH
    1      , 0.147       ,   # WP
    2.19   , -0.080      ,   # NC
    4.33   , -0.264      ,   # RS
    2.09   , -0.069      ,   # WS
    1      , 0.194       ,   # AB
    3.10   , -0.214      ,   # GB
    3.10   , -0.214      ,   # RB
    4.10   , -0.272      ,   # RO
    2.10   , -0.035      ,   # PB
    2.65   , 0.157       ,   # QA
    2.63   , -0.132      ,   # RM
    1      , 0.161       ,   # SM
    4.23   , -0.264      ,   # YB
    2.65   , 0.157       ,   # OH
    2.3276 , 0.027842857 ,   # OS
    2.79282, -0.090113333),  # 99
    ncol=2,byrow=TRUE)
  sprow <-  match(SPP,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)
  X <- MCW/(coefs[sprow,1]*DBH**coefs[sprow,2])
  return(X)
}


