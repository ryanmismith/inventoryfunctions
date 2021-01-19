#' Returns Species Values Necessary As Dependencies for Other Functions
#'
#' This function returns species values which are dependencies for other
#' more complex functions. These values include whether a species is a hardwood or softwood,
#' a species standard gravity, and coefficients for shade, drought, waterlog, and wd.
#' Some of these coefficients may be valuable on their own, but they are grouped together to
#' simplify function calls for other more complex functions in this inventory package.
#'
#'@param SPP Species: use FVS species codes
#'
#'@return
#' This function returns a vector of length 5.
#' SPtype 1, : shade 2, : sg 3, : wd 4, : waterlog 5,
#'
#' @examples
#' SPP.func("BA")
#'
#'@export

#Species function
SPP.func <- function(SPP){
  SPcodes <- c(
    'AB',  # AB=American beech
    'AS',  # AS=ash
    'BA',  # BA=black ash
    'BC',  # BC=black cherry
    'BF',  # BF=balsam fir
    'BP',  # BP=balsam poplar
    'BS',  # BS=black spruce
    'BT',  # BT=bigtooth aspen
    'EC',  # EC=eastern cottonwood
    'EH',  # EH=eastern hemlock
    'GA',  # GA=green ash
    'GB',  # GB=gray birch
    'HH',  # HH=eastern hophornbeam
    'JP',  # JP=jack pine
    'NS',  # NS=Norway spruce
    'OH',  # OH=other hardwoods
    'OS',  # OS=other softwoods
    'PB',  # PB=paper birch
    'PC',  # PC=pin cherry
    'PR',  # PR=pin cherry
    'QA',  # QA=quaking aspen
    'RB',  # RB=river birch
    'RM',  # RM=red maple
    'RP',  # RP=red pine
    'RN',  # RN=red pine
    'RO',  # RO=red oak
    'RS',  # RS=red spruce
    'SB',  # SB=Sweet birch
    'SM',  # SM=sugar maple
    'ST',  # ST=striped maple
    'TA',  # TA=larch/tamarack
    'WA',  # WA=white ash
    'WC',  # WC=northern white cedar
    'WP',  # WP=white pine
    'WS',  # WS=white spruce
    'YB',  # YB=yellow birch
    '99')  # other
  SPtype <- c(
    'HW', # AB=American beech
    'HW', # AS=ash
    'HW', # BA=black ash
    'HW', # BC=black cherry
    'SW', # BF=balsam fir
    'HW', # BP=balsam poplar
    'SW', # BS=black spruce
    'HW', # BT=bigtooth aspen
    'HW', # EC=eastern cottonwood
    'SW', # EH=eastern hemlock
    'HW', # GA=green ash
    'HW', # GB=gray birch
    'HW', # HH=eastern hophornbeam
    'SW', # JP=jack pine
    'SW', # NS=Norway spruce
    'HW', # OH=other hardwoods
    'SW', # OS=other softwoods
    'SW', # PB=paper birch
    'HW', # PC=pin cherry
    'HW', # PR=pin cherry
    'HW', # QA=quaking aspen
    'HW', # RB=river birch
    'HW', # RM=red maple
    'SW', # RP=red pine
    'SW', # RN=red pine
    'HW', # RO=red oak
    'SW', # RS=red spruce
    'HW', # SB=Sweet birch
    'HW', # SM=sugar maple
    'HW', # ST=striped maple
    'SW', # TA=larch/tamarack
    'HW', # WA=white ash
    'SW', # WC=northern white cedar
    'SW', # WP=white pine
    'SW', # WS=white spruce
    'HW', # YB=yellow birch
    'HW') # other
  attrs <- matrix (c(
    # sg      wd     shade drought  waterlog
    0.64  ,0.56   , 4.75 , 1.5  , 1.5 ,    # AB=American beech
    0.57  ,0.51   , 2.84 , 2.74 , 3.02,    # AS=ash
    0.5   ,0.45   , 2.96 , 2    , 3.5 ,    # BA=black ash
    0.5   ,0.47   , 2.46 , 3.02 , 1.06,    # BC=black cherry
    0.35  ,0.33   , 5.01 , 1    , 2   ,    # BF=balsam fir
    0.34  ,0.31   , 1.27 , 1.77 , 2.63,    # BP=balsam poplar
    0.46  ,0.38   , 4.08 , 2.0  , 2.0 ,    # BS=black spruce
    0.39  ,0.36   , 1.21 , 2.5  , 2   ,    # BT=bigtooth aspen
    0.4   ,0.37   , 1.76 , 1.57 , 3.03,    # EC=eastern cottonwood
    0.4   ,0.38   , 4.83 , 1    , 1.25,    # EH=eastern hemlock
    0.56  ,0.53   , 3.11 , 3.85 , 2.98,    # GA=green ash
    0.48  ,0.45   , 1.5  , 2.34 , 1   ,    # GB=gray birch
    0.78  ,0.63   , 4.58 , 3.25 , 1.07,    # HH=eastern hophornbeam
    0.43  ,0.4    , 1.36 , 4    , 1   ,    # JP=jack pine
    0.43  ,0.37023, 4.45 , 1.75 , 1.22,    # NS=Norway spruce
    0.5121,0      , 2.29 ,  0   , 0   ,    # OH=other hardwoods
    0.445 ,0      , 2.27 , 0    , 0   ,    # OS=other softwoods
    0.55  ,0.48   , 1.54 , 2.02 , 1.25,    # PB=paper birch
    0.38  ,0.36   , 2.26 , 0    , 0   ,    # PC=pin cherry
    0.38  ,0.36   , 2.26 , 0    , 0   ,    # PR=pin cherry
    0.38  ,0.35   , 1.21 , 1.77 , 1.77,    # QA=quaking aspen
    0.62  ,0.49   , 1.45 , 1.53 , 2.85,    # RB=river birch
    0.54  ,0.49   , 3.44 , 1.84 , 3.08,    # RM=red maple
    0.46  ,0.41   , 1.89 , 3    , 1   ,    # RP=red pine
    0.46  ,0.41   , 1.89 , 3    , 1   ,    # RN=red pine
    0.63  ,0.56   , 2.75 , 2.88 , 1.12,    # RO=red oak
    0.4   ,0.37   , 4.39 , 2.5  , 2   ,    # RS=red spruce
    0.65  ,0.6    , 2.58 , 3    , 1   ,    # SB=Sweet birch
    0.63  ,0.56   , 4.76 , 2.25 , 1.09,    # SM=sugar maple
    0.46  ,0.44   , 3.56 , 2    , 1   ,    # ST=striped maple
    0.53  ,0.49   , 0.98 , 2    , 3   ,    # TA=larch/tamarack
    0.6   ,0.55   , 2.46 , 2.38 , 2.59,    # WA=white ash
    0.31  ,0.29   , 3.45 , 2.71 , 1.46,    # WC=northern white cedar
    0.35  ,0.34   , 3.21 , 2.29 , 1.03,    # WP=white pine
    0.4   ,0.33   , 4.15 , 2.88 , 1.02,    # WS=white spruce
    0.62  ,0.55   , 3.17 , 3    , 2   ,    # YB=yellow birch
    0.3   ,0.3    , 3.0  , 0    , 0   ),   # other
    ncol=5,byrow=TRUE)
  sprow = match(SPP,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)
  # SPtype [1,] : shade [2,] : sg[3,] : wd[4,] : waterlog [5,]
  return(c(SPtype=SPtype[sprow], shade=attrs[sprow,3],
           sg=attrs[sprow,1], wd = attrs[sprow,2], waterlog = attrs[sprow,5]))
}

