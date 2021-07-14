#' Value Estimate
#'
#' This function estimates the Sawlog and Pulp values of each sample tree based
#' on the estimated sawlog BF and pulp volume. Value estimates are based on
#' Average Maine 2019 Statewide stumpage prices.
#'
#'
#' @details
#' Each tree is given a value based on the average price paid per MBF or ib for pulp.
#' If no value is reported either in the statewide report
#' the product will be deemed non-commercial and given a value of $0.
#'
#' It is recommended that Stand and Plot be factors and each plot must have a unique ID
#' and not be a recycled number (see details section of the HeightPredict function).
#'
#'@examples
#'
#'ValueEstimate(1, 1, 1, "SM", 382.48, 1.19)
#'
#' @return Returns a list of Sawlog and Pulp values for each sample tree.
#' @author Ryan Smith
#'
#' @seealso [inventoryfunctions::HeightPredict]
#' @family Merchandising Functions
#'
#' @param Stand Stand ID for Plot where the nth tree is located.
#' @param Plot Plot ID for Plot where the nth tree is located.
#' @param Tree The ID for the tree.
#' @param SPP Tree Species using FVS SPP codes.
#' @param BoardFeet Etimated Sawlog Boardfeet for the tree.
#' @param PulpVol The estimated volume as pulp in cubic meters
#' @export

ValueEstimate <- function(Stand, Plot, Tree, SPP, BoardFeet, PulpVol){

   Stand <- as.factor(Stand)
   Plot <- as.factor(Plot)


   SPcodes <- c(
      'AB',  # AB=American beech
      'AE',  # AE=American Elm
      'AS',  # AS=ash
      'BA',  # BA=black ash
      'BC',  # BC=black cherry
      'BF',  # BF=balsam fir
      'BO',  # BO=Black Oak
      'BP',  # BP=balsam poplar
      'BS',  # BS=black spruce
      'BT',  # BT=bigtooth aspen
      'BW',  # BW=Basswood
      'EC',  # EC=eastern cottonwood
      'EH',  # EH=eastern hemlock
      'GA',  # GA=green ash
      'GB',  # GB=gray birch
      'HH',  # HH=eastern hophornbeam
      'JP',  # JP=jack pine
      'NS',  # NS=Norway spruce
      'OC',  # OC=Other Cedar
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
      'SH',  # SH=Shagbark Hickory
      'SM',  # SM=sugar maple
      'ST',  # ST=striped maple
      'TA',  # TA=larch/tamarack
      'WA',  # WA=white ash
      'WC',  # WC=northern white cedar
      'WO',  # WO=White Oak
      'WP',  # WP=white pine
      'WS',  # WS=white spruce
      'YB',  # YB=yellow birch
      '99')  # other
   SPCommonName <- c(
      'American beech', # AB=American beech
      'American Elm',  # AE=American Elm
      'Ash Species', # AS=ash
      'Black Ash', # BA=black ash
      'Black Cherry', # BC=black cherry
      'Balsam Fir', # BF=balsam fir
      'Black Oak',  # BO=Black Oak
      'Balsam Poplar', # BP=balsam poplar
      'Black Spruce', # BS=black spruce
      'Bigtooth Aspen', # BT=bigtooth aspen
      'Basswood',  # BW=Basswood
      'Eastern Cottonwood', # EC=eastern cottonwood
      'Eastern Hemlock', # EH=eastern hemlock
      'Green Ash', # GA=green ash
      'Gray Birch', # GB=gray birch
      'Eastern Hophornbeam', # HH=eastern hophornbeam
      'Jack Pine', # JP=jack pine
      'Norway Spruce', # NS=Norway spruce
      'Other Cedar',  # OC=Other Cedar
      'Other Hardwoods', # OH=other hardwoods
      'Other Softwoods', # OS=other softwoods
      'Paper Birch', # PB=paper birch
      'Pin Cherry', # PC=pin cherry
      'Pin Cherry', # PR=pin cherry
      'Quaking Aspen', # QA=quaking aspen
      'River Birch', # RB=river birch
      'Red Maple', # RM=red maple
      'Red Pine', # RP=red pine
      'Red Pine', # RN=red pine
      'Red Oak', # RO=red oak
      'Red Spruce', # RS=red spruce
      'Sweet Birch', # SB=Sweet birch
      'Shagbark Hickory',  # SH=Shagbark Hickory
      'Sugar Maple', # SM=sugar maple
      'Striped Maple', # ST=striped maple
      'Tamarack', # TA=larch/tamarack
      'White Ash', # WA=white ash
      'Northern White Cedar', # WC=northern white cedar
      'White Oak',  # WO=White Oak
      'White Pine', # WP=white pine
      'White Spruce', # WS=white spruce
      'Yellow Birch', # YB=yellow birch
      'Other') # other
   attrs <- matrix (c(  # Weight from Miles and Smith, Values from 2019 Maine Stumpage Report
      # Genus      # Simple Name  #HW/SW  #Saw$  #Pulp$ #Wt(kg/m3)
      'Fagus',       'Beech',       'HW',   133,    5,   865,     # AB=American beech
      'Ulmus',       'Elm',         'HW',    0,     0,   892,     # AE=American Elm
      'Fraxinus',    'Ash',         'HW',   197,    9,   801,     # AS=ash
      'Fraxinus',    'Ash',         'HW',   197,    0,   833,     # BA=black ash
      'Prunus',      'Cherry',      'HW',   0,      9,   721,     # BC=black cherry
      'Abies',       'Fir',         'SW',   128,    5,   721,     # BF=balsam fir
      'Quercus',     'Oak',         'HW',   265,    9,   881,     # BO=Black Oak
      'Populus',     'Poplar',      'HW',   98,     11,  641,     # BP=balsam poplar
      'Picea',       'Spruce',      'SW',   128,    5,   561,     # BS=black spruce
      'Populus',     'Aspen',       'HW',   98,     11,  689,     # BT=bigtooth aspen
      'Tilia',       'Basswood',    'HW',   0,      9,   657,     # BW=Basswood
      'Populus',     'Cottonwood',  'HW',   0,      9,   801,     # EC=eastern cottonwood
      'Tsuga',       'Hemlock',     'SW',   67,     5,   801,     # EH=eastern hemlock
      'Fraxinus',    'Ash',         'HW',   197,    9,   833,     # GA=green ash
      'Betula',      'Birch',       'HW',   0,      9,   737,     # GB=gray birch
      'Ostrya',      'Hophornbeam', 'HW',   0,      9,   961,     # HH=eastern hophornbeam
      'Pinus',       'Pine',        'SW',   0,      0,   801,     # JP=jack pine
      'Picea',       'Spruce',      'SW',   128,    5,   545,     # NS=Norway spruce
      'Thuja',       'Cedar',       'SW',   104,    15,  577,     # OC=Other Cedar
      'Hardwood',    'Hardwood',    'HW',   0,      9,   801,     # OH=other hardwoods
      'Softwood',    'Softwood',    'SW',   0,      0,   673,     # OS=other softwoods
      'Betula',      'Birch',       'HW',   158,    5,   833,     # PB=paper birch
      'Prunus',      'Cherry',      'HW',   0,      9,   600,     # PC=pin cherry
      'Prunus',      'Cherry',      'HW',   0,      9,   600,     # PR=pin cherry
      'Populus',     'Aspen',       'HW',   98,     11,  801,     # QA=quaking aspen
      'Betula',      'Birch',       'HW',   0,      9,   913,     # RB=river birch
      'Acer',        'Maple',       'HW',   157,    9,   801,     # RM=red maple
      'Pinus',       'Pine',        'SW',   72,     4,   673,     # RP=red pine
      'Pinus',       'Pine',        'SW',   72,     4,   673,     # RN=red pine
      'Quercus',     'Oak',         'HW',   265,    9,   1025,    # RO=red oak
      'Picea',       'Spruce',      'SW',   128,    5,   545,     # RS=red spruce
      'Betula',      'Birch',       'HW',   198,    9,   1038,    # SB=Sweet birch
      'Carya',       'Hickory',     'HW',   0,      9,   1025,    # SH=Shagbark Hickory
      'Acer',        'Maple',       'HW',   254,    9,   881,     # SM=sugar maple
      'Acer',        'Maple',       'HW',   0,      9,   753,     # ST=striped maple
      'Larix',       'Larch',       'SW',   0,      0,   753,     # TA=larch/tamarack
      'Fraxinus',    'Ash',         'HW',   197,    9,   801,     # WA=white ash
      'Thuja',       'Cedar',       'SW',   104,    15,  577,     # WC=northern white cedar
      'Quercus',     'Oak',         'HW',   135,    9,   1009,    # WO=White Oak
      'Pinus',       'Pine',        'SW',   175,    3,   561,     # WP=white pine
      'Picea',       'Spruce',      'SW',   128,    5,   561,     # WS=white spruce
      'Betula',      'Birch',       'HW',   198,    9,   945,     # YB=yellow birch
      'Other',       'Other',       'HW',   0,      0,   600),  # other
      ncol=6, byrow=TRUE)
   sprow = match(SPP,SPcodes)
   sprow[is.na(sprow)] = length(SPcodes)
   # Common Name [1] : Genus [2] : Simple[3] : WoodType[4] : Saw$[5] : Pulp$[6] : WT(kg/m3)[7]

   SPPValues <- (c(Common=SPCommonName[sprow],
            Genus=attrs[sprow,1], Simple=attrs[sprow,2], WoodType=attrs[sprow,3],
            SawPrice=attrs[sprow,4], PulpPrice=attrs[sprow,5], WT=attrs[sprow,6]))

   GreenWeight <- as.numeric(SPPValues[7])
   SawDollar <- as.numeric(SPPValues[5])
   PulpDollar <- as.numeric(SPPValues[6])
   Ton <- 907.18474
   Weight <- PulpVol*GreenWeight
   SawlogValue <- (BoardFeet/1000)*SawDollar
   PulpValue <- (Weight/Ton)*PulpDollar

   SawlogValue <- round(SawlogValue, 2)
   PulpValue <- round(PulpValue, 2)
   Values <- data.frame(Stand, Plot, Tree, SawlogValue, PulpValue)

return(Values)
}









