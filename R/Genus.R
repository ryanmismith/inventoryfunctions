#' Returns Genus and Common Name for SPP in Inventory
#'
#' This function returns the Genus, Wood Type (HW/SW), and Common Name for species
#' included in the Northeast Variant of FVS. Genus can be useful for
#' modeling purposes, and the common and lazy names can be used for creating
#' graphic representations of forest composition for non-foresters.
#'
#'
#'@param SPP Species: use FVS species codes
#'
#'@return
#' The 1: Common Name, 2: Genus 3: Lazy Name and
#' 4: WoodType (HW/SW) of species entered.
#'
#' @examples
#' trees <- c(1,2,3,4,5)
#' SPP <- c("BF", "BF", "RO", "RS", "RO")
#' tree_df <- data.frame(trees, SPP)
#' genusinfo <- apply(tree_df, 2, Genus)
#'
#' Genus("BA")
#'
#'@export

Genus <- function(SPP){
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
  attrs <- matrix (c(
    # Genus      # Simple Name  #HW/SW
    'Fagus',       'Beech',       'HW',   # AB=American beech
    'Ulmus',       'Elm',         'HW',   # AE=American Elm
    'Fraxinus',    'Ash',         'HW',   # AS=ash
    'Fraxinus',    'Ash',         'HW',   # BA=black ash
    'Prunus',      'Cherry',      'HW',   # BC=black cherry
    'Abies',       'Fir',         'SW',   # BF=balsam fir
    'Quercus',     'Oak',         'HW',   # BO=Black Oak
    'Populus',     'Poplar',      'HW',   # BP=balsam poplar
    'Picea',       'Spruce',      'SW',   # BS=black spruce
    'Populus',     'Aspen',       'HW',   # BT=bigtooth aspen
    'Tilia',       'Basswood',    'HW',   # BW=Basswood
    'Populus',     'Cottonwood',  'HW',   # EC=eastern cottonwood
    'Tsuga',       'Hemlock',     'SW',   # EH=eastern hemlock
    'Fraxinus',    'Ash',         'HW',   # GA=green ash
    'Betula',      'Birch',       'HW',   # GB=gray birch
    'Ostrya',      'Hophornbeam', 'HW',   # HH=eastern hophornbeam
    'Pinus',       'Pine',        'SW',   # JP=jack pine
    'Picea',       'Spruce',      'SW',   # NS=Norway spruce
    'Thuja',       'Cedar',       'SW',   # OC=Other Cedar
    'Hardwood',    'Hardwood',    'HW',   # OH=other hardwoods
    'Softwood',    'Softwood',    'SW',   # OS=other softwoods
    'Betula',      'Birch',       'HW',   # PB=paper birch
    'Prunus',      'Cherry',      'HW',   # PC=pin cherry
    'Prunus',      'Cherry',      'HW',   # PR=pin cherry
    'Populus',     'Aspen',       'HW',   # QA=quaking aspen
    'Betula',      'Birch',       'HW',   # RB=river birch
    'Acer',        'Maple',       'HW',   # RM=red maple
    'Pinus',       'Pine',        'SW',   # RP=red pine
    'Pinus',       'Pine',        'SW',   # RN=red pine
    'Quercus',     'Oak',         'HW',   # RO=red oak
    'Picea',       'Spruce',      'SW',   # RS=red spruce
    'Betula',      'Birch',       'HW',   # SB=Sweet birch
    'Carya',       'Hickory',     'HW',   # SH=Shagbark Hickory
    'Acer',        'Maple',       'HW',   # SM=sugar maple
    'Acer',        'Maple',       'HW',   # ST=striped maple
    'Larix',       'Larch',       'SW',   # TA=larch/tamarack
    'Fraxinus',    'Ash',         'HW',   # WA=white ash
    'Thuja',       'Cedar',       'SW',   # WC=northern white cedar
    'Quercus',     'Oak',         'HW',   # WO=White Oak
    'Pinus',       'Pine',        'SW',   # WP=white pine
    'Picea',       'Spruce',      'SW',   # WS=white spruce
    'Betula',      'Birch',       'HW',   # YB=yellow birch
    'Other',       'Other',       'HW'),  # other
    ncol=3,byrow=TRUE)
  sprow = match(SPP,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)
  # Common Name [1] : Genus [2] : Simple[3] : WoodType[4]
  return(c(Common=SPCommonName[sprow],
           Genus=attrs[sprow,1], Simple=attrs[sprow,2], WoodType=attrs[sprow,3]))
}



