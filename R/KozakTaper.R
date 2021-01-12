#' Kozak Taper Equation
#'
#' This function allows you to calculate the diameter at any point on tree
#' for common tree species found in the northeastern and acadian forests.
#' All measurements must be entered as metric. Function requires
#' you to gives the specified height on tree for which you want diameter calculated.
#'
#' @param Bark specify if you want measurement inside or outside bark: 'ob' = outside, 'ib' = inside
#' @param SPP Species: use FVS species codes: example 'RO' - Red Oak, 'WS' = White Spruce
#' @param DHT Diameter at Height in cm where diameter measurment is desired
#' @param DBH Diameter Breast Height in cm
#' @param HT Tree Height in meters
#' @param Planted specify if the tree is planted, specify as TRUE or FALSE
#'
#' @examples
#' KozakTaper('ob', 'RO', 12, 40, 20, FALSE)
#' KozakTaper('ib', 'RS', 8, 28, 16, TRUE)
#'
#' @export

KozakTaper <- function(Bark, SPP, DHT, DBH, HT, Planted) {
  if (Bark == "ob" & SPP == "AB") {
    a0_tap <- 1.0693567631
    a1_tap <- 0.9975021951
    a2_tap <- -0.01282775
    b1_tap <- 0.3921013594
    b2_tap <- -1.054622304
    b3_tap <- 0.7758393514
    b4_tap <- 4.1034897617
    b5_tap <- 0.1185960455
    b6_tap <- -1.080697381
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "BC") {
    a0_tap <- 0.9802172591
    a1_tap <- 0.9900811022
    a2_tap <- 0.0215023934
    b1_tap <- 0.6092829761
    b2_tap <- -0.54627086
    b3_tap <- 0.5221909952
    b4_tap <- 1.6561496035
    b5_tap <- 0.040879378
    b6_tap <- -0.302807393
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "BF") {
    a0_tap <- 0.88075316
    a1_tap <- 1.01488665
    a2_tap <- 0.01958804
    b1_tap <- 0.41951756
    b2_tap <- -0.67232564
    b3_tap <- 0.54329725
    b4_tap <- 1.48181152
    b5_tap <- 0.06470371
    b6_tap <- -0.34684837
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "BF") {
    a0_tap <- 0.7909
    a1_tap <- 0.9745
    a2_tap <- 0.1198
    b1_tap <- 0.2688
    b2_tap <- -0.55134
    b3_tap <- 0.5612
    b4_tap <- 0.9007
    b5_tap <- 0.1257
    b6_tap <- -0.6708
    b7_tap <- 0
    # parms w/ FIA data
    a0_tap <- 0.87045800178728
    a1_tap <- 0.998148536293802
    a2_tap <- 0.0584816955042306
    b1_tap <- 0.302539012401385
    b2_tap <- -0.605787065734974
    b3_tap <- 0.588861845770261
    b4_tap <- 0.8826608914125
    b5_tap <- 0.103280103524893
    b6_tap <- -0.57432603217401
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "BP") {
    a0_tap <- 1.0036248405
    a1_tap <- 0.744246238
    a2_tap <- 0.2876417207
    b1_tap <- 0.6634046516
    b2_tap <- -2.004812235
    b3_tap <- 0.7507983401
    b4_tap <- 3.9248261105
    b5_tap <- 0.0276793767
    b6_tap <- -0.130928845
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "BS") {
    a0_tap <- 0.80472902
    a1_tap <- 1.00804553
    a2_tap <- 0.05601099
    b1_tap <- 0.35533529
    b2_tap <- -0.41320046
    b3_tap <- 0.41527304
    b4_tap <- 1.11652424
    b5_tap <- 0.0990167
    b6_tap <- -0.40992056
    b7_tap <- 0.11394943
  }
  else if (Bark == "ob" & SPP == "BS") {
    a0_tap <- 0.858
    a1_tap <- 0.9611
    a2_tap <- 0.105
    b1_tap <- 0.2604
    b2_tap <- -0.3409
    b3_tap <- 0.4797
    b4_tap <- 0.5008
    b5_tap <- 0.1097
    b6_tap <- -0.4952
    b7_tap <- 0.0969
    # parms w/ FIA data
    a0_tap <- 0.896382313496267
    a1_tap <- 0.979157280469517
    a2_tap <- 0.07070415827334
    b1_tap <- 0.288205614793081
    b2_tap <- -0.303580327062765
    b3_tap <- 0.435229599780184
    b4_tap <- 0.287092390832665
    b5_tap <- 0.0861036484421037
    b6_tap <- -0.407747649433411
    b7_tap <- 0.371113950891855
  }
  else if (Bark == "ob" & SPP == "BT") {
    a0_tap <- 1.0200889056
    a1_tap <- 1.0054957243
    a2_tap <- -0.011030907
    b1_tap <- 0.5104511725
    b2_tap <- -1.326415929
    b3_tap <- 0.5568665797
    b4_tap <- 7.2108347873
    b5_tap <- 0.071149738
    b6_tap <- -0.571844802
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "EH") {
    a0_tap <- 0.960235102
    a1_tap <- 1.00821143
    a2_tap <- -0.025167937
    b1_tap <- 0.825260258
    b2_tap <- 1.962520834
    b3_tap <- 0.415234319
    b4_tap <- -5.061571874
    b5_tap <- 0.009839526
    b6_tap <- -0.095533007
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "EH") {
    a0_tap <- 0.8681
    a1_tap <- 0.916
    a2_tap <- 0.1558
    b1_tap <- 0.4067
    b2_tap <- -0.6163
    b3_tap <- 0.4177
    b4_tap <- 3.6257
    b5_tap <- 0.1686
    b6_tap <- -0.8829
    b7_tap <- 0
    # parms w/ FIA data
    a0_tap <- 0.846409603849866
    a1_tap <- 0.984317716125905
    a2_tap <- 0.0807523481457474
    b1_tap <- 0.445438700558324
    b2_tap <- -0.671467572085628
    b3_tap <- 0.504954501484816
    b4_tap <- 2.48940465528
    b5_tap <- 0.124152912027385
    b6_tap <- -0.722954836646604
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "GA") {
    a0_tap <- 1.0852385488
    a1_tap <- 1.1861877395
    a2_tap <- -0.226193745
    b1_tap <- 0.5198788065
    b2_tap <- 1.4303205202
    b3_tap <- -0.349453901
    b4_tap <- 3.1952591271
    b5_tap <- 0.1391694941
    b6_tap <- -0.296716822
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "GB") {
    a0_tap <- 1.0263926931
    a1_tap <- 0.8835623138
    a2_tap <- 0.1307522645
    b1_tap <- 0.6113533288
    b2_tap <- -0.114188076
    b3_tap <- 0.2883217076
    b4_tap <- 2.657433495
    b5_tap <- 0.0590046356
    b6_tap <- -0.175127606
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "JP") {
    a0_tap <- 0.931552701
    a1_tap <- 1.008192708
    a2_tap <- -0.004177373
    b1_tap <- 0.431297353
    b2_tap <- -0.863672736
    b3_tap <- 0.511698303
    b4_tap <- 2.232484834
    b5_tap <- 0.059865263
    b6_tap <- -0.331897255
    b7_tap <- 0.039630786
  }
  else if (Bark == "ob" & SPP == "JP") {
    a0_tap <- 1.0214
    a1_tap <- 0.9817
    a2_tap <- 0.0147
    b1_tap <- 0.3753
    b2_tap <- -0.7954
    b3_tap <- 0.499
    b4_tap <- 2.0407
    b5_tap <- 0.0768
    b6_tap <- -0.3335
    b7_tap <- 0.0408
    # parms w/ FIA data
    a0_tap <- 0.842483072142665
    a1_tap <- 0.99279768524928
    a2_tap <- 0.0739425827838225
    b1_tap <- 0.37221919371203
    b2_tap <- -0.723225866494174
    b3_tap <- 0.453434142074953
    b4_tap <- 1.33754275322832
    b5_tap <- 0.073372838152118
    b6_tap <- -0.3105255908992
    b7_tap <- 0.396398949039286
  }
  else if (Bark == "ib" & SPP == "NS") {
    a0_tap <- 0.9308817
    a1_tap <- 0.97360573
    a2_tap <- 0.03522864
    b1_tap <- 0.65078104
    b2_tap <- -0.30355787
    b3_tap <- 0.37832812
    b4_tap <- 1.18815216
    b5_tap <- 0.03111631
    b6_tap <- -0.03172809
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "NS") {
    a0_tap <- 1.0513
    a1_tap <- 0.9487
    a2_tap <- 0.0374
    b1_tap <- 0.611
    b2_tap <- -0.3001
    b3_tap <- 0.3731
    b4_tap <- 1.1255
    b5_tap <- 0.0318
    b6_tap <- -0.0297
    b7_tap <- 0
    # parms w/ FIA data
    a0_tap <- 0.950952303433305
    a1_tap <- 0.99162401049595
    a2_tap <- 0.0357175689757522
    b1_tap <- 0.507484658718266
    b2_tap <- -0.44046929698967
    b3_tap <- 0.405856745795155
    b4_tap <- 1.2849978191539
    b5_tap <- 0.0143964536822362
    b6_tap <- -0.0785889411281423
    b7_tap <- 0.169725200257675
  }
  else if (Bark == "ib" & SPP == "PB") {
    a0_tap <- 0.7161229027
    a1_tap <- 0.9811224473
    a2_tap <- 0.1382539493
    b1_tap <- 0.4782152412
    b2_tap <- 0.3091537448
    b3_tap <- 0.3266307618
    b4_tap <- -0.302056097
    b5_tap <- 0.0858585241
    b6_tap <- -0.278661048
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "PB") {
    a0_tap <- 0.7161229027
    a1_tap <- 0.9811224473
    a2_tap <- 0.1382539493
    b1_tap <- 0.4782152412
    b2_tap <- 0.3091537448
    b3_tap <- 0.3266307618
    b4_tap <- -0.302056097
    b5_tap <- 0.0858585241
    b6_tap <- -0.278661048
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "QA") {
    a0_tap <- 0.5586975794
    a1_tap <- 0.9047841359
    a2_tap <- 0.3075094544
    b1_tap <- 0.7131251715
    b2_tap <- -0.588345303
    b3_tap <- 0.4292045831
    b4_tap <- 2.8516108932
    b5_tap <- 0.0381609362
    b6_tap <- -0.13426388
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "RM") {
    a0_tap <- 0.745826994
    a1_tap <- 1.0092251371
    a2_tap <- 0.0890931039
    b1_tap <- 0.5861620841
    b2_tap <- -0.865905462
    b3_tap <- 0.6539243149
    b4_tap <- 3.0603989176
    b5_tap <- 0.0827619274
    b6_tap <- -0.64859681
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "RM") {
    a0_tap <- 0.745826994
    a1_tap <- 1.0092251371
    a2_tap <- 0.0890931039
    b1_tap <- 0.5861620841
    b2_tap <- -0.865905462
    b3_tap <- 0.6539243149
    b4_tap <- 3.0603989176
    b5_tap <- 0.0827619274
    b6_tap <- -0.64859681
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "RO") {
    a0_tap <- 1.1751352376
    a1_tap <- 1.02249704
    a2_tap <- -0.069888591
    b1_tap <- 0.4505675893
    b2_tap <- -0.902884964
    b3_tap <- 0.5812519636
    b4_tap <- 3.6267479819
    b5_tap <- 0.1656137742
    b6_tap <- -1.114281314
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "RP" | SPP == "RN") {
    a0_tap <- 0.9717883
    a1_tap <- 1.00113806
    a2_tap <- -0.01597933
    b1_tap <- 0.51143292
    b2_tap <- -0.9739954
    b3_tap <- 0.25844201
    b4_tap <- 4.75315518
    b5_tap <- 0.05846224
    b6_tap <- -0.12372176
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "RP") {
    a0_tap <- 1.0962
    a1_tap <- 1.006
    a2_tap <- -0.0352
    b1_tap <- 0.5
    b2_tap <- -0.9959
    b3_tap <- 0.3007
    b4_tap <- 4.6358
    b5_tap <- 0.0473
    b6_tap <- -0.05
    b7_tap <- 0
    # parms w/ FIA data
    a0_tap <- 1.06470820904747
    a1_tap <- 0.994899036827748
    a2_tap <- -0.0123828485987216
    b1_tap <- 0.458957297467137
    b2_tap <- -1.04575412640177
    b3_tap <- 0.361452014890273
    b4_tap <- 4.00047777431758
    b5_tap <- 0.0543368451581955
    b6_tap <- -0.128025447306836
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "RS") {
    a0_tap <- 0.89797987
    a1_tap <- 1.00579742
    a2_tap <- 0.01667313
    b1_tap <- 0.49500865
    b2_tap <- -0.63375155
    b3_tap <- 0.3836274
    b4_tap <- 1.41380994
    b5_tap <- 0.08866994
    b6_tap <- -0.29753964
    b7_tap <- 0.15192029
  }
  else if (Bark == "ob" & SPP == "RS") {
    a0_tap <- 0.8758
    a1_tap <- 0.992
    a2_tap <- 0.0633
    b1_tap <- 0.4128
    b2_tap <- -0.6877
    b3_tap <- 0.4413
    b4_tap <- 1.1818
    b5_tap <- 0.1131
    b6_tap <- -0.4356
    b7_tap <- 0.1042
    # parms w/ FIA data
    a0_tap <- 0.886886241411388
    a1_tap <- 0.995431239145283
    a2_tap <- 0.0541365481351767
    b1_tap <- 0.411160410244944
    b2_tap <- -0.658022227353248
    b3_tap <- 0.418213595349517
    b4_tap <- 1.09113756405639
    b5_tap <- 0.102379812299201
    b6_tap <- -0.40367256147942
    b7_tap <- 0.104842994095004
  }
  # Sweet birch
  else if (Bark == "ob" & SPP == "SB") {
    a0_tap <- 0.8471057131
    a1_tap <- 0.9875376729
    a2_tap <- 0.0769690406
    b1_tap <- 0.9322599144
    b2_tap <- -0.954580316
    b3_tap <- 0.48553875
    b4_tap <- 3.0294545606
    b5_tap <- 0.0767610836
    b6_tap <- -0.238398236
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "SM") {
    a0_tap <- 1.0517056747
    a1_tap <- 0.96129896
    a2_tap <- 0.0386037512
    b1_tap <- 0.8556437779
    b2_tap <- -0.249723079
    b3_tap <- 0.4149367053
    b4_tap <- 1.2548340569
    b5_tap <- 0.0412998707
    b6_tap <- -0.113500099
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "TL") {
    a0_tap <- 0.7387
    a1_tap <- 0.9716
    a2_tap <- 0.1431
    b1_tap <- 0.271
    b2_tap <- -0.4958
    b3_tap <- 0.6508
    b4_tap <- -0.3887
    b5_tap <- 0.1324
    b6_tap <- -0.7035
    b7_tap <- 0
    # parms w/ FIA data
    a0_tap <- 0.762977580507808
    a1_tap <- 0.979320525735404
    a2_tap <- 0.122788251183516
    b1_tap <- 0.245935863173793
    b2_tap <- -0.564901857800367
    b3_tap <- 0.666790795105499
    b4_tap <- -0.0728778930339496
    b5_tap <- 0.143651487515151
    b6_tap <- -0.791188036888163
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "WA") {
    a0_tap <- 0.8550736297
    a1_tap <- 0.9768941226
    a2_tap <- 0.0770356694
    b1_tap <- 0.7819090026
    b2_tap <- -0.791762733
    b3_tap <- 0.476698925
    b4_tap <- 3.5003928402
    b5_tap <- 0.0859040469
    b6_tap <- -0.487974342
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "WC") {
    a0_tap <- 0.86118766
    a1_tap <- 0.98152118
    a2_tap <- 0.0568203
    b1_tap <- 0.40717678
    b2_tap <- -0.05482572
    b3_tap <- 0.47809459
    b4_tap <- -1.32512447
    b5_tap <- 0.1538487
    b6_tap <- -0.53687808
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "WC") {
    a0_tap <- 0.902
    a1_tap <- 0.9676
    a2_tap <- 0.085
    b1_tap <- 0.3204
    b2_tap <- -0.4336
    b3_tap <- 0.5212
    b4_tap <- 0.0157
    b5_tap <- 0.137
    b6_tap <- -0.4585
    b7_tap <- 0
    # parms w/ FIA data
    a0_tap <- 0.876976728762079
    a1_tap <- 0.972187200775237
    a2_tap <- 0.0905032843727524
    b1_tap <- 0.319643790061659
    b2_tap <- -0.495778605215774
    b3_tap <- 0.546605647382787
    b4_tap <- -0.0540118375921429
    b5_tap <- 0.131666046721139
    b6_tap <- -0.454765563250266
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "WP") {
    a0_tap <- 1.04881379
    a1_tap <- 1.00779696
    a2_tap <- -0.04595353
    b1_tap <- 0.38085445
    b2_tap <- -0.85956463
    b3_tap <- 0.34380669
    b4_tap <- 4.60836993
    b5_tap <- 0.111855
    b6_tap <- -0.5523203
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "WP") {
    a0_tap <- 1.0202
    a1_tap <- 0.985
    a2_tap <- 0.0149
    b1_tap <- 0.3697
    b2_tap <- -0.7512
    b3_tap <- 0.3536
    b4_tap <- 3.8496
    b5_tap <- 0.1074
    b6_tap <- -0.5131
    b7_tap <- 0
    # parms w/ FIA data
    a0_tap <- 0.961977278802905
    a1_tap <- 0.985977453808376
    a2_tap <- 0.0333180987707418
    b1_tap <- 0.383416881614619
    b2_tap <- -0.753661988626837
    b3_tap <- 0.392529765236197
    b4_tap <- 3.4224381734935
    b5_tap <- 0.100601541094101
    b6_tap <- -0.485617012177084
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "WS") {
    a0_tap <- 1.0202
    a1_tap <- 0.985
    a2_tap <- 0.0149
    b1_tap <- 0.3697
    b2_tap <- -0.7512
    b3_tap <- 0.3536
    b4_tap <- 3.8496
    b5_tap <- 0.1074
    b6_tap <- -0.5131
    b7_tap <- 0
  }
  else if (Bark == "ib" & SPP == "WS") {
    a0_tap <- 0.75826241
    a1_tap <- 0.98481863
    a2_tap <- 0.09956165
    b1_tap <- 0.36505143
    b2_tap <- -0.51501314
    b3_tap <- 0.55913869
    b4_tap <- 0.75846281
    b5_tap <- 0.07011851
    b6_tap <- -0.44928376
    b7_tap <- 0.07830011
  }
  else if (Bark == "ob" & SPP == "WS") {
    a0_tap <- 0.7317
    a1_tap <- 0.9577
    a2_tap <- 0.1593
    b1_tap <- 0.2638
    b2_tap <- -0.4246
    b3_tap <- 0.5505
    b4_tap <- -0.1269
    b5_tap <- 0.1145
    b6_tap <- -0.6249
    b7_tap <- 0.088
    # parms w/ FIA data
    a0_tap <- 0.4273654
    a1_tap <- 0.9966565
    a2_tap <- 0.3042950
    b1_tap <- 0.2537842
    b2_tap <- -0.3901056
    b3_tap <- 0.5697576
    b4_tap <- -1.3592192
    b5_tap <- 0.1345268
    b6_tap <- -0.7087157
    b7_tap <- 0.1619956
  }
  else if (Bark == "ob" & SPP == "YB") {
    a0_tap <- 1.1263776728
    a1_tap <- 0.9485083275
    a2_tap <- 0.0371321602
    b1_tap <- 0.7662525552
    b2_tap <- -0.028147685
    b3_tap <- 0.2334044323
    b4_tap <- 4.8569609081
    b5_tap <- 0.0753180483
    b6_tap <- -0.205052535
    b7_tap <- 0
  }
  else if (Bark == "ob" & SPP == "OH" | SPP == "OHW") {
    a0_tap <- 1.1263776728
    a1_tap <- 0.9485083275
    a2_tap <- 0.0371321602
    b1_tap <- 0.7662525552
    b2_tap <- -0.028147685
    b3_tap <- 0.2334044323
    b4_tap <- 4.8569609081
    b5_tap <- 0.0753180483
    b6_tap <- -0.205052535
    b7_tap <- 0
  }
  else {
    a0_tap <- 1.1263776728
    a1_tap <- 0.9485083275
    a2_tap <- 0.0371321602
    b1_tap <- 0.7662525552
    b2_tap <- -0.028147685
    b3_tap <- 0.2334044323
    b4_tap <- 4.8569609081
    b5_tap <- 0.0753180483
    b6_tap <- -0.205052535
    b7_tap <- 0
  }
  p <- 1.3 / HT
  z <- DHT / HT
  Xi <- (1 - z^(1 / 3)) / (1 - p^(1 / 3))
  Qi <- 1 - z^(1 / 3)
  y <- (a0_tap * (DBH^a1_tap) * (HT^a2_tap)) * Xi^(b1_tap * z^4 + b2_tap * (exp(-DBH / HT)) +
                                                     b3_tap * Xi^0.1 + b4_tap * (1 / DBH) + b5_tap * HT^Qi + b6_tap * Xi + b7_tap * Planted)
  return(y = round(y, 4))
}





