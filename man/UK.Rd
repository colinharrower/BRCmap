\name{UK}
\alias{UK}
\docType{data}
\title{
List of Spatial Polygons containing UK outlines
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(UK)}
\format{
  The format is:
List of 3
 $ britain:Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  .. ..@ polygons   :List of 2002
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 461231 1220164
  .. .. .. .. .. .. .. ..@ area   : num 27637
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 461160 461126 461361 461321 461160 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 461231 1220164
  .. .. .. .. ..@ ID       : chr "0"
  .. .. .. .. ..@ area     : num 27637
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 460669 1219701
  .. .. .. .. .. .. .. ..@ area   : num 29913
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 460600 460585 460753 460748 460600 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 460669 1219701
  .. .. .. .. ..@ ID       : chr "1"
  .. .. .. .. ..@ area     : num 29913
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 460401 1219346
  .. .. .. .. .. .. .. ..@ area   : num 36426
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 460360 460311 460475 460459 460360 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 460401 1219346
  .. .. .. .. ..@ ID       : chr "2"
  .. .. .. .. ..@ area     : num 36426
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 460129 1219190
  .. .. .. .. .. .. .. ..@ area   : num 39536
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 460120 460021 460176 460217 460120 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 460129 1219190
  .. .. .. .. ..@ ID       : chr "3"
  .. .. .. .. ..@ area     : num 39536
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 459797 1218912
  .. .. .. .. .. .. .. ..@ area   : num 17502
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 459840 459700 459790 459881 459840 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 459797 1218912
  .. .. .. .. ..@ ID       : chr "4"
  .. .. .. .. ..@ area     : num 17502
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 460910 1208877
  .. .. .. .. .. .. .. ..@ area   : num 1.23e+08
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:42, 1:2] 457220 458433 457777 459396 459776 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 460910 1208877
  .. .. .. .. ..@ ID       : chr "5"
  .. .. .. .. ..@ area     : num 1.23e+08
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 459928 1218333
  .. .. .. .. .. .. .. ..@ area   : num 12900
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 459960 459875 459880 459984 459960 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 459928 1218333
  .. .. .. .. ..@ ID       : chr "6"
  .. .. .. .. ..@ area     : num 12900
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 466950 1217503
  .. .. .. .. .. .. .. ..@ area   : num 12225
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 466940 466879 466978 467018 466940 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 466950 1217503
  .. .. .. .. ..@ ID       : chr "7"
  .. .. .. .. ..@ area     : num 12225
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 466748 1217132
  .. .. .. .. .. .. .. ..@ area   : num 13123
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 466860 466692 466613 466788 466860 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 466748 1217132
  .. .. .. .. ..@ ID       : chr "8"
  .. .. .. .. ..@ area     : num 13123
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 466788 1216928
  .. .. .. .. .. .. .. ..@ area   : num 27420
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 466760 466686 466880 466861 466760 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 466788 1216928
  .. .. .. .. ..@ ID       : chr "9"
  .. .. .. .. ..@ area     : num 27420
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 456866 1211272
  .. .. .. .. .. .. .. ..@ area   : num 32203
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 456920 456763 456796 456995 456920 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 456866 1211272
  .. .. .. .. ..@ ID       : chr "10"
  .. .. .. .. ..@ area     : num 32203
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 457175 1210208
  .. .. .. .. .. .. .. ..@ area   : num 21166
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 457140 457081 457216 457262 457140 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 457175 1210208
  .. .. .. .. ..@ ID       : chr "11"
  .. .. .. .. ..@ area     : num 21166
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 457332 1210024
  .. .. .. .. .. .. .. ..@ area   : num 13179
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 457320 457262 457312 457419 457320 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 457332 1210024
  .. .. .. .. ..@ ID       : chr "12"
  .. .. .. .. ..@ area     : num 13179
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 465938 1207738
  .. .. .. .. .. .. .. ..@ area   : num 547677
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 466020 466220 465779 465780 466020 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 465938 1207738
  .. .. .. .. ..@ ID       : chr "13"
  .. .. .. .. ..@ area     : num 547677
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 448150 1206601
  .. .. .. .. .. .. .. ..@ area   : num 20746
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 448100 448082 448152 448241 448100 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 448150 1206601
  .. .. .. .. ..@ ID       : chr "14"
  .. .. .. .. ..@ area     : num 20746
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 464868 1206187
  .. .. .. .. .. .. .. ..@ area   : num 118413
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 464760 464741 465020 464936 464760 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 464868 1206187
  .. .. .. .. ..@ ID       : chr "15"
  .. .. .. .. ..@ area     : num 118413
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 448417 1206210
  .. .. .. .. .. .. .. ..@ area   : num 71386
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 448420 448277 448360 448596 448420 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 448417 1206210
  .. .. .. .. ..@ ID       : chr "16"
  .. .. .. .. ..@ area     : num 71386
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 456064 1206253
  .. .. .. .. .. .. .. ..@ area   : num 13013
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 456040 456004 456059 456143 456040 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 456064 1206253
  .. .. .. .. ..@ ID       : chr "17"
  .. .. .. .. ..@ area     : num 13013
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 452029 1205828
  .. .. .. .. .. .. .. ..@ area   : num 26285
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 452060 451932 452000 452119 452060 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 452029 1205828
  .. .. .. .. ..@ ID       : chr "18"
  .. .. .. .. ..@ area     : num 26285
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 456119 1205781
  .. .. .. .. .. .. .. ..@ area   : num 8227
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 456100 456077 456087 456183 456100 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 456119 1205781
  .. .. .. .. ..@ ID       : chr "19"
  .. .. .. .. ..@ area     : num 8227
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 449658 1191772
  .. .. .. .. .. .. .. ..@ area   : num 2.1e+08
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:65, 1:2] 447580 447109 448042 447738 449705 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 449658 1191772
  .. .. .. .. ..@ ID       : chr "20"
  .. .. .. .. ..@ area     : num 2.1e+08
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 456411 1205652
  .. .. .. .. .. .. .. ..@ area   : num 17295
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 456340 456319 456519 456508 456340 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 456411 1205652
  .. .. .. .. ..@ ID       : chr "21"
  .. .. .. .. ..@ area     : num 17295
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 453688 1205418
  .. .. .. .. .. .. .. ..@ area   : num 14349
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 453720 453580 453732 453761 453720 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 453688 1205418
  .. .. .. .. ..@ ID       : chr "22"
  .. .. .. .. ..@ area     : num 14349
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 447576 1204210
  .. .. .. .. .. .. .. ..@ area   : num 7134
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 447600 447522 447574 447618 447600 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 447576 1204210
  .. .. .. .. ..@ ID       : chr "23"
  .. .. .. .. ..@ area     : num 7134
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 464673 1203168
  .. .. .. .. .. .. .. ..@ area   : num 12276
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 464660 464600 464709 464737 464660 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 464673 1203168
  .. .. .. .. ..@ ID       : chr "24"
  .. .. .. .. ..@ area     : num 12276
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 460353 1199097
  .. .. .. .. .. .. .. ..@ area   : num 2289645
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 459860 461392 460977 459061 459860 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 460353 1199097
  .. .. .. .. ..@ ID       : chr "25"
  .. .. .. .. ..@ area     : num 2289645
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 457456 1199007
  .. .. .. .. .. .. .. ..@ area   : num 16518
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 457540 457500 457502 457358 457540 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 457456 1199007
  .. .. .. .. ..@ ID       : chr "26"
  .. .. .. .. ..@ area     : num 16518
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 455972 1198382
  .. .. .. .. .. .. .. ..@ area   : num 232099
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 455880 456300 456178 455360 455880 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 455972 1198382
  .. .. .. .. ..@ ID       : chr "27"
  .. .. .. .. ..@ area     : num 232099
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 463495 1198187
  .. .. .. .. .. .. .. ..@ area   : num 73601
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 463560 463342 463391 463640 463560 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 463495 1198187
  .. .. .. .. ..@ ID       : chr "28"
  .. .. .. .. ..@ area     : num 73601
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437640 1198033
  .. .. .. .. .. .. .. ..@ area   : num 20658
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437700 437565 437579 437720 437700 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437640 1198033
  .. .. .. .. ..@ ID       : chr "29"
  .. .. .. .. ..@ area     : num 20658
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437762 1197837
  .. .. .. .. .. .. .. ..@ area   : num 32449
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437800 437618 437807 437863 437800 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437762 1197837
  .. .. .. .. ..@ ID       : chr "30"
  .. .. .. .. ..@ area     : num 32449
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437498 1197605
  .. .. .. .. .. .. .. ..@ area   : num 15341
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437500 437400 437534 437574 437500 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437498 1197605
  .. .. .. .. ..@ ID       : chr "31"
  .. .. .. .. ..@ area     : num 15341
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 461373 1197407
  .. .. .. .. .. .. .. ..@ area   : num 19249
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 461280 461440 461410 461372 461280 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 461373 1197407
  .. .. .. .. ..@ ID       : chr "32"
  .. .. .. .. ..@ area     : num 19249
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437542 1197425
  .. .. .. .. .. .. .. ..@ area   : num 15494
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437520 437446 437577 437636 437520 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437542 1197425
  .. .. .. .. ..@ ID       : chr "33"
  .. .. .. .. ..@ area     : num 15494
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437578 1197148
  .. .. .. .. .. .. .. ..@ area   : num 15348
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437580 437503 437600 437640 437580 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437578 1197148
  .. .. .. .. ..@ ID       : chr "34"
  .. .. .. .. ..@ area     : num 15348
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 445625 1196878
  .. .. .. .. .. .. .. ..@ area   : num 21285
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 445580 445576 445669 445687 445580 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 445625 1196878
  .. .. .. .. ..@ ID       : chr "35"
  .. .. .. .. ..@ area     : num 21285
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437783 1196818
  .. .. .. .. .. .. .. ..@ area   : num 17365
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437740 437701 437817 437880 437740 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437783 1196818
  .. .. .. .. ..@ ID       : chr "36"
  .. .. .. .. ..@ area     : num 17365
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 438284 1196713
  .. .. .. .. .. .. .. ..@ area   : num 89662
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 438400 438078 438210 438436 438400 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 438284 1196713
  .. .. .. .. ..@ ID       : chr "37"
  .. .. .. .. ..@ area     : num 89662
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 457923 1196122
  .. .. .. .. .. .. .. ..@ area   : num 36553
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 457980 457837 457745 458118 457980 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 457923 1196122
  .. .. .. .. ..@ ID       : chr "38"
  .. .. .. .. ..@ area     : num 36553
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 459594 1195613
  .. .. .. .. .. .. .. ..@ area   : num 138740
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 459400 459479 459860 459546 459400 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 459594 1195613
  .. .. .. .. ..@ ID       : chr "39"
  .. .. .. .. ..@ area     : num 138740
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437785 1195558
  .. .. .. .. .. .. .. ..@ area   : num 17651
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437780 437678 437835 437878 437780 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437785 1195558
  .. .. .. .. ..@ ID       : chr "40"
  .. .. .. .. ..@ area     : num 17651
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437955 1195406
  .. .. .. .. .. .. .. ..@ area   : num 13698
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437980 437860 437952 438041 437980 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437955 1195406
  .. .. .. .. ..@ ID       : chr "41"
  .. .. .. .. ..@ area     : num 13698
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437907 1195204
  .. .. .. .. .. .. .. ..@ area   : num 27823
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437980 437830 437780 438014 437980 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437907 1195204
  .. .. .. .. ..@ ID       : chr "42"
  .. .. .. .. ..@ area     : num 27823
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 460329 1194916
  .. .. .. .. .. .. .. ..@ area   : num 75541
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 460200 460187 460479 460421 460200 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 460329 1194916
  .. .. .. .. ..@ ID       : chr "43"
  .. .. .. .. ..@ area     : num 75541
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 459773 1194992
  .. .. .. .. .. .. .. ..@ area   : num 32734
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 459800 459660 459764 459880 459800 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 459773 1194992
  .. .. .. .. ..@ ID       : chr "44"
  .. .. .. .. ..@ area     : num 32734
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 436220 1156678
  .. .. .. .. .. .. .. ..@ area   : num 9.48e+08
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:353, 1:2] 431900 434411 435674 436481 435909 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 436220 1156678
  .. .. .. .. ..@ ID       : chr "45"
  .. .. .. .. ..@ area     : num 9.48e+08
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 459393 1194938
  .. .. .. .. .. .. .. ..@ area   : num 18632
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 459320 459335 459477 459448 459320 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 459393 1194938
  .. .. .. .. ..@ ID       : chr "46"
  .. .. .. .. ..@ area     : num 18632
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 462297 1191396
  .. .. .. .. .. .. .. ..@ area   : num 40328338
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] 458440 459766 459620 460504 461860 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 462297 1191396
  .. .. .. .. ..@ ID       : chr "47"
  .. .. .. .. ..@ area     : num 40328338
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 464167 1194379
  .. .. .. .. .. .. .. ..@ area   : num 22481
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 464180 464075 464187 464241 464180 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 464167 1194379
  .. .. .. .. ..@ ID       : chr "48"
  .. .. .. .. ..@ area     : num 22481
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 455111 1192075
  .. .. .. .. .. .. .. ..@ area   : num 2388414
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 456320 455540 454740 454769 454117 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 455111 1192075
  .. .. .. .. ..@ ID       : chr "49"
  .. .. .. .. ..@ area     : num 2388414
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 467145 1193173
  .. .. .. .. .. .. .. ..@ area   : num 19115
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 467220 467052 467019 467215 467220 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 467145 1193173
  .. .. .. .. ..@ ID       : chr "50"
  .. .. .. .. ..@ area     : num 19115
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 431452 1192933
  .. .. .. .. .. .. .. ..@ area   : num 193919
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 431140 431281 432025 431532 431140 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 431452 1192933
  .. .. .. .. ..@ ID       : chr "51"
  .. .. .. .. ..@ area     : num 193919
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 443819 1193076
  .. .. .. .. .. .. .. ..@ area   : num 17756
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 443880 443746 443794 443858 443880 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 443819 1193076
  .. .. .. .. ..@ ID       : chr "52"
  .. .. .. .. ..@ area     : num 17756
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 432700 1192741
  .. .. .. .. .. .. .. ..@ area   : num 16481
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 432640 432637 432753 432770 432640 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 432700 1192741
  .. .. .. .. ..@ ID       : chr "53"
  .. .. .. .. ..@ area     : num 16481
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 443526 1191708
  .. .. .. .. .. .. .. ..@ area   : num 26953
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 443500 443440 443508 443636 443500 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 443526 1191708
  .. .. .. .. ..@ ID       : chr "54"
  .. .. .. .. ..@ area     : num 26953
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 452745 1191308
  .. .. .. .. .. .. .. ..@ area   : num 21077
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 452680 452660 452797 452824 452680 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 452745 1191308
  .. .. .. .. ..@ ID       : chr "55"
  .. .. .. .. ..@ area     : num 21077
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 438196 1189511
  .. .. .. .. .. .. .. ..@ area   : num 12336
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 438160 438129 438227 438263 438160 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 438196 1189511
  .. .. .. .. ..@ ID       : chr "56"
  .. .. .. .. ..@ area     : num 12336
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 443401 1189240
  .. .. .. .. .. .. .. ..@ area   : num 30676
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 443340 443320 443434 443504 443340 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 443401 1189240
  .. .. .. .. ..@ ID       : chr "57"
  .. .. .. .. ..@ area     : num 30676
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437864 1189191
  .. .. .. .. .. .. .. ..@ area   : num 13547
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437840 437800 437888 437925 437840 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437864 1189191
  .. .. .. .. ..@ ID       : chr "58"
  .. .. .. .. ..@ area     : num 13547
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 440281 1188830
  .. .. .. .. .. .. .. ..@ area   : num 120286
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 440300 440100 440257 440459 440300 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 440281 1188830
  .. .. .. .. ..@ ID       : chr "59"
  .. .. .. .. ..@ area     : num 120286
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 443850 1188784
  .. .. .. .. .. .. .. ..@ area   : num 17399
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 443860 443781 443829 443922 443860 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 443850 1188784
  .. .. .. .. ..@ ID       : chr "60"
  .. .. .. .. ..@ area     : num 17399
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 428768 1186778
  .. .. .. .. .. .. .. ..@ area   : num 33808
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 428920 428667 428662 428808 428920 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 428768 1186778
  .. .. .. .. ..@ ID       : chr "61"
  .. .. .. .. ..@ area     : num 33808
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 440352 1186067
  .. .. .. .. .. .. .. ..@ area   : num 29232
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 440400 440254 440289 440459 440400 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 440352 1186067
  .. .. .. .. ..@ ID       : chr "62"
  .. .. .. .. ..@ area     : num 29232
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 425918 1185870
  .. .. .. .. .. .. .. ..@ area   : num 15750
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 425960 425905 425839 425978 425960 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 425918 1185870
  .. .. .. .. ..@ ID       : chr "63"
  .. .. .. .. ..@ area     : num 15750
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437803 1185540
  .. .. .. .. .. .. .. ..@ area   : num 7357
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437820 437763 437797 437839 437820 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437803 1185540
  .. .. .. .. ..@ ID       : chr "64"
  .. .. .. .. ..@ area     : num 7357
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 427965 1185305
  .. .. .. .. .. .. .. ..@ area   : num 25679
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 428000 427870 427926 428056 428000 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 427965 1185305
  .. .. .. .. ..@ ID       : chr "65"
  .. .. .. .. ..@ area     : num 25679
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 437924 1185300
  .. .. .. .. .. .. .. ..@ area   : num 10292
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 437940 437863 437921 437981 437940 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 437924 1185300
  .. .. .. .. ..@ ID       : chr "66"
  .. .. .. .. ..@ area     : num 10292
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 438189 1185273
  .. .. .. .. .. .. .. ..@ area   : num 16661
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 438200 438119 438157 438262 438200 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 438189 1185273
  .. .. .. .. ..@ ID       : chr "67"
  .. .. .. .. ..@ area     : num 16661
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 422007 1184988
  .. .. .. .. .. .. .. ..@ area   : num 17516
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 422160 422045 421819 422044 422160 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 422007 1184988
  .. .. .. .. ..@ ID       : chr "68"
  .. .. .. .. ..@ area     : num 17516
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 427418 1184938
  .. .. .. .. .. .. .. ..@ area   : num 20973
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 427380 427342 427416 427519 427380 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 427418 1184938
  .. .. .. .. ..@ ID       : chr "69"
  .. .. .. .. ..@ area     : num 20973
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 421909 1184746
  .. .. .. .. .. .. .. ..@ area   : num 24250
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 422020 421938 421713 421984 422020 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 421909 1184746
  .. .. .. .. ..@ ID       : chr "70"
  .. .. .. .. ..@ area     : num 24250
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 438982 1181536
  .. .. .. .. .. .. .. ..@ area   : num 317289
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 439100 439383 438700 438780 439100 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 438982 1181536
  .. .. .. .. ..@ ID       : chr "71"
  .. .. .. .. ..@ area     : num 317289
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 442425 1181387
  .. .. .. .. .. .. .. ..@ area   : num 374660
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 442820 442569 441902 442540 442820 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 442425 1181387
  .. .. .. .. ..@ ID       : chr "72"
  .. .. .. .. ..@ area     : num 374660
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 444165 1180938
  .. .. .. .. .. .. .. ..@ area   : num 167743
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 444320 444199 443938 444299 444320 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 444165 1180938
  .. .. .. .. ..@ ID       : chr "73"
  .. .. .. .. ..@ area     : num 167743
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 441323 1180615
  .. .. .. .. .. .. .. ..@ area   : num 22544
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 441360 441229 441291 441413 441360 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 441323 1180615
  .. .. .. .. ..@ ID       : chr "74"
  .. .. .. .. ..@ area     : num 22544
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 443366 1180143
  .. .. .. .. .. .. .. ..@ area   : num 12075
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 443400 443318 443317 443437 443400 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 443366 1180143
  .. .. .. .. ..@ ID       : chr "75"
  .. .. .. .. ..@ area     : num 12075
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 444265 1179194
  .. .. .. .. .. .. .. ..@ area   : num 504635
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 444360 443803 443999 444880 444360 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 444265 1179194
  .. .. .. .. ..@ ID       : chr "76"
  .. .. .. .. ..@ area     : num 504635
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 443518 1179820
  .. .. .. .. .. .. .. ..@ area   : num 13028
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 443480 443442 443559 443594 443480 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 443518 1179820
  .. .. .. .. ..@ ID       : chr "77"
  .. .. .. .. ..@ area     : num 13028
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 440215 1179458
  .. .. .. .. .. .. .. ..@ area   : num 171682
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 440200 440500 440382 439818 440200 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 440215 1179458
  .. .. .. .. ..@ ID       : chr "78"
  .. .. .. .. ..@ area     : num 171682
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 420798 1179338
  .. .. .. .. .. .. .. ..@ area   : num 14555
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 420760 420748 420852 420855 420760 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 420798 1179338
  .. .. .. .. ..@ ID       : chr "79"
  .. .. .. .. ..@ area     : num 14555
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 451653 1178587
  .. .. .. .. .. .. .. ..@ area   : num 39600
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 451500 451600 451720 451780 451500 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 451653 1178587
  .. .. .. .. ..@ ID       : chr "80"
  .. .. .. .. ..@ area     : num 39600
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 453497 1178576
  .. .. .. .. .. .. .. ..@ area   : num 14458
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 453460 453440 453520 453569 453460 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 453497 1178576
  .. .. .. .. ..@ ID       : chr "81"
  .. .. .. .. ..@ area     : num 14458
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 447221 1177792
  .. .. .. .. .. .. .. ..@ area   : num 15560
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 447280 447174 447150 447280 447280 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 447221 1177792
  .. .. .. .. ..@ ID       : chr "82"
  .. .. .. .. ..@ area     : num 15560
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 420148 1177780
  .. .. .. .. .. .. .. ..@ area   : num 15000
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 420180 420095 420098 420213 420180 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 420148 1177780
  .. .. .. .. ..@ ID       : chr "83"
  .. .. .. .. ..@ area     : num 15000
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 449240 1177481
  .. .. .. .. .. .. .. ..@ area   : num 85758
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 449100 449343 449380 449100 449100 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 449240 1177481
  .. .. .. .. ..@ ID       : chr "84"
  .. .. .. .. ..@ area     : num 85758
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 420118 1177480
  .. .. .. .. .. .. .. ..@ area   : num 16610
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 420140 420058 420078 420178 420140 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 420118 1177480
  .. .. .. .. ..@ ID       : chr "85"
  .. .. .. .. ..@ area     : num 16610
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 420365 1177419
  .. .. .. .. .. .. .. ..@ area   : num 15604
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 420340 420283 420358 420465 420340 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 420365 1177419
  .. .. .. .. ..@ ID       : chr "86"
  .. .. .. .. ..@ area     : num 15604
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 446629 1176214
  .. .. .. .. .. .. .. ..@ area   : num 779304
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 447240 446023 446437 447277 447240 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 446629 1176214
  .. .. .. .. ..@ ID       : chr "87"
  .. .. .. .. ..@ area     : num 779304
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 420689 1176752
  .. .. .. .. .. .. .. ..@ area   : num 116102
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 420820 420501 420564 420861 420820 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 420689 1176752
  .. .. .. .. ..@ ID       : chr "88"
  .. .. .. .. ..@ area     : num 116102
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 420349 1176510
  .. .. .. .. .. .. .. ..@ area   : num 61988
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 420400 420235 420315 420456 420400 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 420349 1176510
  .. .. .. .. ..@ ID       : chr "89"
  .. .. .. .. ..@ area     : num 61988
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 448389 1176403
  .. .. .. .. .. .. .. ..@ area   : num 25783
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 448440 448288 448369 448477 448440 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 448389 1176403
  .. .. .. .. ..@ ID       : chr "90"
  .. .. .. .. ..@ area     : num 25783
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 421882 1176157
  .. .. .. .. .. .. .. ..@ area   : num 28210
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 421920 421776 421872 421980 421920 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 421882 1176157
  .. .. .. .. ..@ ID       : chr "91"
  .. .. .. .. ..@ area     : num 28210
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 426509 1175828
  .. .. .. .. .. .. .. ..@ area   : num 25447
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 426540 426403 426446 426620 426540 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 426509 1175828
  .. .. .. .. ..@ ID       : chr "92"
  .. .. .. .. ..@ area     : num 25447
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 426998 1175198
  .. .. .. .. .. .. .. ..@ area   : num 20838
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 427100 426890 426878 427070 427100 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 426998 1175198
  .. .. .. .. ..@ ID       : chr "93"
  .. .. .. .. ..@ area     : num 20838
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 452597 1174782
  .. .. .. .. .. .. .. ..@ area   : num 50893
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 452620 452460 452596 452725 452620 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 452597 1174782
  .. .. .. .. ..@ ID       : chr "94"
  .. .. .. .. ..@ area     : num 50893
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 452288 1174496
  .. .. .. .. .. .. .. ..@ area   : num 18948
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 452240 452320 452352 452240 452240 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 452288 1174496
  .. .. .. .. ..@ ID       : chr "95"
  .. .. .. .. ..@ area     : num 18948
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 447692 1174143
  .. .. .. .. .. .. .. ..@ area   : num 93200
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 447740 447460 447780 447860 447740 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 447692 1174143
  .. .. .. .. ..@ ID       : chr "96"
  .. .. .. .. ..@ area     : num 93200
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 446585 1173377
  .. .. .. .. .. .. .. ..@ area   : num 263200
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 446500 446360 446660 446800 446500 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 446585 1173377
  .. .. .. .. ..@ ID       : chr "97"
  .. .. .. .. ..@ area     : num 263200
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 430100 1173700
  .. .. .. .. .. .. .. ..@ area   : num 23400
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 430140 429960 430200 430140 1173600 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 430100 1173700
  .. .. .. .. ..@ ID       : chr "98"
  .. .. .. .. ..@ area     : num 23400
  .. .. .. [list output truncated]
  .. ..@ plotOrder  : int [1:2002] 421 440 810 46 1343 1697 1560 1672 366 1607 ...
  .. ..@ bbox       : num [1:2, 1:2] 5665 5220 655580 1220241
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. .. ..@ projargs: chr NA
 $ ireland:Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  .. ..@ polygons   :List of 149
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 69184 628026
  .. .. .. .. .. .. .. ..@ area   : num 319455
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 68578 69098 69876 68578 627829 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 69184 628026
  .. .. .. .. ..@ ID       : chr "0"
  .. .. .. .. ..@ area     : num 319455
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 28
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -10970 381016
  .. .. .. .. .. .. .. ..@ area   : num 6.96e+10
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:2045, 1:2] -33817 -32619 -33171 -37855 -37302 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 5015 542297
  .. .. .. .. .. .. .. ..@ area   : num 959181
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 4395 4932 5621 5092 4395 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -97578 471238
  .. .. .. .. .. .. .. ..@ area   : num 1180501
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -98401 -96764 -97408 -97049 -97343 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -135115 432544
  .. .. .. .. .. .. .. ..@ area   : num 2836061
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -138984 -136399 -135085 -135643 -134717 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -140628 424187
  .. .. .. .. .. .. .. ..@ area   : num 198559
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -140831 -140680 -140445 -140525 -140831 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -111312 413971
  .. .. .. .. .. .. .. ..@ area   : num 202785
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -110826 -111397 -111708 -111407 -110826 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -62986 389276
  .. .. .. .. .. .. .. ..@ area   : num 95603
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -63614 -62591 -62754 -63614 389072 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -67015 344397
  .. .. .. .. .. .. .. ..@ area   : num 191298
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -67508 -66625 -66625 -67119 -67508 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -47425 330938
  .. .. .. .. .. .. .. ..@ area   : num 131274
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -47574 -47197 -47504 -47574 330277 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -58725 327926
  .. .. .. .. .. .. .. ..@ area   : num 250834
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -58464 -59102 -59055 -58459 -58464 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 94552 265146
  .. .. .. .. .. .. .. ..@ area   : num 3247452
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] 92620 95832 96000 96369 96574 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 17453 260014
  .. .. .. .. .. .. .. ..@ area   : num 132437
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 17533 17777 17303 16874 17533 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -2960 250217
  .. .. .. .. .. .. .. ..@ area   : num 328541
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -3655 -2884 -2476 -2704 -3655 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -144587 244022
  .. .. .. .. .. .. .. ..@ area   : num 713310
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -144521 -144243 -144539 -144370 -144474 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -24797 240912
  .. .. .. .. .. .. .. ..@ area   : num 134676
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -24860 -24619 -24913 -24860 240502 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -159961 238688
  .. .. .. .. .. .. .. ..@ area   : num 248276
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -160116 -159628 -159839 -160247 -160116 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -38459 232344
  .. .. .. .. .. .. .. ..@ area   : num 1536166
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -37256 -37326 -38349 -40290 -37256 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -57533 225915
  .. .. .. .. .. .. .. ..@ area   : num 64317
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -57819 -57353 -57427 -57819 225812 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -47478 221837
  .. .. .. .. .. .. .. ..@ area   : num 155243
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -47076 -47839 -48088 -47466 -47076 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -61433 217379
  .. .. .. .. .. .. .. ..@ area   : num 188076
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -61423 -61181 -61409 -61701 -61423 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -75235 213317
  .. .. .. .. .. .. .. ..@ area   : num 2348013
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -74192 -74006 -76365 -75951 -74430 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -139249 204707
  .. .. .. .. .. .. .. ..@ area   : num 185457
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -139987 -139004 -138813 -138905 -139987 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -106644 203946
  .. .. .. .. .. .. .. ..@ area   : num 540067
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -106220 -106434 -106980 -106961 -106220 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -3927 249801
  .. .. .. .. .. .. .. ..@ area   : num 160105
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -3937 -3862 -3655 -4097 -3937 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -36697 231296
  .. .. .. .. .. .. .. ..@ area   : num 117500
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -36255 -36579 -37256 -36255 230842 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -34982 231059
  .. .. .. .. .. .. .. ..@ area   : num 1333657
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -33817 -35877 -36255 -33930 -33817 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -12069 579082
  .. .. .. .. .. .. .. ..@ area   : num 57622
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -12073 -12243 -11892 -12073 579300 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -132298 429120
  .. .. .. .. .. .. .. ..@ area   : num 53529
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -131950 -132594 -132351 -131950 429331 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int [1:28] 1 11 4 21 17 26 3 2 14 23 ...
  .. .. .. .. ..@ labpt    : num [1:2] -10970 381016
  .. .. .. .. ..@ ID       : chr "1"
  .. .. .. .. ..@ area     : num 6.96e+10
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 4510 614589
  .. .. .. .. .. .. .. ..@ area   : num 2373321
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 5860 2786 2869 5644 6632 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 4510 614589
  .. .. .. .. ..@ ID       : chr "2"
  .. .. .. .. ..@ area     : num 2373321
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 131853 608464
  .. .. .. .. .. .. .. ..@ area   : num 14629742
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] 133608 132922 133512 131617 129293 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 131853 608464
  .. .. .. .. ..@ ID       : chr "3"
  .. .. .. .. ..@ area     : num 14629742
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 7039 604260
  .. .. .. .. .. .. .. ..@ area   : num 641651
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 6937 6394 7243 7613 6937 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 7039 604260
  .. .. .. .. ..@ ID       : chr "4"
  .. .. .. .. ..@ area     : num 641651
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 4
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 96688 533728
  .. .. .. .. .. .. .. ..@ area   : num 1.41e+10
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:355, 1:2] 169982 171216 170448 170001 169369 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 82442 590750
  .. .. .. .. .. .. .. ..@ area   : num 170612
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 81934 82415 82819 82695 81934 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 170210 510009
  .. .. .. .. .. .. .. ..@ area   : num 435788
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 169982 169967 170207 170609 169982 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 98103 595704
  .. .. .. .. .. .. .. ..@ area   : num 1161036
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 96450 96941 98089 99560 100248 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int [1:4] 1 4 3 2
  .. .. .. .. ..@ labpt    : num [1:2] 96688 533728
  .. .. .. .. ..@ ID       : chr "5"
  .. .. .. .. ..@ area     : num 1.41e+10
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 8486 602076
  .. .. .. .. .. .. .. ..@ area   : num 1911733
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 7167 8358 9338 9608 7167 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 8486 602076
  .. .. .. .. ..@ ID       : chr "6"
  .. .. .. .. ..@ area     : num 1911733
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -6109 595881
  .. .. .. .. .. .. .. ..@ area   : num 1649701
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -6013 -7141 -6553 -6946 -5648 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -6109 595881
  .. .. .. .. ..@ ID       : chr "7"
  .. .. .. .. ..@ area     : num 1649701
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 97786 595839
  .. .. .. .. .. .. .. ..@ area   : num 917211
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 96450 97981 100248 98089 96941 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 97786 595839
  .. .. .. .. ..@ ID       : chr "8"
  .. .. .. .. ..@ area     : num 917211
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -11897 592580
  .. .. .. .. .. .. .. ..@ area   : num 1122678
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -12095 -12603 -11553 -11272 -12095 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -11897 592580
  .. .. .. .. ..@ ID       : chr "9"
  .. .. .. .. ..@ area     : num 1122678
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -10222 589682
  .. .. .. .. .. .. .. ..@ area   : num 2293658
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -9977 -10427 -11467 -10381 -10535 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -10222 589682
  .. .. .. .. ..@ ID       : chr "10"
  .. .. .. .. ..@ area     : num 2293658
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 82442 590750
  .. .. .. .. .. .. .. ..@ area   : num 170612
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 81934 82695 82819 82415 81934 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 82442 590750
  .. .. .. .. ..@ ID       : chr "11"
  .. .. .. .. ..@ area     : num 170612
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 49330 588550
  .. .. .. .. .. .. .. ..@ area   : num 14372211
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] 50791 49114 47239 46893 47703 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 49330 588550
  .. .. .. .. ..@ ID       : chr "12"
  .. .. .. .. ..@ area     : num 14372211
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -17581 586199
  .. .. .. .. .. .. .. ..@ area   : num 17296736
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -15656 -15232 -17032 -17353 -20046 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -17581 586199
  .. .. .. .. ..@ ID       : chr "13"
  .. .. .. .. ..@ area     : num 17296736
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -10514 587339
  .. .. .. .. .. .. .. ..@ area   : num 109064
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -10819 -10580 -10141 -10819 587143 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -10514 587339
  .. .. .. .. ..@ ID       : chr "14"
  .. .. .. .. ..@ area     : num 109064
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 58227 581627
  .. .. .. .. .. .. .. ..@ area   : num 5.4e+07
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:20, 1:2] 54935 55423 57707 59070 59446 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 58227 581627
  .. .. .. .. ..@ ID       : chr "15"
  .. .. .. .. ..@ area     : num 5.4e+07
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -13836 585915
  .. .. .. .. .. .. .. ..@ area   : num 397515
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -13348 -13710 -14348 -13903 -13348 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -13836 585915
  .. .. .. .. ..@ ID       : chr "16"
  .. .. .. .. ..@ area     : num 397515
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -13170 585301
  .. .. .. .. .. .. .. ..@ area   : num 138953
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -13348 -12921 -13041 -13298 -13348 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -13170 585301
  .. .. .. .. ..@ ID       : chr "17"
  .. .. .. .. ..@ area     : num 138953
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -13103 584052
  .. .. .. .. .. .. .. ..@ area   : num 899113
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -12777 -13732 -13226 -12442 -12777 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -13103 584052
  .. .. .. .. ..@ ID       : chr "18"
  .. .. .. .. ..@ area     : num 899113
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -12533 581877
  .. .. .. .. .. .. .. ..@ area   : num 1029702
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -12372 -13370 -12340 -11874 -12372 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -12533 581877
  .. .. .. .. ..@ ID       : chr "19"
  .. .. .. .. ..@ area     : num 1029702
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -12069 579082
  .. .. .. .. .. .. .. ..@ area   : num 57622
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -12073 -11892 -12243 -12073 579300 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -12069 579082
  .. .. .. .. ..@ ID       : chr "20"
  .. .. .. .. ..@ area     : num 57622
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 5126 542322
  .. .. .. .. .. .. .. ..@ area   : num 1245004
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 4395 5092 5789 5621 4932 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 5126 542322
  .. .. .. .. ..@ ID       : chr "21"
  .. .. .. .. ..@ area     : num 1245004
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 172157 537289
  .. .. .. .. .. .. .. ..@ area   : num 1069100
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 172472 171411 171951 172869 172472 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 172157 537289
  .. .. .. .. ..@ ID       : chr "22"
  .. .. .. .. ..@ area     : num 1069100
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -31844 525149
  .. .. .. .. .. .. .. ..@ area   : num 474391
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -31477 -32341 -32331 -31360 -31477 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -31844 525149
  .. .. .. .. ..@ ID       : chr "23"
  .. .. .. .. ..@ area     : num 474391
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 164204 518757
  .. .. .. .. .. .. .. ..@ area   : num 1729934
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 164205 163648 163461 164333 164809 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 164204 518757
  .. .. .. .. ..@ ID       : chr "24"
  .. .. .. .. ..@ area     : num 1729934
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 165055 512328
  .. .. .. .. .. .. .. ..@ area   : num 558601
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 164610 164808 165496 165406 164610 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 165055 512328
  .. .. .. .. ..@ ID       : chr "25"
  .. .. .. .. ..@ area     : num 558601
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 170210 510009
  .. .. .. .. .. .. .. ..@ area   : num 435788
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 169982 170609 170207 169967 169982 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 170210 510009
  .. .. .. .. ..@ ID       : chr "26"
  .. .. .. .. ..@ area     : num 435788
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -28547 509630
  .. .. .. .. .. .. .. ..@ area   : num 1224296
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -28719 -29090 -28649 -27849 -28719 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -28547 509630
  .. .. .. .. ..@ ID       : chr "27"
  .. .. .. .. ..@ area     : num 1224296
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 163889 508952
  .. .. .. .. .. .. .. ..@ area   : num 454794
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 163527 163691 164158 164053 163527 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 163889 508952
  .. .. .. .. ..@ ID       : chr "28"
  .. .. .. .. ..@ area     : num 454794
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -29984 508556
  .. .. .. .. .. .. .. ..@ area   : num 118999
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -30048 -30207 -29989 -29719 -30048 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -29984 508556
  .. .. .. .. ..@ ID       : chr "29"
  .. .. .. .. ..@ area     : num 118999
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -66462 503789
  .. .. .. .. .. .. .. ..@ area   : num 538096
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -65475 -66357 -68782 -66080 -65475 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -66462 503789
  .. .. .. .. ..@ ID       : chr "30"
  .. .. .. .. ..@ area     : num 538096
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 162252 503961
  .. .. .. .. .. .. .. ..@ area   : num 392300
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 161965 161957 162668 162504 161965 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 162252 503961
  .. .. .. .. ..@ ID       : chr "31"
  .. .. .. .. ..@ area     : num 392300
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -135142 502613
  .. .. .. .. .. .. .. ..@ area   : num 1591749
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -135703 -135283 -135856 -135363 -134369 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -135142 502613
  .. .. .. .. ..@ ID       : chr "32"
  .. .. .. .. ..@ area     : num 1591749
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -136890 500532
  .. .. .. .. .. .. .. ..@ area   : num 1805981
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -137971 -137476 -135911 -136009 -137971 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -136890 500532
  .. .. .. .. ..@ ID       : chr "33"
  .. .. .. .. ..@ area     : num 1805981
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -134450 495588
  .. .. .. .. .. .. .. ..@ area   : num 459512
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -134653 -135176 -133722 -133959 -134653 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -134450 495588
  .. .. .. .. ..@ ID       : chr "34"
  .. .. .. .. ..@ area     : num 459512
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -126760 482667
  .. .. .. .. .. .. .. ..@ area   : num 1.46e+08
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:45, 1:2] -123610 -122016 -119428 -121486 -121251 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -126760 482667
  .. .. .. .. ..@ ID       : chr "35"
  .. .. .. .. ..@ area     : num 1.46e+08
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -117658 484871
  .. .. .. .. .. .. .. ..@ area   : num 2327702
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -118419 -119028 -116362 -117166 -118375 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -117658 484871
  .. .. .. .. ..@ ID       : chr "36"
  .. .. .. .. ..@ area     : num 2327702
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -115315 483195
  .. .. .. .. .. .. .. ..@ area   : num 2106011
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -115763 -115899 -115036 -114618 -115763 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -115315 483195
  .. .. .. .. ..@ ID       : chr "37"
  .. .. .. .. ..@ area     : num 2106011
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -97543 471101
  .. .. .. .. .. .. .. ..@ area   : num 851580
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -98401 -97265 -97343 -97049 -97408 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -97543 471101
  .. .. .. .. ..@ ID       : chr "38"
  .. .. .. .. ..@ area     : num 851580
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -122611 470731
  .. .. .. .. .. .. .. ..@ area   : num 1445996
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -122042 -123396 -123249 -121864 -122042 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -122611 470731
  .. .. .. .. ..@ ID       : chr "39"
  .. .. .. .. ..@ area     : num 1445996
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -97019 469505
  .. .. .. .. .. .. .. ..@ area   : num 264442
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -96817 -97032 -97415 -96745 -96817 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -97019 469505
  .. .. .. .. ..@ ID       : chr "40"
  .. .. .. .. ..@ area     : num 264442
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -97707 469303
  .. .. .. .. .. .. .. ..@ area   : num 397962
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -97415 -97032 -98244 -98297 -97415 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -97707 469303
  .. .. .. .. ..@ ID       : chr "41"
  .. .. .. .. ..@ area     : num 397962
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -98758 466778
  .. .. .. .. .. .. .. ..@ area   : num 676788
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -98212 -98445 -99414 -98984 -98212 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -98758 466778
  .. .. .. .. ..@ ID       : chr "42"
  .. .. .. .. ..@ area     : num 676788
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -101279 466989
  .. .. .. .. .. .. .. ..@ area   : num 37868
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -101032 -101513 -101291 -101032 466832 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -101279 466989
  .. .. .. .. ..@ ID       : chr "43"
  .. .. .. .. ..@ area     : num 37868
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -126361 464160
  .. .. .. .. .. .. .. ..@ area   : num 15469664
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -124367 -125738 -130246 -129574 -126265 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -126361 464160
  .. .. .. .. ..@ ID       : chr "44"
  .. .. .. .. ..@ area     : num 15469664
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -100481 466632
  .. .. .. .. .. .. .. ..@ area   : num 215004
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -101032 -99924 -99883 -100901 -101032 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -100481 466632
  .. .. .. .. ..@ ID       : chr "45"
  .. .. .. .. ..@ area     : num 215004
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -103216 466253
  .. .. .. .. .. .. .. ..@ area   : num 51939
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -103605 -103053 -102989 -103605 466120 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -103216 466253
  .. .. .. .. ..@ ID       : chr "46"
  .. .. .. .. ..@ area     : num 51939
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -104098 465775
  .. .. .. .. .. .. .. ..@ area   : num 525458
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -103605 -103688 -104710 -104299 -103605 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -104098 465775
  .. .. .. .. ..@ ID       : chr "47"
  .. .. .. .. ..@ area     : num 525458
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -102075 464859
  .. .. .. .. .. .. .. ..@ area   : num 630277
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -101871 -102530 -101970 -102940 -101630 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -102075 464859
  .. .. .. .. ..@ ID       : chr "48"
  .. .. .. .. ..@ area     : num 630277
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -102339 462844
  .. .. .. .. .. .. .. ..@ area   : num 179671
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -101908 -102272 -102884 -102245 -101908 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -102339 462844
  .. .. .. .. ..@ ID       : chr "49"
  .. .. .. .. ..@ area     : num 179671
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -102060 461716
  .. .. .. .. .. .. .. ..@ area   : num 108178
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -101908 -101916 -102357 -101908 461987 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -102060 461716
  .. .. .. .. ..@ ID       : chr "50"
  .. .. .. .. ..@ area     : num 108178
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -97971 461114
  .. .. .. .. .. .. .. ..@ area   : num 39013
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -98290 -97844 -97780 -98290 461191 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -97971 461114
  .. .. .. .. ..@ ID       : chr "51"
  .. .. .. .. ..@ area     : num 39013
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -129479 454880
  .. .. .. .. .. .. .. ..@ area   : num 488035
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -129167 -129796 -129960 -129075 -129167 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -129479 454880
  .. .. .. .. ..@ ID       : chr "52"
  .. .. .. .. ..@ area     : num 488035
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -134922 453974
  .. .. .. .. .. .. .. ..@ area   : num 5650631
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -136780 -135836 -133690 -132864 -133659 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -134922 453974
  .. .. .. .. ..@ ID       : chr "53"
  .. .. .. .. ..@ area     : num 5650631
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -142710 445577
  .. .. .. .. .. .. .. ..@ area   : num 9644647
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -141572 -144382 -145645 -144768 -143736 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -142710 445577
  .. .. .. .. ..@ ID       : chr "54"
  .. .. .. .. ..@ area     : num 9644647
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -147446 445000
  .. .. .. .. .. .. .. ..@ area   : num 1452070
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -148431 -147747 -146180 -147787 -148431 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -147446 445000
  .. .. .. .. ..@ ID       : chr "55"
  .. .. .. .. ..@ area     : num 1452070
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -128799 437140
  .. .. .. .. .. .. .. ..@ area   : num 745657
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -128001 -129056 -129605 -128450 -128001 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -128799 437140
  .. .. .. .. ..@ ID       : chr "56"
  .. .. .. .. ..@ area     : num 745657
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -140674 435273
  .. .. .. .. .. .. .. ..@ area   : num 1845408
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -140838 -141614 -140422 -139780 -140838 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -140674 435273
  .. .. .. .. ..@ ID       : chr "57"
  .. .. .. .. ..@ area     : num 1845408
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -134986 432507
  .. .. .. .. .. .. .. ..@ area   : num 2983148
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -138984 -136446 -135488 -134999 -132948 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -134986 432507
  .. .. .. .. ..@ ID       : chr "58"
  .. .. .. .. ..@ area     : num 2983148
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -140177 433082
  .. .. .. .. .. .. .. ..@ area   : num 569678
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -140502 -140505 -140247 -139570 -140502 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -140177 433082
  .. .. .. .. ..@ ID       : chr "59"
  .. .. .. .. ..@ area     : num 569678
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -139858 431761
  .. .. .. .. .. .. .. ..@ area   : num 288492
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -139987 -140613 -139167 -139298 -139987 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -139858 431761
  .. .. .. .. ..@ ID       : chr "60"
  .. .. .. .. ..@ area     : num 288492
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -132298 429120
  .. .. .. .. .. .. .. ..@ area   : num 53529
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -131950 -132351 -132594 -131950 429331 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -132298 429120
  .. .. .. .. ..@ ID       : chr "61"
  .. .. .. .. ..@ area     : num 53529
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -140628 424187
  .. .. .. .. .. .. .. ..@ area   : num 198559
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -140831 -140525 -140445 -140680 -140831 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -140628 424187
  .. .. .. .. ..@ ID       : chr "62"
  .. .. .. .. ..@ area     : num 198559
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -144054 421965
  .. .. .. .. .. .. .. ..@ area   : num 37802
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -143696 -144270 -144196 -143696 421887 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -144054 421965
  .. .. .. .. ..@ ID       : chr "63"
  .. .. .. .. ..@ area     : num 37802
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -124952 417601
  .. .. .. .. .. .. .. ..@ area   : num 2050628
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -124884 -125547 -124859 -125031 -124139 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -124952 417601
  .. .. .. .. ..@ ID       : chr "64"
  .. .. .. .. ..@ area     : num 2050628
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -111312 413971
  .. .. .. .. .. .. .. ..@ area   : num 202785
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -110826 -111407 -111708 -111397 -110826 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -111312 413971
  .. .. .. .. ..@ ID       : chr "65"
  .. .. .. .. ..@ area     : num 202785
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -130566 410970
  .. .. .. .. .. .. .. ..@ area   : num 748182
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -130407 -130966 -130997 -129929 -130407 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -130566 410970
  .. .. .. .. ..@ ID       : chr "66"
  .. .. .. .. ..@ area     : num 748182
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -123157 407939
  .. .. .. .. .. .. .. ..@ area   : num 1533861
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -122370 -122901 -124238 -122968 -122340 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -123157 407939
  .. .. .. .. ..@ ID       : chr "67"
  .. .. .. .. ..@ area     : num 1533861
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -110755 408198
  .. .. .. .. .. .. .. ..@ area   : num 1111200
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -111540 -111367 -109994 -109905 -111540 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -110755 408198
  .. .. .. .. ..@ ID       : chr "68"
  .. .. .. .. ..@ area     : num 1111200
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -108575 407263
  .. .. .. .. .. .. .. ..@ area   : num 1815362
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -108197 -109695 -108468 -107631 -108197 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -108575 407263
  .. .. .. .. ..@ ID       : chr "69"
  .. .. .. .. ..@ area     : num 1815362
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 133237 407129
  .. .. .. .. .. .. .. ..@ area   : num 2257280
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 133390 132134 132638 134558 133390 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 133237 407129
  .. .. .. .. ..@ ID       : chr "70"
  .. .. .. .. ..@ area     : num 2257280
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -111345 405537
  .. .. .. .. .. .. .. ..@ area   : num 9163873
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -112263 -113899 -112242 -113447 -112915 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -111345 405537
  .. .. .. .. ..@ ID       : chr "71"
  .. .. .. .. ..@ area     : num 9163873
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -120357 406554
  .. .. .. .. .. .. .. ..@ area   : num 341860
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -120991 -120371 -119659 -120246 -120991 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -120357 406554
  .. .. .. .. ..@ ID       : chr "72"
  .. .. .. .. ..@ area     : num 341860
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -113923 403763
  .. .. .. .. .. .. .. ..@ area   : num 789794
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -114013 -114475 -114162 -113218 -114013 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -113923 403763
  .. .. .. .. ..@ ID       : chr "73"
  .. .. .. .. ..@ area     : num 789794
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -111855 400617
  .. .. .. .. .. .. .. ..@ area   : num 23324604
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -113447 -114465 -114671 -111879 -111982 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -111855 400617
  .. .. .. .. ..@ ID       : chr "74"
  .. .. .. .. ..@ area     : num 23324604
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -116166 401412
  .. .. .. .. .. .. .. ..@ area   : num 787684
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -116150 -116570 -116351 -115624 -116150 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -116166 401412
  .. .. .. .. ..@ ID       : chr "75"
  .. .. .. .. ..@ area     : num 787684
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -115894 399336
  .. .. .. .. .. .. .. ..@ area   : num 3898744
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -117357 -116290 -114715 -115036 -117357 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -115894 399336
  .. .. .. .. ..@ ID       : chr "76"
  .. .. .. .. ..@ area     : num 3898744
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 123494 394077
  .. .. .. .. .. .. .. ..@ area   : num 2724848
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 122369 121842 123145 125888 122369 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] 123494 394077
  .. .. .. .. ..@ ID       : chr "77"
  .. .. .. .. ..@ area     : num 2724848
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -65726 394199
  .. .. .. .. .. .. .. ..@ area   : num 701863
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -64569 -64777 -67219 -65814 -64569 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -65726 394199
  .. .. .. .. ..@ ID       : chr "78"
  .. .. .. .. ..@ area     : num 701863
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -68874 392513
  .. .. .. .. .. .. .. ..@ area   : num 1155468
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -68367 -70007 -70003 -67499 -68714 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -68874 392513
  .. .. .. .. ..@ ID       : chr "79"
  .. .. .. .. ..@ area     : num 1155468
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -65650 390897
  .. .. .. .. .. .. .. ..@ area   : num 379094
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -66358 -65812 -64893 -65600 -66358 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -65650 390897
  .. .. .. .. ..@ ID       : chr "80"
  .. .. .. .. ..@ area     : num 379094
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -116661 387018
  .. .. .. .. .. .. .. ..@ area   : num 33528731
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -111433 -123322 -123261 -121763 -120289 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -116661 387018
  .. .. .. .. ..@ ID       : chr "81"
  .. .. .. .. ..@ area     : num 33528731
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -62986 389276
  .. .. .. .. .. .. .. ..@ area   : num 95603
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -63614 -62754 -62591 -63614 389072 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -62986 389276
  .. .. .. .. ..@ ID       : chr "82"
  .. .. .. .. ..@ area     : num 95603
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -72274 386557
  .. .. .. .. .. .. .. ..@ area   : num 1342218
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -72444 -73268 -71982 -71365 -72444 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -72274 386557
  .. .. .. .. ..@ ID       : chr "83"
  .. .. .. .. ..@ area     : num 1342218
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -75266 384862
  .. .. .. .. .. .. .. ..@ area   : num 292661
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -74885 -75012 -75545 -75554 -74885 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -75266 384862
  .. .. .. .. ..@ ID       : chr "84"
  .. .. .. .. ..@ area     : num 292661
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -108047 381286
  .. .. .. .. .. .. .. ..@ area   : num 9361510
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -109328 -109960 -109616 -108515 -106619 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -108047 381286
  .. .. .. .. ..@ ID       : chr "85"
  .. .. .. .. ..@ area     : num 9361510
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -104196 377865
  .. .. .. .. .. .. .. ..@ area   : num 5587490
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -104109 -106084 -105236 -103014 -102351 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -104196 377865
  .. .. .. .. ..@ ID       : chr "86"
  .. .. .. .. ..@ area     : num 5587490
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -106512 350695
  .. .. .. .. .. .. .. ..@ area   : num 1168360
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -105874 -107248 -107413 -105678 -105874 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -106512 350695
  .. .. .. .. ..@ ID       : chr "87"
  .. .. .. .. ..@ area     : num 1168360
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -67015 344397
  .. .. .. .. .. .. .. ..@ area   : num 191298
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -67508 -67119 -66625 -66625 -67508 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -67015 344397
  .. .. .. .. ..@ ID       : chr "88"
  .. .. .. .. ..@ area     : num 191298
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -74837 336567
  .. .. .. .. .. .. .. ..@ area   : num 1433705
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -75576 -76185 -74657 -73363 -75576 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -74837 336567
  .. .. .. .. ..@ ID       : chr "89"
  .. .. .. .. ..@ area     : num 1433705
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -72736 336027
  .. .. .. .. .. .. .. ..@ area   : num 1046948
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -73548 -73194 -71767 -72083 -73548 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -72736 336027
  .. .. .. .. ..@ ID       : chr "90"
  .. .. .. .. ..@ area     : num 1046948
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -68837 335343
  .. .. .. .. .. .. .. ..@ area   : num 713215
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -69183 -69635 -68053 -68097 -69183 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -68837 335343
  .. .. .. .. ..@ ID       : chr "91"
  .. .. .. .. ..@ area     : num 713215
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -70430 335278
  .. .. .. .. .. .. .. ..@ area   : num 605955
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -70578 -71044 -69848 -69853 -70578 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -70430 335278
  .. .. .. .. ..@ ID       : chr "92"
  .. .. .. .. ..@ area     : num 605955
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -76062 333274
  .. .. .. .. .. .. .. ..@ area   : num 791867
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -76564 -76863 -75513 -75346 -76564 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -76062 333274
  .. .. .. .. ..@ ID       : chr "93"
  .. .. .. .. ..@ area     : num 791867
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -75939 332170
  .. .. .. .. .. .. .. ..@ area   : num 857370
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -76591 -76103 -75207 -75842 -76591 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -75939 332170
  .. .. .. .. ..@ ID       : chr "94"
  .. .. .. .. ..@ area     : num 857370
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -59393 331430
  .. .. .. .. .. .. .. ..@ area   : num 51140
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -59313 -59223 -59505 -59549 -59313 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -59393 331430
  .. .. .. .. ..@ ID       : chr "95"
  .. .. .. .. ..@ area     : num 51140
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -47425 330938
  .. .. .. .. .. .. .. ..@ area   : num 131274
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] -47574 -47504 -47197 -47574 330277 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -47425 330938
  .. .. .. .. ..@ ID       : chr "96"
  .. .. .. .. ..@ area     : num 131274
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -108719 328661
  .. .. .. .. .. .. .. ..@ area   : num 661332
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -109000 -109078 -108357 -108348 -109000 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -108719 328661
  .. .. .. .. ..@ ID       : chr "97"
  .. .. .. .. ..@ area     : num 661332
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 1
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -58725 327926
  .. .. .. .. .. .. .. ..@ area   : num 250834
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -58464 -58459 -59055 -59102 -58464 ...
  .. .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. .. .. .. .. ..$ : NULL
  .. .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. .. ..@ plotOrder: int 1
  .. .. .. .. ..@ labpt    : num [1:2] -58725 327926
  .. .. .. .. ..@ ID       : chr "98"
  .. .. .. .. ..@ area     : num 250834
  .. .. .. [list output truncated]
  .. ..@ plotOrder  : int [1:149] 2 6 36 16 82 111 121 75 130 14 ...
  .. ..@ bbox       : num [1:2, 1:2] -190402 196447 177561 628350
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. .. ..@ projargs: chr NA
 $ ci     :Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  .. ..@ data       :'data.frame':	1 obs. of  4 variables:
  .. .. ..$ SP_ID     : Factor w/ 1 level "Channel Islands": 1
  .. .. ..$ LEVEL_NAME: Factor w/ 1 level "Channel Islands": 1
  .. .. ..$ REGION_NAM: Factor w/ 1 level "Northern Europe": 1
  .. .. ..$ CONTINENT : Factor w/ 1 level "EUROPE": 1
  .. .. ..- attr(*, "data_types")= chr [1:4] "C" "C" "C" "C"
  .. ..@ polygons   :List of 1
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 9
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 390485 -76095
  .. .. .. .. .. .. .. ..@ area   : num 1.22e+08
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:59, 1:2] 381688 381820 382397 383229 383679 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 385645 -20346
  .. .. .. .. .. .. .. ..@ area   : num 9103471
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] 383129 383279 384255 385270 385376 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 381688 -18585
  .. .. .. .. .. .. .. ..@ area   : num 165834
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 381383 381426 382123 381678 381383 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 373785 -52040
  .. .. .. .. .. .. .. ..@ area   : num 5973650
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] 372323 372411 373284 373202 372629 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 371890 -51918
  .. .. .. .. .. .. .. ..@ area   : num 423246
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 371418 371633 372398 372012 371418 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 367539 -47217
  .. .. .. .. .. .. .. ..@ area   : num 1388415
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] 366856 367772 368061 367376 367234 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 366581 -48854
  .. .. .. .. .. .. .. ..@ area   : num 142933
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] 366310 366696 366801 366521 366310 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 358085 -48956
  .. .. .. .. .. .. .. ..@ area   : num 66390267
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:62, 1:2] 351124 351216 352362 352815 352951 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] 351658 -48510
  .. .. .. .. .. .. .. ..@ area   : num 159654
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:4, 1:2] 351326 351795 351853 351326 -48442 ...
  .. .. .. .. ..@ plotOrder: int [1:9] 1 8 2 4 6 5 3 9 7
  .. .. .. .. ..@ labpt    : num [1:2] 390485 -76095
  .. .. .. .. ..@ ID       : chr "0"
  .. .. .. .. ..@ area     : num 2.06e+08
  .. ..@ plotOrder  : int 1
  .. ..@ bbox       : num [1:2, 1:2] 351124 -81858 398919 -18427
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. .. ..@ projargs: chr NA
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(UK)
## maybe str(UK) ; plot(UK) ...
}
\keyword{datasets}
