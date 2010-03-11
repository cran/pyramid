\name{pyramid}
\alias{pyramid}
\title{Drawing population pyramid using data.frame}
\description{
  Drawing population pyramid using data.frame.
  Detailed explanation is given in Japanese
  at http://phi.med.gunma-u.ac.jp/swtips/R.html#PYRAMID.
}
\usage{ pyramid(data, Laxis=NULL, Raxis=NULL, 
 AxisFM="g", AxisBM="", AxisBI=3, Cgap=0.3, Cstep=1, Csize=1, 
 Llab="Males", Rlab="Females", Clab="Ages", GL=TRUE, Cadj=-0.03, 
 Lcol="Cyan", Rcol="Pink", Ldens=-1, Rdens=-1, main="", ...)
}
\arguments{
 \item{data}{A data.frame including left pyramid numbers in the 1st column and
 and right pyramid numbers in the 2nd column, where the numbers of males in 
 each age-class are usually given to left numbers and those of females are to
 right numbers.  If the data.frame includes 3rd column, it is used as age-class
 labels, otherwise the row.names(data) is used as age-class labels.}
 \item{Laxis}{A vector of axis for left pyramid.  If missing, automatically given.}
 \item{Raxis}{A vector of axis for right pyramid.  If missing, Laxis is used.}
 \item{AxisFM}{A format code of formatC for plotting axis.  If missing, "g" is used.}
 \item{AxisBM}{A big.mark of formatC for plotting axis.  If missing, none.}
 \item{AxisBI}{A big.interval number of formatC for plotting axis.  Default is 3}
 \item{Cgap}{The width of center gap (as ratio to each panel) to draw age-class. Default is 0.3}
 \item{Cstep}{The interval to write the labels of age classes. Default is 1}
 \item{Csize}{The font size factor to write the labels of age classes. Default is 1}
 \item{Cadj}{The vertical adjustment factor for the labels of age classes. Default is -0.03}
 \item{Llab}{The label of the left pyramid.  Default is "Males".}
 \item{Rlab}{The label of the right pyramid.  Default is "Females".}
 \item{Clab}{The label of the center age-class.  Default is "Ages".}
 \item{GL}{Logical value to draw the vertical dotted lines.  Default is TRUE.}
 \item{Lcol}{The color of the left pyramid.  Default is "Cyan".}
 \item{Ldens}{The density of hatching lines (/inch) for left pyramid.
 Default is -1, when the pyramid will be filled.}
 \item{Rcol}{The color of the right pyramid.  Default is "Pink".}
 \item{Rdens}{The density of hatching lines (/inch) for right pyramid.
 Default is -1, when the pyramid will be filled.}
 \item{main}{The main title of the pyramid.}
 \item{...}{Other options.}
}
\author{Minato Nakazawa \email{minato-nakazawa@umin.net} \url{http://phi.med.gunma-u.ac.jp/}}
\examples{
ages <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-')
males <- c(34,19,11,11,8,7,5)
females <- c(26,25,16,11,7,5,1)
data <- data.frame(males,females,ages)
pyramid(data)
# another example
py.Males <- c(80,40,30,20,10)
names(py.Males) <- c('0-9','10-19','20-29','30-39','40-')
py.Females <- c(60,50,40,30,5)
names(py.Females) <- names(py.Males)
py.df <- data.frame(py.Females,py.Males)
pyramid(py.df,Llab="Females",Rlab="Males",Lcol="navy", Ldens=5, Rcol="red", 
 Rdens=10, GL=FALSE,main="An example of population pyramid\n with auto-axis")
# The census 2005 for Gunma prefecture data obtained from
# http://www.e-stat.go.jp/SG1/estat/List.do?bid=000001005042&cycode=0
# as "a00411.xls"
GunmaPop2005 <- data.frame(
 Males=c(8872, 9144, 9528, 9812, 9817, 10049, 10234, 10047, 10222, 
 10187, 10319, 10420, 10135, 10473, 10219, 10492, 10943, 11243, 10579, 
 9653, 9941, 10252, 10396, 10649, 11343, 12031, 12420, 13015, 13354, 
 14132, 14624, 15873, 16013, 15809, 15338, 14876, 14568, 14536, 14580, 
 10713, 13844, 12911, 12303, 12158, 12086, 12117, 12620, 12331, 12376, 
 13138, 14144, 13553, 14353, 15194, 16064, 17286, 18561, 18347, 18230, 
 12001, 11938, 14363, 13684, 14122, 13475, 12241, 10279, 10866, 10939, 
 10853, 10093, 9996, 9575, 9334, 9147, 8643, 8249, 7760, 7353, 6812, 
 6170, 5041, 4099, 3206, 2711, 2778, 2113, 1895, 1630, 1431, 1146, 966, 
 675, 559, 384, 263, 212, 133, 84, 38, 24, 15, 11, 6, 1, 2, 1, 0, 0),
Females=c(8323, 8750, 8964, 9359, 9559, 9605, 9511, 9800, 9790, 9848, 
 9939, 9755, 9696, 9884, 9734, 9767, 10293, 10616, 10040, 9383, 9731, 
 9748, 10211, 10204, 10500, 11086, 11758, 12248, 12548, 13524, 13907, 
 14930, 15129, 15057, 14344, 13982, 13942, 13587, 13972, 10359, 13212, 
 12435, 11934, 11673, 11668, 11583, 12100, 11867, 11917, 12746, 13364, 
 13170, 13968, 15318, 16251, 17125, 18253, 18042, 17927, 11981, 11773, 
 14450, 14124, 14438, 13502, 12960, 10729, 11710, 11697, 11884, 11413, 
 11442, 11087, 11035, 11209, 10646, 10482, 9784, 9777, 9491, 8891, 8188, 
 7636, 7034, 6123, 6103, 4577, 4415, 3861, 3426, 3035, 2571, 2064, 1683, 
 1209, 878, 739, 536, 333, 193, 134, 86, 56, 28, 13, 8, 5, 3, 1),
Ages=0:108)
pyramid(GunmaPop2005,Llab="Males",Rlab="Females",Clab="",Laxis=seq(0,20000,len=5),
 AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
 main="Population pyramid of Gunma Prefecture\n (Data: Census 2005, total by gender)")
}
\keyword{hplot}
