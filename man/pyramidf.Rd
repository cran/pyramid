\name{pyramidf}
\alias{pyramidf}
\title{Drawing population pyramid using data.frame within the specified frame}
\description{
  Drawing population pyramid using data.frame.
  Detailed explanation is given in Japanese
  at http://minato.sip21c.org/swtips/R.html#PYRAMID.
}
\usage{ pyramidf(data, Laxis=NULL, Raxis=NULL, 
 frame=c(-1.15, 1.15, -0.05, 1.1), 
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
 \item{Laxis}{A vector of axis for left pyramid.  If missing, automatically given using pretty().}
 \item{Raxis}{A vector of axis for right pyramid.  If missing, Laxis is used.}
 \item{frame}{A vector of frame c(x-left, x-right, y-bottom, y-top), to draw pyramid.  Default frame is same as }
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
\author{Minato Nakazawa \email{minato-nakazawa@umin.net} \url{http://minato.sip21c.org/}}
\examples{
# GunmaPop2005 and GumaPop2010 are included in this package.
plot(c(0,100), c(0,100), type="n", frame=FALSE, axes=FALSE, xlab="", ylab="", 
 main="Population pyramid of Gunma prefecture")
# overlay
pyramidf(GunmaPop2005, frame=c(10, 75, 0, 90), Clab="", Lcol="skyblue", Rcol="pink",
 Cstep=10, Laxis=0:4*5000, AxisFM="d")
pyramidf(GunmaPop2010, frame=c(10, 75, 0, 90), Clab="", Lcol="deepskyblue", Rcol="deeppink",
 Ldens=10, Rdens=10, Cstep=10, Laxis=0:4*5000, AxisFM="d")
legend("right", fill=c("skyblue","pink","deepskyblue","deeppink"), density=c(NA,NA,10,10), 
 legend=c("Male 2005", "Female 2005", "Male 2010", "Female 2010"), cex=0.8)
}
\keyword{hplot}
