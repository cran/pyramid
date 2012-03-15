\name{pyramid}
\alias{pyramid}
\title{Drawing population pyramid using data.frame}
\description{
  Drawing population pyramid using data.frame.
  Detailed explanation is given in Japanese
  at http://minato.sip21c.org/swtips/R.html#PYRAMID.
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
\author{Minato Nakazawa \email{minato-nakazawa@umin.net} \url{http://minato.sip21c.org/}}
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
# GunmaPop2005 is included in this package.
pyramid(GunmaPop2005,Llab="Males",Rlab="Females",Clab="",Laxis=seq(0,20000,len=5),
 AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
 main="Population pyramid of Gunma Prefecture\n (Data: Census 2005, total by gender)")
}
\keyword{hplot}
