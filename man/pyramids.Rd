\name{pyramids}
\alias{pyramids}
\title{Drawing population pyramid using vectors}
\description{
  Drawing population pyramid using vectors.
  Detailed explanation is given in Japanese
  at http://minato.sip21c.org/swtips/R.html#PYRAMID.
}
\usage{
pyramids(Left, Right, Center=NULL, ...)
}
\arguments{
 \item{Left}{A vector of the numbers of people given for the left pyramid.}
 \item{Right}{A vector of the numbers of people given for the right pyramid.}
 \item{Center}{If given, a vector of the age-class labels to be drawn in center.
 If not given, names(Left) is used instead.}
 \item{...}{This is the lapper function for pyramid().  All options for pyramid() can be passed.}
}
\author{Minato Nakazawa \email{minato-nakazawa@umin.net} \url{http://minato.sip21c.org/}}
\examples{
 py.Males <- c(80,40,30,20,10)
 names(py.Males) <- c('0-9','10-19','20-29','30-39','40-')
 py.Females <- c(60,50,40,30,5)
 pyramids(Left=py.Males,Llab="Males",Right=py.Females,Rlab="Females",
  Laxis=c(0,50,100),main="An example of population pyramid\n with fixed axis")
}
\keyword{hplot}
