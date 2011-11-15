\name{Fertility}
\alias{Fertility}
\docType{data}
\title{
Swedish fertility rates in Lexis triangles, calculated from HFD data.
}
\description{
Swedish fertility rates in Lexis triangles, ages 13-54, years 1891-2008. These were calculated by simply dividing Births by exposure, both of which are available in lexis triangles from the Human Fertility Database. No other adjustments were made.
}
\usage{data(Fertility)}
\format{
  The format is:
List of 2
 $ LowerTriangles: num [1:42, 1:118] 7.69e-06 1.92e-05 2.73e-04 1.67e-03 5.33e-03 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:42] "13" "14" "15" "16" ...
  .. ..$ : chr [1:118] "1891" "1892" "1893" "1894" ...
 $ UpperTriangles: num [1:42, 1:118] 3.38e-05 2.29e-05 6.39e-04 2.55e-03 7.38e-03 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:42] "13" "14" "15" "16" ...
  .. ..$ : chr [1:118] "1891" "1892" "1893" "1894" ...
}
\source{
Human Fertility Database. Max Planck Institute for Demographic Research (Germany) and Vienna Institute of Demography (Austria). Available at www.humanfertility.org (data downloaded on 28-7-2011).
}
\examples{
# see examples for function \code{LexisSurface()}.
}
\keyword{datasets}
