\name{Robots}
\alias{Robots}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("Robots")}
\format{
  The format is:
Class 'R6ClassGenerator' <Robots> object generator
  Public:
    nrow: 20
    ncol: 50
    player: NA NA
    robots: 
    piles: 
    robot.pch: 3
    player.pch: 16
    pile.pch: 8
    robot.char: +
    player.char: O
    pile.char: *
    auto.plot: TRUE
    auto.print: FALSE
    safe.teleport: TRUE
    move.mat: NW W SW N . S NE E SE
    initialize: function (nrobots = 10) 
    plot: function () 
    print: function () 
    move: function (dir) 
    clone: function (deep = FALSE) 
  Parent env: <environment: R_GlobalEnv>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE 
 - attr(*, "name")= chr "Robots_generator"
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
data(Robots)
## maybe str(Robots) ; plot(Robots) ...
}
\keyword{datasets}
