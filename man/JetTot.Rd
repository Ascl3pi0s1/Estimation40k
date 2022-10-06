\name{JetTot}
\alias{JetTot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Jet Totale simulant une attaque à 40k
%%  ~~function to do ... ~~
}
\description{Jet Totale simulant une attaque à 40k
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
JetTot(Nb_attack, CT, rrTouch = 0, mod_Touch = 0, Bless_auto = 7, For, End, rrBless = 0, mod_Bless = 0, Transu = 0, dmg, Pv, svg, fnp = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nb_attack}{
%%     ~~Describe \code{Nb_attack} here~~
}
  \item{CT}{
%%     ~~Describe \code{CT} here~~
}
  \item{rrTouch}{
%%     ~~Describe \code{rrTouch} here~~
}
  \item{mod_Touch}{
%%     ~~Describe \code{mod_Touch} here~~
}
  \item{Bless_auto}{
%%     ~~Describe \code{Bless_auto} here~~
}
  \item{For}{
%%     ~~Describe \code{For} here~~
}
  \item{End}{
%%     ~~Describe \code{End} here~~
}
  \item{rrBless}{
%%     ~~Describe \code{rrBless} here~~
}
  \item{mod_Bless}{
%%     ~~Describe \code{mod_Bless} here~~
}
  \item{Transu}{
%%     ~~Describe \code{Transu} here~~
}
  \item{dmg}{
%%     ~~Describe \code{dmg} here~~
}
  \item{Pv}{
%%     ~~Describe \code{Pv} here~~
}
  \item{svg}{
%%     ~~Describe \code{svg} here~~
}
  \item{fnp}{
%%     ~~Describe \code{fnp} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Nb_attack, CT, rrTouch = 0, mod_Touch = 0, Bless_auto = 7, 
    For, End, rrBless = 0, mod_Bless = 0, Transu = 0, dmg, Pv, 
    svg, fnp = 7) 
{
    FNP(fnp, miss_save(jet_de_bless(JetTouch = jet_de_touche(Nb_attack = Nb_attack, 
        CT = CT, rrTouch = rrTouch, mod_Touch = mod_Touch, Bless_auto = Bless_auto), 
        For = For, End = End, rrBless = rrBless, mod_Bless = mod_Bless, 
        Transu = Transu), svg), dmg, Pv, save)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
