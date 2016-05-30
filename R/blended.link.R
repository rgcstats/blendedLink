#' Link object where the link function equals a specified link function for values
#' of the mean up to a specified cutoff, and a smooth linear-rescaling of a
#' different link function when the mean is above the cutoff
#'
#' This function produces a blended object for a specified cutover and link functions
#' for use in binary regression using glm or glm2.
#'
#' @details
#' This function returns a link function which which equals link function link1 for
#' values of the fitted probability below cutover. When the fitted probability is greater
#' than cutover, a linearly scaled user-specified linkis used.
#' The linear scaling is such that the new link is smooth (link and first derivative both continuous).
#' The main use of the function is to use an easily interpretable link in binary regression up to
#' a cutoff (e.g. log), and a more traditional link which asymptotes to 1 above the cutoff (e.g. logit).
#' The advantage is that the exponentiated regression coefficients can be interpreted as relative risks
#' for individuals whose probability is up to cutover both with and without
#' the risk factor. In practice this is usually the great majority of cases.
#' The function is not normally called directly. Instead, glm or glm2 should be
#' used (preferably the latter to avoid fitting problems), as per the example below.
#' @param link1 Character string indicating the link function to be used up to the cutover
#' @param link2 Character string indicating the link function to be used above the cutover
#' @param cutover The link function switches smoothly from link1 to link2
#' @return An object of class "link-glm", with link name "blended".
#' 
#' @import stats
#' @examples
#' example.binary.regression <- glm(y~x1+x2 , data=loglogit.example,
#'     family = binomial(link=blendedLink("log","logit",0.8)))
#' summary(example.binary.regression)
#' @export
blendedLink <- function(link1,link2,cutover){
  # calculate linear scalers a and b first
  L1 <- make.link(link1) ; L2 <- make.link(link2)
  b <- L1$mu.eta(L1$linkfun(cutover)) / L2$mu.eta(L2$linkfun(cutover))
  a <- L2$linkfun(cutover) - b*L1$linkfun(cutover)
  # now create and return link components
  linkfun <- function(mu){
    (mu<=cutover)*L1$linkfun(mu) + (mu>cutover)*(L2$linkfun(mu)-a)/b
  }
  linkinv <- function(eta){
    (L1$linkinv(eta)<=cutover)*L1$linkinv(eta) + (L1$linkinv(eta)>cutover)*L2$linkinv(a+b*eta)
  }
  mu.eta <- function(eta){
    (L1$linkinv(eta)<=cutover)*L1$mu.eta(eta) + (L1$linkinv(eta)>cutover)*L2$mu.eta(a+b*eta)*b
  }
  valideta <- function(eta){
    L1$valideta(pmin(eta,L1$linkfun(cutover))) & L2$valideta(pmax(eta,L1$linkfun(cutover)))
  }
  out <- list(  linkfun=linkfun , linkinv=linkinv , mu.eta=mu.eta , valideta=valideta ,
                name="blended" )
  class(out) <- "link-glm"
  out
}
