#' @title Highest density interval HDI from a sample 
#' @description Computes highest density interval from a sample of representative values,
#'   estimated as shortest credible interval.
#' @param sampleVec a vector of representative values from a probability distribution.
#' @param  credMass a scalar between 0 and 1, indicating the mass within the credible
#'     interval that is to be estimated.
#' @return a vector containing the limits of the HDI
#' @export HDIofMCMC
#' @examples
#' set.seed(1)
#' HDIofMCMC(rnorm(100))
#' @author John K. Kruschke

HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  sortedPts = sort( sampleVec )
    ciIdxInc = floor( credMass * length( sortedPts ) )
    nCIs = length( sortedPts ) - ciIdxInc
    ciWidth = rep( 0 , nCIs )
    for ( i in 1:nCIs ) {
        ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
    }
    HDImin = sortedPts[ which.min( ciWidth ) ]
    HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
    HDIlim = c( HDImin , HDImax )
    return( HDIlim )
}

