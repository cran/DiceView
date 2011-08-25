
##=========================================================
## make colors semi-transparent
##
## alpha = 0 OPAQUE
## alpha = 1 FULLY TRANSPARENT
##
##=========================================================

translude <- function(colors, alpha = 0.6) {
    
    alpha <- rep(alpha, length.out = length(colors))
    rgb <- as.matrix(col2rgb(colors)/255)
    colors2 <- rgb(red = rgb["red", ],
            green = rgb["green", ],
            blue = rgb["blue", ],
            alpha = alpha)
    
    
}

##========================================================
## fade color: one color and several fading values
## between 0 and 1
##
## alpha ~ 0 nearly equal to the input 'color'
## alpha ~ 1 nearly invisible grayed/transparent 
##
## The returned value is vector. It is build
## from a matrix with four rows as
## those produced by col2rgb(x, alpha = TRUE)
## 
##========================================================

fade <- function(color = "red",
        alpha =  seq(from = 0, to = 1, length.out = 5),
        plot = FALSE) {
    
    if (any(alpha < 0) || any(alpha > 1)) stop("'alpha' values must be >=0 and <= 1")
    if (length(color) > 1) stop("'color' must be of length 1")  
    
    ## a matrix with  1 col
    rgbcol <- col2rgb(color)/255
    mat <- matrix(1-alpha, nrow = 3, ncol = length(alpha), byrow = TRUE)
    
    mat <- mat + rgbcol %*% alpha 
    
    colors2 <- rgb(red = mat[1, ],
            green = mat[2, ],
            blue = mat[3, ],
            alpha = alpha)
    
    if (plot) {
        x <- seq(from = 0, to = 1, length.out = length(alpha))
        plot.new( )
        for ( i in 1:(length(alpha)) ){
            rect(xleft = x[i],
                    xright = x[i+1],
                    ybottom = 0,
                    ytop =  1,
                    border = NA,
                    col = colors2[i])
        }
        
    }
    
    colors2
    
}

##========================================================
## try to find a good formatted value for a numeric vector
## x using a vector of diff range drx
##
## For instance, if drx is 1000, no decimal or very few
## decimals should be used.
## 
##========================================================

tryFormat <- function(x, drx) {
    
    d <- length(x)
    ldx <- log(drx, base = 10)
    ff <- rep(1, d)
    fd <- rep(1, d)
    ff[ldx > 0] <- ceiling(ldx[ldx > 0])
    fd[ldx < 0] <- ceiling(-ldx[ldx < 0]) + 2
    ff <- ff + fd +1
    
    formats <- paste("%", ff, ".", fd, "f", sep = "")
    fx <- sprintf(formats, x)
    
}

imagetranslude <- function(x1=NULL,x2=NULL,ZImage,ZTranslude,rangeZImage=NULL,rangeZTranslude=NULL,title=NULL,red=1,green=1,blue=1,TransludeOption=NULL){
    
    if (is.null(x1)) x1 <- seq(1:ncol(ZImage))
    if (is.null(x2)) x2 <- seq(1:nrow(ZImage))
    if (is.null(rangeZTranslude)) rangeZTranslude <- range(ZTranslude)
    if (is.null(TransludeOption$TransludeAscending)) TransludeOption$TransludeAscending <- TRUE
    if (is.null(TransludeOption$TransludeNorm)) TransludeOption$TransludeNorm <- TRUE
    
    if (TransludeOption$TransludeNorm) { #mofidy ZTranslude
        if (rangeZTranslude[1] == rangeZTranslude[2]){
            if (rangeZTranslude[1]!=0) ZTranslude <- ZTranslude/rangeZTranslude[1]
        } else ZTranslude <- (ZTranslude - rangeZTranslude[1]) / (rangeZTranslude[2]- rangeZTranslude[1])	#normalize ZTranslude
    }
    
    if (range(ZTranslude)[1] < 0) stop("ZTranslude values must be between 0 and 1 or need to be normalized")
    if (range(ZTranslude)[2] > 1) stop("ZTranslude values must be between 0 and 1 or need to be normalized")
    if(!TransludeOption$TransludeAscending) ZTranslude <- 1-ZTranslude #a value of x becomes 1-x
    
    Graph2d_ContourImageTranslude (x1=x1,x2=x2,ZImage=ZImage,ZTranslude=ZTranslude,title=title,red=red,green=green,blue=blue,rangeZImage=rangeZImage)
    
}

Graph2d_ContourImageTranslude <- function(x1,x2,ZContour=NULL,ZImage,ZTranslude,title=NULL,contourlevels = NULL,red=1,green=1,blue=1,rangeZImage=NULL){
    
    #ZTranslude is assumed to be between 0 and 1
    
    if (is.null(contourlevels)) contourlevels = c(0.05,0.5,0.95)
    if (is.null(rangeZImage)) rangeZImage = range(ZImage)
    if (is.null(title)) title = "Z"
    
    #resolution <- sqrt(length(ZImage))
    nrow1<-nrow(ZImage)
    ncol1<-ncol(ZImage)
    
    colorsToTranslude <- color.scale(ZImage,redrange=c(0,red),greenrange=c(0,green),bluerange=c(0,blue),xrange=rangeZImage)
    
    colorsTransluded <- matrix(c(0),nrow=nrow1,ncol=ncol1)
    for (i in 1:nrow1){
        for (j in 1: ncol1){
            colorsTransluded[i,j] <- translude(colorsToTranslude[i,j],ZTranslude[i,j])
        }
    } 
    
    color2D.matplot(matrix(c(0),nrow=nrow1,ncol=ncol1),
            cellcolors=rotate.matrix(colorsTransluded),border="NA",xlab="x1",ylab="x2",show.legend=FALSE,axes=FALSE)
    
    if (!is.null(ZContour)) contour(x1*nrow1, x2*ncol1, ZContour, labcex=1,levels = contourlevels,add = TRUE,lwd=4,col="white")
    title(main = title, font.main = 4,cex.main = 1)
}

rotate.matrix <- function(mat){
    #M(i,j) <- M(j,n+1-i)
    nrow1 <- nrow(mat)
    ncol1 <- ncol(mat)
    result <- matrix(c(0),nrow=ncol1,ncol=nrow1)
    for (i in 1:nrow(result)) result[i,] <- mat[,nrow(result)+1-i]
    return(result)
}
