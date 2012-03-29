sectionview.list <- function(model,
                             center = NULL, axis = NULL,
                             npoints = 100,
                             col_points = "red",
                             col_surf = "blue",
                             bg_blend = 5,
                             mfrow = NULL,
                             Xname = NULL, yname = NULL,
                             Xscale = 1, yscale = 1,
                             xlim = NULL, ylim = NULL,
                             title = NULL,
                             add = FALSE,
                             ...) {
    
    D <- length(model$data$X)
    
    if (is.null(center)) {
        if (D != 1) stop("Section center in 'section' required for >1-D model.")
    }
    
    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }
    
    if (is.null(mfrow) && (D>1)) {
        nc <- round(sqrt(D))
        nl <- ceiling(D/nc)
        mfrow <- c(nc,nl)
    }
    
    if (!isTRUE(add)) {
        if (D>1) {
            close.screen( all.screens = TRUE )
            split.screen(figs = mfrow)
        }
        .split.screen.lim <<- matrix(NaN,ncol=4,nrow=D) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }
    
    # apply scaling factor
    X_doe <- Xscale * model$data$X
    n <- dim(X_doe)[1]
    y_doe <- yscale * model$data$Y
    
    ## find limits: 'rx' is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(xlim)) rx <- matrix(xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max") 
    drx <- rx["max", ] - rx["min", ]
    
    if (is.null(ylim)) {
        ymin <- min(y_doe)
        ymax <- max(y_doe)
        ylim <- c(ymin, ymax)
    }
    
    # define X & y labels
    if (is.null(yname)) yname <- names(y_doe)
    if (is.null(yname)) yname <-  "y"
    if (is.null(Xname)) Xname <- names(X_doe)
    if (is.null(Xname)) Xname <- paste(sep = "", "X", 1:D)
    
    ## try to find a good formatted value 'fcenter' for 'center'
    fcenter <- tryFormat(x = center, drx = drx)
    
    for (id in 1:dim(axis)[1]) {
        if (D>1) screen(id, new=!add)
        
        d <- axis[id,]        
        
        xdmin <- rx["min", d]
        xdmax <- rx["max", d]
        xlim = c(xdmin,xdmax)
        
        xd <- seq(from = xdmin, to = xdmax, length.out = npoints)
        x <- data.frame(t(matrix(as.numeric(center), nrow = D , ncol = npoints)))
        if (!is.null(center)) if(!is.null(names(center))) names(x) <- names(center)
        x[ , d] <- xd
        y_mean <- array(0,npoints)
        
        for (i in 1:npoints) {
            y <- modelPredict(model, newdata = (x[i,]))
            y_mean[i] <- yscale * y
        }
        
        if (is.null(title)){
            if (D>1) {
                title_d <- paste(collapse = ", ", paste(Xname[-d], '=', fcenter[-d]))
            } else {
                title_d <- paste(collapse = "~", yname, Xname[d])
            }
        } else {
            title_d <- title
        }
        
        if (isTRUE(add)) {
            # re-use global settings for limits of this screen
            xlim <- c(.split.screen.lim[d,1],.split.screen.lim[d,2])
            ylim <- c(.split.screen.lim[d,3],.split.screen.lim[d,4])
            if (D>1) {
                plot(xd, y_mean,
                     xlim=xlim, ylim=ylim,
                     type = "l",
                     col = col_surf,
                     ...)
            } else { # not using screen(), so need for a non reset plotting method
                lines(xd, y_mean,
                      xlim=xlim, ylim=ylim,
                      col = col_surf,
                      ...)
            }
        } else {
            .split.screen.lim[d,] <<- matrix(c(xlim[1],xlim[2],ylim[1],ylim[2]),nrow=1)
            plot(xd, y_mean,
                 xlab = Xname[d], ylab = yname,
                 ylim = ylim, main = title_d,
                 type = "l",
                 col = col_surf,
                 ...)
            if(D>1) abline(v=center[d],col='black',lty=2)
        }
        
        n <- dim(X_doe)[1]
        
        if (D>1) {
            
            xrel <- scale(x = as.matrix(X_doe),
                          center = center,
                          scale = rx["max", ] - rx["min", ])
            
            alpha <- apply(X = xrel[ , -d, drop = FALSE],
                           MARGIN = 1,
                           FUN = function(x) (1 - (sqrt(sum(x^2)/D)))^bg_blend)
            
            ##   for (i in 1:n) {
            ##         xrel = data.frame(((X_doe[i,] - center) / (xmax - xmin)))
            ##         xrel[d] <- NULL
            ##         alpha[i] = (1 - sqrt(sum(xrel^2)/(D))) ^ bg_blend
            ##       }
        } else {
            alpha <- rep(1, n)
        }
        
        ## modif YD : col
        col1 <- fade(color = col_points, alpha = alpha)
        points(X_doe[ , d], y_doe,
               col = col1,
               ## col = rgb(1, 1-alpha, 1-alpha, alpha),
               pch = 20)
        
    }
    
}
