sectionview.fun <- function(fun,
        center = NULL,dim = length(center),
        npoints = 100,
        col_surf = "blue",
        mfrow = NULL,
        Xname = NULL, yname = NULL,
        Xscale = 1, yscale = 1,
        xlim = c(0,1), ylim = NULL,
        title = NULL,
        add = FALSE,
        ...) {
    
    D <- dim
    
    if (is.null(center)) {
        if (D != 1) stop("Section center in 'section' required for >1-D fun.")
    }
       
    if (is.null(mfrow)) {
        nc <- round(sqrt(D))
        nl <- ceiling(D/nc)
        mfrow <- c(nc, nl)
    }
    
    if (!isTRUE(add)) {
        close.screen( all.screens = TRUE )
        split.screen(figs = mfrow)
        .split.screen.lim <<- matrix(NaN,ncol=4,nrow=D) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }
    
    ## find limits: 'rx' is matrix with mins in row 1 and maxs in row 2
    if(!is.null(xlim)) rx <- matrix(xlim,nrow=2,ncol=D)
    else stop("x bounds required for fun.")
    rownames(rx) <- c("min", "max")
    drx <- rx["max", ] - rx["min", ]
    
    ## define X & y labels
    if (is.null(yname)) yname <-  "y"
    if (is.null(Xname)) Xname <- paste(sep = "", "X", 1:D)
    
    ## try to find a good formatted value 'fcenter' for 'center'
    fcenter <- tryFormat(x = center, drx = drx)
    
    for (d in 1:D) {
        screen(d)
        
        xdmin <- rx["min", d]
        xdmax <- rx["max", d]
        xlim = c(xdmin,xdmax)
        
        xd <- seq(from = xdmin, to = xdmax, length.out = npoints)
        x <- data.frame(t(matrix(as.numeric(center), nrow = D, ncol = npoints)))
        if (!is.null(center)) if(!is.null(names(center))) names(x) <- names(center)
        x[ , d] <- xd
        
        ## could be simplified in the future
        y <- array(0, npoints)
        
        for (i in 1:npoints) {
            y[i] <- as.numeric(yscale * fun(x[i, ]))
        }
        
        if (is.null(title)){
            if (D>1) {
                title_d <- paste(collapse = ", ", paste(Xname[-d], '=', fcenter[-d]))
            } else {
                title_d <- paste(collapse = "~", yname, Xname[d])}
        } else {
            title_d <- title
        }
        
        if (is.null(ylim)) {
            ylim <- c(min(y),max(y))
        }
        
        if (isTRUE(add)) {
            # re-use global settings for limits of this screen
            xlim <- c(.split.screen.lim[d,1],.split.screen.lim[d,2])
            ylim <- c(.split.screen.lim[d,3],.split.screen.lim[d,4])
            plot(xd, y,
                    xlab = "", ylab = "",
                    xlim = xlim, ylim = ylim, 
                    type = "l",
                    col = col_surf,
                    add = TRUE,
                    ...)
        } else {
            .split.screen.lim[d,] <<- matrix(c(xlim[1],xlim[2],ylim[1],ylim[2]),nrow=1)
            plot(xd, y,
                xlab = Xname[d], ylab = yname,
                xlim = xlim, ylim = ylim, 
                main = title_d,
                type = "l",
                col = col_surf,
                ...)
            if(D>1) abline(v=center[d],col='black',lty=2)
        }
    }
    
}
