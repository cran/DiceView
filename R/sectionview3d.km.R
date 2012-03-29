sectionview3d.km <- function(model, type = "UK",
        center = NULL, axis = NULL,
        npoints = 20,
        col_points = "red",
        col_surf = "blue",
        col_needles = NA,
        conf_lev = c(0.95),
        conf_blend = NULL,
        bg_blend = 5,
        Xname = NULL, yname = NULL,
        Xscale = 1, yscale = 1,
        xlim = NULL, ylim = NULL, 
        title = NULL,
        add = FALSE,
        ...) {
    
    require(rgl)
    
    D <- model@d
    
    if (D == 1) stop("for a model with dim 1, use 'sectionview'")
    
    if (is.null(center)) {
        if (D != 2) stop("Section center in 'section' required for >2-D model.")
    }
    
    if (is.null(axis)) {
        axis <- t(combn(D, 2))
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 2)
    }
    
    ## Changed by YD: a vector
    ## if (is.null(dim(npoints))) { npoints <- rep(npoints,D) }
    npoints <- rep(npoints, length.out = D)
    
    if (is.null(conf_blend) ||
            length(conf_blend) != length(conf_lev)) {
        
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))
        
    }
    
    ##  apply scaling factor
    X_doe <- Xscale * model@X
    n <- dim(X_doe)[1]
    y_doe <- yscale * model@y
    
    if (model@noise.flag) {
        sdy_doe <- abs(yscale) * sqrt(model@noise.var)
    } else if (model@covariance@nugget.flag) {
        sdy_doe <- rep(abs(yscale) * sqrt(model@covariance@nugget), n)
    } else {
        sdy_doe <- rep(0, n)
    }
    
    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(xlim)) rx <- matrix(xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- rx["max", ] - rx["min", ]
    
    if (is.null(ylim)) {
        zlim <- range(y_doe)
    } else zlim <- ylim
    
    
    ## define X & y labels
    if (is.null(yname)) yname <- names(y_doe)
    if (is.null(yname)) yname <- "y"
    if (is.null(Xname)) Xname <- names(X_doe)
    if (is.null(Xname)) Xname <- paste(sep = "", "X", 1:D)
    
    
    ## Moved outside the loop by YD
    if (is.null(center)) { 
        center <- rep(0, D)
        names(center) <- paste("X", 1:D, sep = "")
    }
    
    ## try to find a good formatted value 'fcenter' for 'center'
    fcenter <- tryFormat(x = center, drx = drx)
    
    ## Each 'id' will produce a RGL plot
    for (id in 1:dim(axis)[1]) {
        
        d <- axis[id,]
        
        npoints_all <- npoints[d[1]]*npoints[d[2]]
        
        ## ind.nonfix flags the non fixed dims
        ind.nonfix <- (1:D) %in% c(d[1], d[2])
        ind.nonfix <- !ind.nonfix
        
        xlim <- rx[ , d[1]]
        ylim <- rx[ , d[2]]
        
        xdmin <- rx["min", d]
        xdmax <- rx["max", d]
        
        xd1 <- seq(from = xdmin[1], to = xdmax[1], length.out = npoints[1])
        xd2 <- seq(from = xdmin[2], to = xdmax[2], length.out = npoints[2])
        
        x <- data.frame(t(matrix(as.numeric(center), D, npoints_all)))
        if (!is.null(center)) if(!is.null(names(center))) names(x) <- names(center)
        x[ , d] <- expand.grid(xd1, xd2)
        y_mean <- array(0, npoints_all)
        y_sd <- array(0, npoints_all)
        yd_mean <- matrix(0,npoints[1], npoints[2])
        yd_sd <- matrix(0,npoints[1], npoints[2])
        
        ## compute predictions for km.
        ## Note that 'sd' is actually a 'se' (standard error) 
        
        for (i1 in 1:npoints[1]) {
            for (i2 in 1:npoints[2]) {
                i <- i1 + (i2-1) * npoints[1]
                y <- predict(model, type = type, newdata = (x[i,]), checkNames=FALSE)
                y_mean[i] <- yscale * y$mean
                y_sd[i] <- abs(yscale) * y$sd
                yd_mean[i1, i2] <- yscale * y$mean
                yd_sd[i1, i2] <- abs(yscale) * y$sd
            }
        }
        
        ## Note that 'ind.nonfix is used here and later
        if (is.null(title)){
            if (D>2) {
                title_d <-  paste(collapse = ", ", paste(Xname[ind.nonfix],'=', fcenter[ind.nonfix]))
            } else {
                title_d <- paste(collapse = "~", yname, paste(collapse = ",", Xname[d[1]], Xname[d[2]]))
            }
        }else {
            title_d <-  title
        }
        
        if (!isTRUE(add)) {
            open3d()
            
            plot3d(x = x[ , 1], y = x[ , 2], z = y_mean,
            xlab = Xname[d[1]], ylab = Xname[d[2]], zlab = yname,
            xlim = xlim, ylim = ylim, zlim = zlim, type = "n",
            main = title_d,
            col = col_surf,
            ...)
        }
        
        surface3d(x = xd1,y = xd2, z = yd_mean,
                col = col_surf, alpha = 0.5,
                box = FALSE)
        
        ## add  "confidence surfaces"
        for (p in 1:length(conf_lev)) {
            
            colp <- translude(col_surf, alpha = conf_blend[p])
            
            surface3d(x = xd1,
                    y = xd2,
                    z = qnorm((1+conf_lev[p])/2, y_mean, y_sd),
                    col = colp,
                    alpha = conf_blend[p],
                    box = FALSE)
            
            surface3d(x = xd1,
                    y = xd2,
                    z = qnorm((1-conf_lev[p])/2, y_mean, y_sd),
                    col = colp,
                    alpha = conf_blend[p],
                    box = FALSE)
            
        }
        
        ## fade colors according to alpha
        if (D>2) {
            
            xrel <- scale(x = as.matrix(X_doe),
                    center = center,
                    scale = drx)
            
            alpha <- apply(X = xrel[ , ind.nonfix, drop = FALSE],
                    MARGIN = 1,
                    FUN = function(x) (1 - sqrt(sum(x^2)/D))^bg_blend) 
            
            ##    for (i in 1:n) {
            ##         xrel <- data.frame(((X_doe[i, ] - center) / (rx["max", ] - rx["min", ])))
            ##         xrel[d[1]] <- NULL
            ##         xrel[d[2]] <- NULL
            ##         alpha[i] <-  (1 - sqrt(sum(xrel^2)/(D))) ^ bg_blend
            ##       }
            
        } else {
            alpha <- rep(1, n)
        }
        
        if (!model@noise.flag) {
            
            ## [YD] add needles, if wanted
            if (!is.na(col_needles)) {
                
                col0 <- fade(color = col_needles, alpha = alpha)
                plot3d(x = X_doe[ , d[1]], y = X_doe[ , d[2]], z = y_doe,
                        type = "h",
                        col = col0,              
                        alpha = alpha,
                        add = TRUE,
                        box = FALSE)
                
            }
            
            col1 <- fade(color = col_points, alpha = alpha)
            
            points3d(x = X_doe[ , d[1]], y = X_doe[ , d[2]], z = y_doe,
                    col = col1,
                    alpha = alpha,
                    pch = 20, box = FALSE)
            
        }
        
        for (p in 1:length(conf_lev)) {
            
            for (i in 1:n) {
                
                lines3d(x = c(X_doe[i, d[1]], X_doe[i, d[1]]),
                        y = c(X_doe[i, d[2]], X_doe[i, d[2]]),
                        z = c(qnorm((1+conf_lev[p])/2, y_doe[i], sdy_doe[i]),
                                qnorm((1-conf_lev[p])/2, y_doe[i], sdy_doe[i])),
                        col = rgb(red = 1, green = 1-alpha[i], blue = 1-alpha[i],
                                alpha = alpha[i]*conf_blend[p]),
                        alpha = alpha[i]*conf_blend[p],
                        lwd = 5, lend = 1, box = FALSE)
            }
            
        }
    }
}
