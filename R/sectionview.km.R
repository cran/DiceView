sectionview.km <- function(model,
                           type = "UK",
                           center = NULL,
                           npoints = 100,
                           col_points = "red",
                           col_surf = "blue",
                           conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                           conf_blend = NULL,
                           bg_blend = 5,
                           mfrow = NULL,
                           Xname = NULL,
                           yname = NULL,
                           Xscale = 1,
                           yscale = 1,
                           ylim = NULL,
                           title = NULL,
                           ...) {

  D <- model@d
  
  if (is.null(center)) {
    if (D != 1) stop("Section center in 'section' required for >1-D model.")
  }
  
  if (is.null(conf_blend) ||
      length(conf_blend) != length(conf_lev))
    conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))
  
  if (is.null(mfrow)) {
    nc <- round(sqrt(D))
    nl <- ceiling(D/nc)
    mfrow <- c(nc, nl)
  }
  if (mfrow[1] != 1 || mfrow[2] != 1)
    par(mfrow = mfrow)
  
  ## apply scaling factor
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
  
  ## find limits: 'rx' is matrix with mins in row 1 and maxs in row 2
  rx <- apply(X_doe, 2, range)
  rownames(rx) <- c("min", "max")
  drx <- rx["max", ] - rx["min", ]
  
  if (is.null(ylim)) {
    ymin <- min(y_doe-3*sdy_doe)
    ymax <- max(y_doe+3*sdy_doe)
    ylim <- c(ymin, ymax)
  }
  
  ## define X & y labels
  if (is.null(yname)) yname <- names(y_doe)
  if (is.null(yname)) yname <-  "y"
  if (is.null(Xname)) Xname <- names(X_doe)
  if (is.null(Xname)) Xname <- paste(sep = "", "X", 1:D)

  ## try to find a good formatted value 'fcenter' for 'center'
  fcenter <- tryFormat(x = center, drx = drx)

  for (d in 1:D) {

    xdmin <- rx["min", d]
    xdmax <- rx["max", d]
 
    xd <- seq(from = xdmin, to = xdmax, length.out = npoints)
    x <- data.frame(t(matrix(as.numeric(center), nrow = D, ncol = npoints)))
    names(x) <- names(center)
    x[ , d] <- xd

    ## could be simplified in the future
    y_mean <- array(0, npoints)
    y_sd <- array(0, npoints)
    
    for (i in 1:npoints) {
      y <- predict.km(model, type = type, newdata = (x[i, ]))
      y_mean[i] <- yscale * y$mean
      y_sd[i] <- abs(yscale) * y$sd
    }

    if (is.null(title)){
      if (D>1) {
        title_d <- paste(collapse = ", ", paste(Xname[-d], '=', fcenter[-d]))
      } else {
        title_d <- paste(collapse = "~", yname, Xname[d])}
    } else {
      title_d <- title
    }
    
    plot(xd, y_mean,
         xlab = Xname[d], ylab = yname,
         ylim = ylim, main = title_d,
         type = "l",
         col = col_surf,
         ...)
    
    ## 'confidence band' filled with the suitable color 
    for (p in 1:length(conf_lev)) {

      colp <- translude(col_surf, alpha = conf_blend[p])
      
      polygon(c(xd,rev(xd)),
              c(qnorm((1+conf_lev[p])/2, y_mean, y_sd),
                rev(qnorm((1-conf_lev[p])/2, y_mean, y_sd))),
              col = colp,
              border = NA)
      
    }

    ## fading colors for points
    if (D>1) {
      
      xrel <- scale(x = as.matrix(X_doe),
                    center = center,
                    scale = rx["max", ] - rx["min", ])
      
      alpha <- apply(X = xrel[ , -d, drop = FALSE],
                     MARGIN = 1,
                     FUN = function(x) (1 - (sqrt(sum(x^2)/D)))^bg_blend)
      
    } else {
      alpha <- rep(1, n)
    }
    
    
    if (!model@noise.flag) {
      
      col1 <- fade(color = col_points, alpha = alpha)
      ## cat("faded colors\n"); print(col1)
      
      points(X_doe[,d], y_doe,
             col = col1,
             ## col = rgb(1, 1-alpha, 1-alpha, alpha),
             pch = 20)
    }
      
    for (p in 1:length(conf_lev)) {
      
      for (i in 1:n) {
        lines(c(X_doe[i,d],X_doe[i,d]),
              c(qnorm((1+conf_lev[p])/2, y_doe[i], sdy_doe[i]),
                qnorm((1-conf_lev[p])/2, y_doe[i], sdy_doe[i])),
              col = rgb(1,1-alpha[i], 1-alpha[i], alpha[i]*conf_blend[p]),
              lwd = 5, lend = 1)
      }
      
    }
    
  }
  
}
