## #old code for wrapping

## sectionview <- function(model, ...) {
##   if (class(model)[1]=="km") {
##     sectionview.km(model=model,...)
##   } else if (class(model)[1]=="list") {
##     sectionview.dm(model=model,...)
##   }
## }


## sectionview3d <- function(model, ...) {
##   if (class(model)[1]=="km") {
##     sectionview3d.km(model=model,...)
##   } else if (class(model)[1]=="list") {
##     sectionview3d.dm(model=model,...)
##   }
## }


## Wrapper for sectionview
if(!isGeneric("sectionview")) {
    setGeneric(name    = "sectionview",
            def = function(model, ...) standardGeneric("sectionview")
    )
}

setMethod("sectionview", "km", 
        function(model,
                type = "UK",
                center = NULL,
                npoints = 100,
                col_points = "red",
                col_surf = "blue",
                conf_lev = c(0.5,0.8,0.9,0.95,0.99),
                conf_blend = NULL,
                bg_blend = 5,
                mfrow = NULL,
                Xname = NULL,
                yname = NULL,
                Xscale = 1,
                yscale = 1,
                ylim = NULL,
                title = NULL,
                ...){
            
            sectionview.km(model = model,
                    type = type,
                    center = center,
                    npoints = npoints,
                    col_points = col_points,
                    col_surf = col_surf,
                    conf_lev = conf_lev,
                    conf_blend = conf_blend,
                    bg_blend = bg_blend,
                    mfrow = mfrow,
                    Xname = Xname,
                    yname=yname,
                    Xscale = Xscale,
                    yscale = yscale,
                    ylim = ylim,
                    title = title,
                    ...)		
        }
)

setMethod("sectionview", "list", 
        function(model,
                center = NULL,
                npoints = 100,
                col_points = "red",
                col_surf = "blue",
                bg_blend = 5,
                mfrow = NULL,
                Xname = NULL,
                yname = NULL,
                Xscale = 1,
                yscale = 1,
                ylim = NULL,
                title = NULL,
                ...){
            
            sectionview.list(model = model,
                    center = center,
                    npoints = npoints,
                    col_points = col_points,
                    col_surf = col_surf,
                    bg_blend = bg_blend,
                    mfrow = mfrow,
                    Xname = Xname,
                    yname = yname,
                    Xscale = Xscale,
                    yscale = yscale,
                    ylim = ylim,
                    title = title,
                    ...)		
        }
)

#Wrapper for sectionview3d
if(!isGeneric("sectionview3d")) {
    setGeneric(name = "sectionview3d",
            def = function(model, ...) standardGeneric("sectionview3d")
    )
}

setMethod("sectionview3d", "km", 
        function(model,
                type = "UK",
                center = NULL,
                axis = NULL,
                npoints = 20,
                col_points = "red",
                col_surf = "blue",
                conf_lev = c(0.95),
                conf_blend = NULL,
                bg_blend = 5,
                Xname = NULL,
                yname = NULL,
                Xscale = 1,
                yscale = 1,
                ylim = NULL,
                title = NULL,
                ...){
            
            sectionview3d.km(model = model,
                    type = type,
                    center = center,
                    axis = axis,
                    npoints = npoints,
                    col_points = col_points,
                    col_surf = col_surf,
                    conf_lev = conf_lev,
                    conf_blend = conf_blend,
                    bg_blend = bg_blend,
                    Xname = Xname,
                    yname = yname,
                    Xscale = Xscale,
                    yscale = yscale,
                    ylim = ylim,
                    title = title,
                    ...)		
        }
)

setMethod("sectionview3d", "list", 
        function(model,
                center = NULL,
                axis = NULL,
                npoints = 20,
                col_points = "red",
                col_surf = "blue",
                bg_blend = 5,
                Xname = NULL,
                yname = NULL,
                Xscale = 1,
                yscale = 1,
                ylim = NULL,
                title = NULL,
                ...){
            
            sectionview3d.list(model = model,
                    center = center,
                    axis = axis,
                    npoints = npoints,
                    col_points = col_points,
                    col_surf = col_surf,
                    bg_blend = bg_blend,
                    Xname = Xname,
                    yname = yname,
                    Xscale = Xscale,
                    yscale = yscale,
                    ylim =
                            ylim,
                    title = title,
                    ...)		
        }
)

#Wrapper for contourview
if(!isGeneric("contourview")) {
    setGeneric(name = "contourview",
            def = function(model, ...) standardGeneric("contourview")
    )
}

setMethod("contourview", "km", 
        function(model,
                type = "UK",
                center = NULL,
                npoints = 20,
                col_points = "red",
                col_surf = "blue",
                bg_blend = 1,
                nlevels = 10,
                Xname = NULL,
                yname = NULL,
                Xscale = 1,
                yscale = 1,
                ylim = NULL,
                title = NULL,
                ...){
            
            contourview.km(model = model,
                    type = type,
                    center = center,
                    npoints = npoints,
                    col_points = col_points,
                    col_surf = col_surf,
                    bg_blend = bg_blend,
                    nlevels = nlevels,
                    Xname = Xname,
                    yname = yname,
                    Xscale = Xscale,
                    yscale = yscale,
                    ylim = ylim,
                    title = title,
                    ...)        
        }
)

setMethod("contourview", "list", 
        function(model,
                center = NULL,
                npoints = 20,
                col_points = "red",
                col_surf = "blue",
                bg_blend = 1,
                nlevels = 10,
                Xname = NULL,
                yname = NULL,
                Xscale = 1,
                yscale = 1,
                ylim = NULL,
                title = NULL,
                ...){
            
            contourview.list(model = model,
                    center = center,
                    npoints = npoints,
                    col_points = col_points,
                    col_surf = col_surf,
                    bg_blend = bg_blend,
                    nlevels = nlevels,
                    Xname = Xname,
                    yname = yname,
                    Xscale = Xscale,
                    yscale = yscale,
                    ylim =
                            ylim,
                    title = title,
                    ...)        
        }
)
