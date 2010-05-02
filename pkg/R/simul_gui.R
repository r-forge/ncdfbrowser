library(gWidgets)
library(RGtk2)
library(ncdf)
library(chromatoplots)
source("~/Prolang/svn/repos/trunk/chromatoplots/R/simul.R")
#vignette('gWidgets',package='gWidgets')
options("guiToolkit"="RGtk2")


## Main window
win <- gwindow('NetCDF browser and simulator',visible=T)
group <- ggroup(horizontal=FALSE,cont=win)


defHandler <- function(h,...) print('hi')
## handler for quit
quitHandler <- function(h,...) dispose(win)
## plot var
plotVarHandler <- function(h,...) {
  if(exists("gsum")) delete(g33,gsum)
  v <- svalue(g4drop)
  p <- svalue(g4drop2)
  n <- get.var.ncdf(temp,v)
  if(length(dim(n))==1){
    sum <- data.matrix(summary(n))
    df <- data.frame(Statistics=rownames(sum),Values=as.numeric(sum[,1]))
    gsum <<- gtable(df,cont=g33,expand=T)
}else{
   sum <- data.matrix(summary(n))
   df <- as.data.frame(cbind(sum))
   gsum<<-gtable(df,cont=g33,expand=T)
  
}
   if(p=='boxplot') boxplot(n)
   if(p=='histogram') hist(n)
   if(p=='contour') {
     test <- melt(n)
     names(test) <- c('x','y','z')
     gp <- ggplot(test,aes(x,y,z=z))+stat_contour()
     print(gp)
   }
  if(p=='contour2') {
 #   browser()
     contour(z=n)
   }
  if(p=='heatmap'){
    heatmap(n)
  }
}
## handler for open

#model <- NULL
#browser()
openHandler <- function(text="Select a file",action="print",type='open',...) {
  gfile(text=text,type=type,...,action=action,handler=function(h,...){
    if(length(h$file)>1) {
      gmessage('Please choose single file')
    }else{
      obj <<- h$file
  }
  
  
    temp <<- open.ncdf(obj)
    var <- names(temp$var)
    tempfile <- tempfile()
    sink(tempfile)
    print.ncdf(temp)
    sink()
    x <- read.table(tempfile)

    x <- x[,2,drop=F]
    colnames(x) <- 'Dimentions and variables'
    gt <- gtable(x,cont=nb,label='Overview')
#    add(nb,gt,label='Overview')
#    lst <- str(temp)
    lst <- temp
     getVal = function(lst,path) {
     if(length(path) == 1)
     return(lst[[path]])
     else
     getVal(lst[[path[1]]],path[-1])
   }
    
    offspring = function(path, data) {
     if(length(path) == 0) {
       alst = lst
     } else {
       alst = getVal(lst, path)
     }
     df = data.frame(items=names(alst),
                    hasOffspring = sapply(names(alst), function(i)
                    is.list(alst[[i]])),
                    stringsAsFactors=FALSE)
     return(df)
 }

    t = gtree(offspring=offspring, cont=nb,label='Structure')
    ## Add another tab for variable summary
    ## Generate a widgeit first then integrate it into the tab
    g3 <- ggroup(horizontal=T,cont=nb,label='Plots')
    g31 <<- ggroup(horizontal=F,cont=g3)
    g4 <- gframe('Variables',cont=g31)
 
    g4drop <<- gdroplist(c('Select a variable',var),cont=g31,handler=function(h,...){
   #   browser()
      if(exists("g4drop2")) delete(g31g,g4drop2)
      if(exists("gsum")) delete(g33,gsum)
      n <- get.var.ncdf(temp,svalue(g4drop))
      ## if(!is.vector(n)) {
      ##   plots <- c('contour','contour2','heatmap')
      ##   g4drop2 <<-gradio(plots,cont=g31g)
      ##   }else{
      ##   plots <- c('boxplot','histogram')
      ##   g4drop2 <<-gradio(plots,cont=g31g)
      ##   }
      #browser()
        if(length(dim(n))>1) {
        plots <- c('contour','contour2','heatmap')
        g4drop2 <<-gradio(plots,cont=g31g)
        }else{
        plots <- c('boxplot','histogram')
        g4drop2 <<-gradio(plots,cont=g31g)
        }
      
    })
    plots <- c('boxplot','historgram')
    g31g <<- ggroup(horizontal=F,cont=g31)
    #g4drop2 <- gradio(plots,cont=g31g)

    g4b1 <- gbutton('go',cont=g31,handler=plotVarHandler)
    g32 <<- gframe('Plots',cont=g3)
    ggraphics(cont=g32)
    g33 <<- gframe('Numerical summary',cont=g3,expand=T)
    
    #browser()
    
    
  })
}

modelHandler <- function(text='choose a model',action='print',type='open',...){
  gf <- gfile(type=type,text=text,...,action=action,handler=function(h,..){
    #assign(model,h$file,envir=.GlobalEnv)
    model<<-h$file
     para[1,4] <- gedit(h$file)
  })
}

simulHandler <- function(h,...){
  ## get values for parameters of simulater

dir <- svalue(dir)
#model <- svalue(model)
int_range = c(as.numeric(svalue(int_min)),as.numeric(svalue(int_max)))
back_sd=as.numeric(svalue(back_sd))
rep_sd=as.numeric(svalue(rep_sd))
rep=as.numeric(svalue(rep))
common=as.numeric(svalue(common))
diff_low=as.numeric(svalue(diff_low))
diff_zero=as.numeric(svalue(diff_zero))
low=as.numeric(svalue(low))
low_sd=as.numeric(svalue(low_sd))
mz_range = c(as.numeric(svalue(mz_min)),as.numeric(svalue(mz_max)))
npeaks_mean=as.numeric(svalue(npeaks_mean))
npeaks_sd=as.numeric(svalue(npeaks_sd))
rt_range = c(as.numeric(svalue(rt_min)),as.numeric(svalue(rt_max)))
rt_diff =  as.numeric(svalue(rt_diff))
rt_shift_sd= as.numeric(svalue(rt_shift_sd))
span=as.numeric(svalue(span_mean))
span_sd=as.numeric(svalue(span_sd))
sigma=as.numeric(svalue(sigma_mean))
sigma_sd=as.numeric(svalue(sigma_sd))
tau_mean = as.numeric(svalue(tau_mean))
tau_sd = as.numeric(svalue(tau_sd))
missing= svalue(missing)

if(missing!='NULL') {missing=as.numeric(missing) } else{missing=NULL}

   simulator(dir=dir,
             model=model,
             int_range = int_range,
             back_sd=back_sd,
             rep_sd=rep_sd,
             rep=rep,
             common=common,
             diff_low=diff_low,
             diff_zero=diff_zero,
             low=low,
             low_sd=low_sd,
             mz_range = mz_range,
             npeaks_mean=npeaks_mean,
             npeaks_sd=npeaks_sd,
             rt_range = rt_range,
             rt_diff =  rt_diff,
             rt_shift_sd= rt_shift_sd,
             span=span,
             span_sd=span_sd,
             sigma=sigma,
             sigma_sd=sigma_sd,
             tau_mean = tau_mean,
             tau_sd = tau_sd,
             missing=missing) }                  


setdefHandler <- function(h,...){
svalue(dir) <- ""
para[1,4] <- gbutton('browser',handler=modelHandler)
svalue(missing) <- "NULL"
svalue(common) <- "5"
svalue(diff_zero) <- "5"
svalue(diff_low) <- "5"
svalue(low) <- "0.3"
svalue(low_sd) <- "0"
svalue(rep) <- "2"
svalue(rep_sd) <- "0.1"
svalue(int_min)<-"300"
svalue(int_max) <- "300000"
svalue(mz_min)<- "50"
svalue(mz_max)<-"800"
svalue(npeaks_mean)<- "100"
svalue(npeaks_sd) <- "0.1"
svalue(rt_min) <- "215"
svalue(rt_max) <- "3600"
svalue(rt_diff) <- "0.5"
svalue(rt_shift_sd) <- "5"
svalue(back_sd) <- "0.1"
svalue(tau_mean)<- "0"
svalue(tau_sd) <- "0.1"
svalue(span_mean)<- "15"
svalue(span_sd)<- "0.1"
svalue(sigma_mean) <- "1.5"
svalue(sigma_sd) <- "0.1"
}

quitNbHandler <- function(h,...){
  dispose(nb)
}


## menubar and toolbar
mbl <- list(
            File=list(
              openFile=list(handler=openHandler,icon='open'),
              quit=list(handler=quitHandler,icon='cancel')
              ),
            Edit=list(
              paste=list(handler=defHandler),
              copy=list(handler=defHandler)
              )
            )

tbl <- list(
            open=list(handler=openHandler,icon='open'),
            save=list(handler=defHandler,icon='save'),
            quit=list(handler=quitHandler,icon='quit'),
            quitNb=list(handler=quitNbHandler,icon='cancel')
            )

mb <- gmenu(mbl,cont=group)
tb <- gtoolbar(tbl,cont=group)
g1 <- ggroup(cont=group,expand=T,horizontal=FALSE)
nb <- gnotebook(cont=g1,expand=T)


## parameter setting tab
g3 <- ggroup(horizontal=F,cont=nb,label='Parameter Settings')
para <- glayout(cont=g3)
g2 <- ggroup(cont=g3,horizontal=TRUE)
addSpring(g2)
button1 <- gbutton("Generate",cont=g2,handler=simulHandler)
button2 <- gbutton("Set to Default",cont=g2,handler=setdefHandler)

## layout
para[1,1] <- glabel('Dir')
para[1,2] <- (dir <- gedit("/home/tengfei/Desktop"))
para[1,3] <- glabel('Model')
para[1,4] <- gbutton('browser',handler=modelHandler)
para[1,5] <- glabel('Misssing')
para[1,6] <- (missing <- gedit("NULL"))
para[1,7] <- glabel('Common')
para[1,8] <- (common <- gedit("5"))

para[2,1] <- glabel('Diff_zero')
para[2,2] <- (diff_zero<- gedit("5"))
para[2,3] <- glabel('Diff_low')
para[2,4] <- (diff_low<- gedit("5"))
para[2,5] <- glabel('Low')
para[2,6] <- (low <- gedit("0.3"))
para[2,7] <- glabel('Low sd')
para[2,8] <- (low_sd <- gedit("0"))

para[3,1] <- glabel('Replicate')
para[3,2] <- (rep <- gedit("2"))
para[3,3] <- glabel('Rep sd')
para[3,4] <- (rep_sd <- gedit("0.1"))
para[3,5] <- glabel('Intensity Min')
para[3,6] <- (int_min <- gedit("300"))
para[3,7] <- glabel('Intensity Max')
para[3,8] <- (int_max <- gedit("300000"))

para[4,1] <- glabel('Mz min')
para[4,2] <- (mz_min<- gedit("50"))
para[4,3] <- glabel('Mz max')
para[4,4] <- (mz_max<- gedit("800"))
para[4,5] <- glabel('Peaks number')
para[4,6] <- (npeaks_mean<- gedit("100"))
para[4,7] <- glabel('Peaks number sd')
para[4,8] <- (npeaks_sd <- gedit("0.1"))

para[5,1] <- glabel('RT min(second)')
para[5,2] <- (rt_min<- gedit("215"))
para[5,3] <- glabel('RT max(second)')
para[5,4] <- (rt_max<- gedit("3600"))
para[5,5] <- glabel('RT diff between scan')
para[5,6] <- (rt_diff<- gedit("0.5"))
para[5,7] <- glabel('RT drift')
para[5,8] <- (rt_shift_sd<- gedit("5"))

para[6,1] <- glabel('Background noise sd')
para[6,2] <- (back_sd <- gedit("0.1"))
para[6,3] <- glabel('Tau mean')
para[6,4] <- (tau_mean<- gedit("0"))
para[6,5] <- glabel('Tau sd')
para[6,6] <- (tau_sd <- gedit("0.1"))
para[6,7] <- glabel('Span mean')
para[6,8] <- (span_mean<- gedit("15"))
para[7,1] <- glabel('Span sd')
para[7,2] <- (span_sd<- gedit("0.1"))
para[7,3] <- glabel('Sigma mean')
para[7,4] <- (sigma_mean<- gedit("1.5"))
para[7,5] <- glabel('Sigma sd')
para[7,6] <- (sigma_sd<- gedit("0.1"))



## expand group
 ## rightArrow <- system.file("images/1rightarrow.gif",package="gWidgets")
 ## downArrow <- system.file("images/1downarrow.gif",package="gWidgets")
 ## g <- ggroup(horizontal=FALSE,cont=T)
 ## g1 <- ggroup(horizontal=TRUE, cont=g)
 ## icon <- gimage(downArrow,cont=g1)
 ## label <- glabel("Expand group example", cont=g1)
 ## g2 <- ggroup(cont=g, expand=TRUE)
 ## expandGroup <- function() add(g,g2, expand=TRUE)
 ## hideGroup <- function() delete(g,g2)
 ## state <- TRUE # a global
 ## changeState <- function(h,...) {
 ## if(state) {
 ## hideGroup()
 ## svalue(icon) <- rightArrow
 ## } else {
 ## expandGroup()
 ## svalue(icon) <- downArrow
 ## }
 ## state <<- !state
 ## }
 ## addHandlerClicked(icon, handler=changeState)
 ## addHandlerClicked(label, handler=changeState)
 ## gbutton("Hide by clicking arrow", cont=g2)
