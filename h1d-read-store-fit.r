# Reads and stores results of the inversion process in H1D
# usage: invres = InvResults([project directory])

FitTablo <- setRefClass("FitTablo", fields=list(correlation = "character",
                                                finalresults = 'character'))
InvResults <- setRefClass("InvResults",  fields = list(projectname = "character",
                                                       init_pars = 'list',
                                                       final_pars = 'data.frame',
                                                       obs_mod = "data.frame",
                                                       bc = "data.frame",
                                                       fit_our = "FitTablo")
)


InvResults$methods(
  initialize = function(projectname)
  {
    .self$projectname = projectname
    .self$printname()
    .self$read_obs()
    .self$read_pars()
    .self$read_bc()
  }
)


InvResults$methods(
  printname = function()
  {
    print (.self$projectname)
  }
)


InvResults$methods(
  read_obs = function()
  {
    project_ = .self$projectname
    ioerr = system(paste('python show-fit.py', project_))
    if (ioerr != 0) {stop('error v py skriptu')}
    
    d = read.table('.modobs.txt', skip = 2, header = T)
    colnames(d)[5] = 'Residual'
    
    fin = readLines('.finalresults.txt')
    cor_ = readLines('.correlation.txt')
    
    .self$obs_mod = d
    .self$fit_our$finalresults = fin
    .self$fit_our$correlation = cor_
  }
)


InvResults$methods(
  read_pars = function()
  {

    init_pars = function(project_)
    {
      init = list()
      file_ = paste(project_, 'FIT.IN', sep = '/')
      fit = readLines(file_)
      start.init = which(grepl('thr', fit))
      cc = 1
      for (i in start.init)
      {
        material = read.table(file_, skip = i-1, nrows = 4, sep = '', header = T)
        init[[paste('material',cc,sep='_')]] = material
        cc = cc + 1
      }
      return(init)
    }

    final_pars = function(project_)
    {
      file_ = paste(project_, 'Fit.out', sep = '/')
      fit = readLines(file_)
      start.fit = which(grepl('Variable', fit))
      end.fit = which(grepl('Contributions', fit))
      nlines = end.fit - start.fit - 3
      pars = read.table(file_, skip = start.fit, nrows = nlines)
      colnames(pars) <- c('Variable', 'Value', 'S.E.Coeff.', 'Lower', 'Upper')
      print (pars)
    }

    project_ = .self$projectname
    .self$init_pars = init_pars(project_)
    .self$final_pars = final_pars(project_)

  }
)


InvResults$methods(
  read_bc = function()
  {
    project_ = .self$projectname
    file_ = 'T_Level.out'
    path = paste(project_, file_, sep='/')

    n = length(readLines(path))
    d = read.table(path, skip = 9, header = F, dec='.', nrows = n-10)

    h1 = c(t(read.table(path, skip = 6, header = F, dec='.', nrows = 1)))
    h2 = c(t(read.table(path, skip = 7, header = F, dec='.', nrows = 1)))
    

    colnames(d) <- paste(h1 ,h2, sep='_')
    print (d)
    .self$bc = d
  }
)



# which_ :: which rows should be used fot the barplot
#           preferable row with the same parameters
InvResults$methods(
  barplots = function(which_, ...)
  {
    pars = .self$final_pars[which_,]
    n = length(which_)
    yr = range(0, pars$Lower, pars$Upper)
    bp = barplot(pars$Value, main = pars$Variable[1], ylim = yr, ...,
                 names.arg = paste('material', 1:length(pars$Variable)))
    for (i in 1:n)
    {
      lines(rep(bp[i,],2), c(pars$Lower[i], pars$Upper[i]))
    }
  }
)



InvResults$methods(
  plot_bc = function()
  {
    bc = .self$bc
    rr = range(bc$`sum(rTop)_[L]`, bc$`sum(rRoot)_[L]`,bc$`sum(vTop)_[L]`, bc$`sum(vBot)_[L]`,25)
    plot(bc$`Time_[T]`, bc$`sum(rTop)_[L]`, cex=0.5, type='l', ylim = rr,
         xlab = '', ylab = ''); grid()
    lines(bc$`Time_[T]`, bc$`sum(vTop)_[L]`, cex=0.5, type='l', lty=2)
    lines(bc$`Time_[T]`, bc$`sum(rRoot)_[L]`, cex=0.5, type='l', col=2)
    lines(bc$`Time_[T]`, bc$`sum(vRoot)_[L]`, cex=0.5, type='l', col=2, lty=2)
    lines(bc$`Time_[T]`, bc$`sum(RunOff)_[L]`, cex=0.5, type='l', col=3, lty=1)
    lines(bc$`Time_[T]`, bc$`sum(vBot)_[L]`, cex=0.5, type='l', col=4, lty=1)
    mtext('DAS', side = 1, line = 2)
    mtext('cm', side = 2, line = 2)
    legend('topleft', cex=0.8, 
           legend = c('pot. top', 'act. top', 'pot. root uptake ', 'act. root update', 'sur. runoff', 'bottom act.'),
           col = c(1,1,2,2,3,4), lty = c(1,2,1,2,1, 1), ncol = 3)
    n = length(bc$`Time_[T]`)
    dwater = -(bc$`sum(vTop)_[L]`[n] + bc$`sum(vRoot)_[L]`[n])
    dwater = bc$`Volume_[L]`[length(bc$`Volume_[L]`)] - bc$`Volume_[L]`[1]
    mtext(paste('water difference at the end of the perios [cm]:', dwater), 
          side = 3, line = 0.5, adj = 0)
  }
)



InvResults$methods(
  plot_fit = function(d)
  {
    om = as.data.frame(.self$obs_mod)
    pos = unique(om$Position)
    yr = range(om$Obs, om$Fitted)
    xr = range(om$Time)
    par(mar = c(3,3,1,1))
    par(cex = 1) #
    plot(NA, ylim = yr, xlim = xr)
    mtext('DAS', 1, 2)
    mtext('SWC', 2, 2)
    
    for (i.pos in pos)
    {
      ii = which(om$Position==i.pos)
      points(om$Time[ii], om$Obs[ii], col = i.pos, cex = 0.5)
      lines(om$Time[ii], om$Fitted[ii], col=i.pos)
    }
    legend('topright', legend = pos, col = pos, lty = 1, cex=0.8)
  }
)


InvResults$methods(
  prt_fitout = function()
  {
    fit = .self$fit_our$finalresults
    start.fit = 1
    end.fit = length(fit)
    plot(NA, xlim=c(0,10), ylim=c(0,end.fit-start.fit+1), bty='n',
         xaxt='n', yaxt='n', xlab='', ylab='')
    text (rep(1,end.fit-start.fit),(end.fit-start.fit):0,fit[start.fit:end.fit], font = 11,
          adj = 0, cex=0.75)
  }
)


InvResults$methods(
  prt_correlation = function()
  {
    fit = .self$fit_our$correlation
    start.fit = 1
    end.fit = length(fit)
    plot(NA, xlim=c(0,10), ylim=c(0,end.fit-start.fit+1), bty='n',
         xaxt='n', yaxt='n', xlab='', ylab='')
    text (rep(1,end.fit-start.fit),(end.fit-start.fit):0,fit[start.fit:end.fit], font = 11,
          adj = 0, cex=0.75)
  }
)


InvResults$methods(
  prt_init_pars = function()
  {
    library(gridExtra)
    library(grid)
    pars = .self$init_pars
    for (par_ in pars)
    {
      plot.new()
      grid.table(par_)
    }  
  }  
)

