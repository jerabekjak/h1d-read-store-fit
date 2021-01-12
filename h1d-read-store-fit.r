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
      final = c()
      file_ = paste(project_, 'Fit.out', sep = '/')
      fit = readLines(file_)
      start.fit = which(grepl('Variable', fit))
      end.fit = which(grepl('Contributions', fit))
      nlines = end.fit - start.fit - 3
      pars = read.table(file_, skip = start.fit, nrows = nlines)
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
    yr = range(pars$V4, pars$V5)
    bp = barplot(pars$V2, main = pars$V1[1], ylim = yr, ...,
                 names.arg = paste('material', 1:length(pars$V1)))
    for (i in 1:n)
    {
      lines(rep(bp[i,],2), c(pars$V4[i], pars$V5[i]))
    }
  }
)

