################ Function to load excel file and change colnames. Optionally slice ###################
load_excel <- function(file,slice = FALSE,...){
  require(readxl)
  inner <- read_excel(file,...)
  names(inner) <- trimws(names(inner))
  colnames(inner) <- make.names(names(inner),unique = T)
  if(!missing(slice)){
    inner <- slice(inner,slice)
  }
  return(inner) 
}

give.n <- function(x){
  return(c(y = 0, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

theme_set(theme_bw()+
            theme(panel.border = element_rect(colour="grey40"),
                  #axis.title = element_blank(),
                  legend.position = "bottom",
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line(colour = "grey90", size = 0.3)
                 )
          )

sample.size = function(population, rate.defects, c.lev = 0.95, precision = .05) {
  z.val = qnorm(.5+c.lev/2)
  ss = (z.val^2 * rate.defects * (1-rate.defects))/precision^2
  p.ss = round((ss/(1 + ((ss-1)/population))), digits=0)
  METHOD = paste("Recommended sample size for a population of ",
                 population, " at a ", c.lev*100,
                 "% confidence level", sep = "")
  structure(list(Population = population,
                 "Confidence level" = c.lev,
                 "Precision" = precision,
                 "Rate of Defects" = rate.defects,
                 "Recommended sample size" = p.ss,
                 method = METHOD),
            class = "power.htest")
}