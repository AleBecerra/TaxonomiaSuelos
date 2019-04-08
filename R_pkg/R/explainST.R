
#' Explicar los taxones a nivel de subgrupo del sistema Soil Taxonomy #'
#' @param x un subgrupo, la concordancia es exacta, no distingue mayúsculas de minúsculas
#' 
#' @return devuelve un bloque de texto, adecuado para mostrar con fuentes de ancho fijo
#' 
#' @note esta función acepta solamente el nivel de subgrupo.
#' 
#' @export
explainST <- function(x) {
  
  # la concordancia se comprueba en minúsculas
  x <- tolower(x)
  
  x.o <- OrderFormativeElements(x)
  x.so <- SubOrderFormativeElements(x)
  x.gg <- GreatGroupFormativeElements(x)
  x.sg <- SubGroupFormativeElements(x)
  
  ex <- list()
  # el taxón a explicar, generalmente un subgrupo
  ex[[1]] <- x
  
  ex[[2]] <- .subGroupLines(x.o, x.so, x.gg, x.sg)
  
  ex[[3]] <- .greatGroupLines(x.o, x.so, x.gg)
  
  ex[[4]] <- .subOrderLines(x.o, x.so)
  
  ex[[5]] <- .soilOrderLines(x.o)
  
  res <- paste(unlist(ex, recursive = TRUE), collapse='\n')
  
  return(res)
}

## funciones usadas internamente

.printExplanation <- function(width=100, pos, txt) {
  # separa la explicación en un vector
  txt <- strsplit(txt, split = '')[[1]]
  # ubicación de la explicación
  idx <- seq(from=pos, to=pos + (length(txt) - 1))
  # hace lugar para explicaciones largas
  ws <- rep(' ', times=pmax(width, max(idx)))
  # ingreso de texto
  ws[idx] <- txt
  
  # convierte a caracter
  return(paste(ws, collapse=''))
}

.makeBars <- function(width=100, pos) {
  # espacio en blanco inicial
  ws <- rep(' ', times=width)
  # inserta barras
  ws[pos] <- '|'
  
  # convierte a caracter
  return(paste(ws, collapse=''))
}



.soilOrderLines <- function(o) {
  txt <- list()
  
  txt[[1]] <- .makeBars(pos=o$char.index)
  txt[[2]] <- .printExplanation(pos = o$char.index, txt = o$defs$connotation)
  
  return(txt)
}

.subOrderLines <- function(o, so) {
  txt <- list()
  
  txt[[1]] <- .makeBars(pos=c(so$char.index, o$char.index))
  txt[[2]] <- .printExplanation(pos = so$char.index, txt = so$defs$connotation)
  
  return(txt)
}

.greatGroupLines <- function(o, so, gg) {
  txt <- list()
  
  txt[[1]] <- .makeBars(pos=c(gg$char.index, so$char.index, o$char.index))
  txt[[2]] <- .printExplanation(pos = gg$char.index, txt = gg$defs$connotation)
  
  return(txt)
}

# 
# sg: lista de listas
.subGroupLines <- function(o, so, gg, sg) {
  txt <- list()
  
  # extrae partes
  sg.pos <- unlist(sg$char.index, unlist)
  sg.defs <- sg$defs[[1]]$connotation
  
  # counters
  i <- 1
  j <- 1
  # copia local de posiciones
  sg.pos.temp <- sg.pos
  
  # itera sobre partes
  while(i < length(sg.pos)+1) {
    
    # agrega todas las barras
    txt[[j]] <- .makeBars(pos=c(sg.pos.temp, gg$char.index, so$char.index, o$char.index))
    txt[[j+1]] <- .printExplanation(pos = sg.pos.temp[1], txt = sg.defs[1])
    
    # vectores mordisqueadores?
    sg.pos.temp <- sg.pos.temp[-1]
    sg.defs <- sg.defs[-1]
    
    # increment vars
    j <- j+2
    i <- i+1
  }
  
  return(txt)
}
