library(circlize)
grid.col = c(CSAT = "grey70", 
             IR = "grey70", 
             AHT = "grey70", 
             ACW = "grey70",
             "Escalation Rate" = "grey70",
             Knowledge = "green",
             Logging = "purple",
             Tools = "blue",
             "Case Duration" = "orange",
             Holds = "darkblue",
             Professionalism = "darkgreen",
             Guidance = "darkred",
             Ownership = "darkcyan")

chordDiagram(data, annotationTrack = c("grid"),
             transparency = 0.4,
             grid.col = grid.col,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(data))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 3,col = "grey30")
}, bg.border = NA)

circos.clear()