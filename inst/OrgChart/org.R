library(graph)
library(Rgraphviz)
orgdata = read.delim("orgdata.tsv", h=TRUE, sep="\t")
stopifnot(all(orgdata$super %in% c(" ", "Bioc", orgdata$name)))
process_nodes = function(tab) {
  bas = sprintf("%s(%s)", tab$name, tab$institute)
  sprintf("%s\n%s", bas, tab$task)
}
orgnodes = process_nodes(orgdata)
names(orgnodes) = orgdata$name
sups = orgnodes[orgdata$super]
okinds = setdiff(seq_len(length(sups)), which(is.na(sups)))
nn = new("graphNEL", nodes=orgnodes, edgemode="directed")
for (i in okinds) nn = addEdge(sups[i], orgnodes[i], nn)
graph.par(list(nodes = list(shape = "plaintext", cex = 1.2)))
gn = layoutGraph(nn)
pdf(file="newgr2.pdf", width=19)
renderGraph(gn)
dev.off()
