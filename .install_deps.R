pkg <- strsplit(read.dcf("DESCRIPTION")[, "Depends"], ",")$Depends
pkg <- sub("\\W", "", pkg)
for (p in pkg) {
    install.packages(p, repos = "http://cran-r.c3sl.ufpr.br/")
}
