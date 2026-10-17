# main.R: runs the whole analysis in the correct order.
# Set the working directory to the project folder first (the one containing R/ and data/).
for (f in sort(list.files("R", pattern = "^0.*\\.R$", full.names = TRUE))) {
  message("Running ", f)
  source(f, echo = FALSE)
}
