#fakedata
library(openxlsx)

fake_1 <- read.xlsx("data-raw/data/gegenereerde data voor verschillende designs.xlsx", sheet = "one way")[,1:3]

fake_2cros <- read.xlsx("data-raw/data/gegenereerde data voor verschillende designs.xlsx", sheet = "two-way crossed")[,1:3]

fake_2nest <- read.xlsx("data-raw/data/gegenereerde data voor verschillende designs.xlsx", sheet = "two-way nested")
colnames(fake_2nest) <- c("patient", "rater", "score")

fake_3cros <- read.xlsx("data-raw/data/gegenereerde data voor verschillende designs.xlsx", sheet = "three-way crossed")
colnames(fake_3cros) <- c("patient", "technician","rater", "score")


fake_3nest <- read.xlsx("data-raw/data/gegenereerde data voor verschillende designs.xlsx", sheet = "three-way nested")
colnames(fake_3nest) <- c("patient", "technician","rater", "score")


fake_3nestn <- read.xlsx("data-raw/data/gegenereerde data voor verschillende designs.xlsx", sheet = "three-way nested unequal n")
colnames(fake_3nestn) <- c("patient", "technician","rater", "score")


save(fake_1, file = "data/fake_1.rda")
save(fake_2cros, file = "data/fake_2cros.rda")
save(fake_2nest, file = "data/fake_2nest.rda")
save(fake_3cros, file = "data/fake_3cros.rda")
save(fake_3nest, file = "data/fake_3nest.rda")
save(fake_3nestn, file = "data/fake_3nestn.rda")
