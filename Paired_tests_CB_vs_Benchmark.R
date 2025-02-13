# Checking the difference between the CB-languages and their respective benchmarks
# with respect to the distances to the (other) CB-languages
install.packages("rstatix")
library(rstatix)

# 1. Uploading and preparing the distance matrix

distances_df <- read.table(file.choose(), header = T, sep = "\t")
distances <- as.matrix(distances_df[,-c(1:2)])
rownames(distances) <- colnames(distances)

# select columns containing only the CB lgs
CB_distances <- distances[ , which(distances_df[,1] == "CB")]

# Baltic Romani and Vlax Romani
# Save the distances to CB from each language in a separate vector
rml <- CB_distances[8,c(-8)]
rmy <- CB_distances[15,c(-8)]

# Carry out the Wilcoxon signed rank test
wilcox.test(rml, rmy, paired = T)

# Inspect the means
mean(rml)
mean(rmy)

# Create a long data frame with the values for the two languages
Rml_Rmy <- data.frame(distance = c(rml, rmy),
                      language = rep(c("rml", "rmy"), each = length(rml))
)

# Compute effect size for Wilcoxon signed-rank test
wilcox_effsize(Rml_Rmy, distance ~ language, paired = TRUE)


# Belarusian & Ukrainian
# Save the distances to CB from each language in a separate vector
bel <- CB_distances[1,c(-1)]
ukr <- CB_distances[16,c(-1)]

# Carry out the Wilcoxon signed rank test
wilcox.test(bel, ukr, paired = T)

# Inspect the means
mean(bel)
mean(ukr)

# Create a long data frame with the values for the two languages
Bel_Ukr <- data.frame(distance = c(bel, ukr),
          language = rep(c("bel", "ukr"), each = length(bel))
          )

# Compute effect size for Wilcoxon signed-rank test
wilcox_effsize(Bel_Ukr, distance ~ language, paired = TRUE)


# Estonian and Hungarian
# Save the distances to CB from each language in a separate vector
est <- CB_distances[3,c(-3)]
hun <- CB_distances[12,c(-3)]

# Carry out the Wilcoxon signed rank test
wilcox.test(est, hun, paired = T)

# Inspect the means
mean(est)
mean(hun)

# Create a long data frame with the values for the two languages
Est_Hun <- data.frame(distance = c(est, hun),
                      language = rep(c("est", "hun"), each = length(est))
)

# Compute effect size for Wilcoxon signed-rank test
wilcox_effsize(Est_Hun, distance ~ language, paired = TRUE)


# Finnish and Hungarian
# Save the distances to CB from each language in a separate vector
fin <- CB_distances[4,c(-4)]
hun <- CB_distances[12,c(-4)]

# Carry out the Wilcoxon signed rank test
wilcox.test(fin, hun, paired = T)

# Inspect the means
mean(fin)
mean(hun)

# Create a long data frame with the values for the two languages
Fin_Hun <- data.frame(distance = c(fin, hun),
                      language = rep(c("fin", "hun"), each = length(fin))
)

# Compute effect size for Wilcoxon signed-rank test
wilcox_effsize(Fin_Hun, distance ~ language, paired = TRUE)

# German and Dutch
# Save the distances to CB from each language in a separate vector
ger <- CB_distances[2,c(-2)]
ndl <- CB_distances[13,c(-2)]

# Carry out the Wilcoxon signed rank test
wilcox.test(ger, ndl, paired = T)

# Inspect the means
mean(ger)
mean(ndl)

# Create a long data frame with the values for the two languages
Ger_Ndl <- data.frame(distance = c(ger, ndl),
                      language = rep(c("ger", "ndl"), each = length(ger))
)

# Compute effect size for Wilcoxon signed-rank test
wilcox_effsize(Ger_Ndl, distance ~ language, paired = TRUE)


# Polish and Czech
# Save the distances to CB from each language in a separate vector
pol <- CB_distances[7,c(-7)]
ces <- CB_distances[11,c(-7)]

# Carry out the Wilcoxon signed rank test
wilcox.test(pol, ces, paired = T)

# Inspect the means
mean(pol)
mean(ces)

# Create a long data frame with the values for the two languages
Pol_Ces <- data.frame(distance = c(pol, ces),
                      language = rep(c("pol", "ces"), each = length(pol))
                      )

# Compute effect size for Wilcoxon signed-rank test
wilcox_effsize(Pol_Ces, distance ~ language, paired = TRUE)

# Russian and Ukrainian
# Save the distances to CB from each language in a separate vector
rus <- CB_distances[9,c(-9)]
ukr <- CB_distances[16,c(-9)]

# Carry out the Wilcoxon signed rank test
wilcox.test(rus, ukr, paired = T)

# Inspect the means
mean(rus)
mean(ukr)

# Create a long data frame with the values for the two languages
Rus_Ukr <- data.frame(distance = c(rus, ukr),
                      language = rep(c("rus", "ukr"), each = length(rus))
)

# Compute effect size for Wilcoxon signed-rank test
wilcox_effsize(Rus_Ukr, distance ~ language, paired = TRUE)

# Swedish and Norwegian
# Save the distances to CB from each language in a separate vector
swe <- CB_distances[10,c(-10)]
nob <- CB_distances[14,c(-10)]

# Carry out the Wilcoxon signed rank test
wilcox.test(swe, nob, paired = T)

# Inspect the means
mean(swe)
mean(nob)

# Calculate means for the two remaining CB-languages without a Benchmark

# Latvian
mean(CB_distances[5,c(-5)])
# Lithuanian
mean(CB_distances[6,c(-6)])