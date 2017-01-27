# NOTE: all this code borrowed from ~\Dropbox\unc.grad.school\my-papers\ms-201608-1\programs\slides\tables-slides.R
library(readxl)
library(reshape2)
require(ggplot2)

# t3.csv is from a4.R program in ~\Dropbox\unc.grad.school\my-papers\ms-201608-1\programs\kure-analysis\
t3f = read.csv(file="C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/kure-analysis/t3.csv")

t3.sub = t3f[,c(2,3,5)] # take out unadjusted estimates
colnames(t3.sub) = c("lipid", "Female", "Male")
t3.sub$country = "Chile"

# Specify sheet with a number or name
tt4 = read_excel("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/slides/compare-est.xlsx", sheet = "table 4, Tikkanen")
colnames(tt4) = c("lipid", "Female", "Male")
tt4$country = "Finland"

fig3.both = rbind(tt4, t3.sub)
fig3.both

fig3.long = melt(fig3.both, id.var=c("lipid",'country'))
fig3.long

fig3.long$est = as.numeric(substr(fig3.long$value,1,4))
fig3.long$se = as.numeric(substr(fig3.long$value,7,10))
fig3.long

# Define the top and bottom of the errorbars
limits <- aes(ymax = est + 2*se, ymin = est - 2*se)

p <- ggplot(fig3.long, aes(y=est, x=country, colour=country)) +
  facet_grid(lipid~variable) +
  geom_point(size=4) + geom_errorbar(limits, width=0.2, size=1.1) +
  coord_flip() +
  theme_bw(base_size = 20) +
  xlab("Country") +
  ylab("Coefficient (95% CI)") +
  geom_hline(yintercept = 0, colour="black", linetype = "longdash") 
  #ggtitle("Weighted polygenic risk score (wPRS) regression coefficients by country and gender")

p

png(filename="AHA-feb-2017/figures/slcs-ms-results.png",
    width=8, height=6, units="in", res=300)
  p
dev.off()