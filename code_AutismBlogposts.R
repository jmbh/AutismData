# jonashaslbeck@gmail.com

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ---- Post A: Estimating Mixed Graphical Models -----------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# ---------- Loading packages ------------------------------------------
# ----------------------------------------------------------------------

library(devtools)
#install_github('SachaEpskamp/qgraph') # we need 1.3.3
library(qgraph)
library(mgm)


# ----------------------------------------------------------------------
# ---------- Load data -------------------------------------------------
# ----------------------------------------------------------------------

datalist <- readRDS('autism_datalist.RDS')
data <- datalist$data
type <- datalist$type
lev <- datalist$lev

round(data[1:4, 1:5],2)

type

lev

# ----------------------------------------------------------------------
# ---------- Estimate mgm ----------------------------------------------
# ----------------------------------------------------------------------

fit <- mgmfit(data, type, cat, lamda.sel="EBIC", d=2)
saveRDS(fit, file='fitobj_mixed.RDS') # save estimation object
fit <- readRDS(file='fitobj_mixed.RDS') # read estimation object


# ----------------------------------------------------------------------
# ---------- Plotting --------------------------------------------------
# ----------------------------------------------------------------------

# define group labels
groups_type <- list("Demographics"=c(1,14,15,28), 
                    "Psychological"=c(2,4,5,6,18,20,21),
                    "Social environment" = c(7,16,17,19,26,27),
                    "Medical"=c(3,8,9,10,11,12,13,22,23,24,25))

# pick some nice colors
group_col <- c("#72CF53", "#53B0CF", "#FFB026", "#ED3939")

# plot
jpeg("Autism_FirstGraph.jpg", height=2*900, width=2*1300, unit='px')
Q0 <- qgraph(fit$adj, 
       vsize=3.5, 
       esize=2, 
       layout="spring", 
       edge.color = rgb(33,33,33,100, maxColorValue = 255), 
       color=group_col,
       border.width=1.5,
       border.color="black",
       groups=groups_type,
       nodeNames=datalist$colnames,
       legend=TRUE, 
       legend.mode="style2",
       legend.cex=1.5)
dev.off()




# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ---- Post B: A closer look at Interactions between Cat Var in MGM ----
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# ---------- Loading packages ------------------------------------------
# ----------------------------------------------------------------------

library(devtools)
#install_github('SachaEpskamp/qgraph') # we need 1.3.3
library(qgraph)
#install_github('jmbh/mgm') # we need 1.1-6
library(mgm)


# ----------------------------------------------------------------------
# ---------- Loading data ----------------------------------------------
# ----------------------------------------------------------------------

datalist <- readRDS("autism_datalist.RDS") 
data <- datalist$data
type <- datalist$type
lev <- datalist$lev

## possibly interesting categoricals variables:

# a) Well-being [1:5 ordinal]
data$`Well being`
table(data$`Well being`)

# b) Integraion in Society [1==yes, 2=partially, 3==nein]
data$`Integration in Society`
table(data$`Integration in Society`)

# c) type of work (1==no, 2=supervised, 3=unpaid, 4=paid)
data$`Type of work`
table(data$`Type of work`)

# d) Satisfaction with social contacts (1==yey, 2=no, 3=neutral)
data$`Satisfaction: Social Contacts`
table(data$`Satisfaction: Social Contacts`)


# ----------------------------------------------------------------------
# ---------- Estimation ----------------------------------------------
# ----------------------------------------------------------------------

fit <- mgmfit(data, type, lev, lambda.sel="EBIC", d=2)
saveRDS(fit, file='fitobj_mixed.RDS') # save estimation object
fit <- readRDS(file='fitobj_mixed.RDS') # read estimation object


# ----------------------------------------------------------------------
# ---------- Same graph, but grouped by Variable type ------------------
# ----------------------------------------------------------------------

colnames(graph) <- datalist$colnames

# define variable types
groups_typeV <- list("Gaussian"=which(datalist$type=='g'), 
                     "Poisson"=which(datalist$type=='p'),
                     "Categorical"=which(datalist$type=='c'))

# pick some nice colors
group_col <- c("#72CF53", "#53B0CF", "#ED3939")


jpeg("Autism_VarTypes.jpg", height=2*900, width=2*1300, unit='px')
par(mfrow=c(1,1), mar=c(0,0,0,0))
qgraph(fit$adj, 
       vsize=3.5, 
       esize=2, 
       layout=Q0$layout, 
       color=group_col,
       border.width=1.5,
       border.color="black",
       groups=groups_typeV,
       nodeNames=datalist$colnames,
       legend=TRUE, 
       legend.mode="style2",
       legend.cex=1.5)
dev.off()



# ----------------------------------------------------------------------
# ---------- Recovering Edge weights & signs of continuous variables ---
# ----------------------------------------------------------------------

wgraph <- fit$wadj # weighted adjacency matrix

jpeg("Autism_VarTypes_WeightAndSign.jpg", height=2*900, width=2*1300, unit='px')
par(mfrow=c(1,1), mar=c(0,0,0,0))
qgraph(wgraph, 
       vsize=3.5, 
       esize=5, 
       layout=Q0$layout, 
       edge.color = fit$edgecolor, 
       color=group_col,
       border.width=1.5,
       border.color="black",
       groups=groups_typeV,
       nodeNames=datalist$colnames,
       legend=TRUE, 
       legend.mode="style2",
       legend.cex=1.5)
dev.off()



# ----------------------------------------------------------------------
# ---------- Exploring Categorical Interactions ------------------------
# ----------------------------------------------------------------------


# a) Interaction Cat-Pois: (16) Type of work X (17) Workinghours
matrix(fit$mpar.matrix[fit$par.labels == 16, fit$par.labels == 17], ncol=1)


# b) Interaction Cat-Cat:
fit$mpar.matrix[fit$par.labels == 14, fit$par.labels == 16]















