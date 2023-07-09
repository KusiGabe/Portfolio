library(dplyr)
library(arsenal)
#install.packages("arsenal")
while (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

dfcc <- read.csv(file.choose())
rownames(dfcc) <- dfcc[, 1]
dfcc <- dfcc[, -1]
boxplot.stats(dfcc$PURCHASES_INSTALLMENTS_FREQUENCY)
# Creating Outlier data using boxplot.stat() function of respective columns
###################
dfc1 <- data.frame(BALANCE = boxplot.stats(dfcc$BALANCE)$out,
                   OUT_ID = c(1:695))
dfc2 <- data.frame(BALANCE_Frequency = boxplot.stats(dfcc$BALANCE_FREQUENCY)$out,
                   OUT_ID = c(1:1493))
dfc3 <- data.frame(PURCHASES = boxplot.stats(dfcc$PURCHASES)$out,
                   OUT_ID = c(1:808))
dfc4 <- data.frame(ONEOFF_PURCHASES = boxplot.stats(dfcc$ONEOFF_PURCHASES)$out,
                   OUT_ID = c(1:1013))
dfc5 <- data.frame(INSTALLMENT_PURCHASES = boxplot.stats(dfcc$INSTALLMENTS_PURCHASES)$out,
                   OUT_ID = c(1:867))
dfc6 <- data.frame(CASH_ADVANCE = boxplot.stats(dfcc$CASH_ADVANCE)$out,
                   OUT_ID = c(1:1030))
##dfc7 <- data.frame(PURCHF_out = boxplot.stats(dfcc$PURCHASES_FREQUENCY)$out,
                  ## OUT_ID = c(1:nrow(dfc7)))
dfc8 <- data.frame(ONEOFF_PURCHASES_FREQUENCY = boxplot.stats(dfcc$ONEOFF_PURCHASES_FREQUENCY)$out,
                   OUT_ID = c(1:782))
##dfc9 <- data.frame(IPURCHF1_out = boxplot.stats(dfcc$PURCHASES_INSTALLMENTS_FREQUENCY)$out,
                  ## OUT_ID = c(1:nrow(dfc9)))
dfc10 <- data.frame(CASH_ADVANCE_FREQUENCY = boxplot.stats(dfcc$CASH_ADVANCE_FREQUENCY)$out,
                    OUT_ID = c(1:525))
dfc11 <- data.frame(CASH_ADVANCE_TRX = boxplot.stats(dfcc$CASH_ADVANCE_TRX)$out,
                    OUT_ID = c(1:804))
dfc12 <- data.frame(CREDIT_LIMIT = boxplot.stats(dfcc$CREDIT_LIMIT)$out,
                    OUT_ID = c(1:248))
dfc13 <- data.frame(PAYMENTS = boxplot.stats(dfcc$PAYMENTS)$out,
                    OUT_ID = c(1:808))
dfc14 <- data.frame(MINIMUM_PAYMENTS = boxplot.stats(dfcc$MINIMUM_PAYMENTS)$out,
                    OUT_ID = c(1:841))
dfc15 <- data.frame(PRC_FULL_PAYMENT = boxplot.stats(dfcc$PRC_FULL_PAYMENT)$out,
                    OUT_ID = c(1:1474))
dfc16 <- data.frame(TENURE = boxplot.stats(dfcc$TENURE)$out,
                    OUT_ID = c(1:1366))
dfc17 <- data.frame(PURCHASES_TRX = boxplot.stats(dfcc$PURCHASES_TRX)$out,
                    OUT_ID = c(1:766))

#Joining the data frames together to make one "total" outlier df
# created my own unique id so that I could join all the outlier dfs
#dfcOut1 needed to be added to create a
dfcOut1 <- dfcc2 %>% select(PURCHASES_FREQUENCY, 
                          PURCHASES_INSTALLMENTS_FREQUENCY,
                          OUT_ID)
dfcOut <- dfc2 %>% 
                left_join(dfc1, by = "OUT_ID")
dfcOut <- dfcOut %>% 
              left_join(dfc3, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc4, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc5, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc6, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc7, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc8, by = "OUT_ID")
## <- dfcOut %>% 
                ##left_join(dfc9, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc10, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc11, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc11, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc12, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc13, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc14, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc15, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc16, by = "OUT_ID")
dfcOut <- dfcOut %>% 
                left_join(dfc17, by = "OUT_ID")

################################################################################

view(dfcOut)

comparedf(dfcc, dfcOut) #used this function to make sure the outlier df and 
# original df had shared variables and values

dfcc2 <- dfcc %>% mutate(OUT_ID = c(1:nrow(dfcc)))
#The above block of code removes the non-shared columns (columns were not
# included in the outlier df becuase there were no outliers in these columns)
# so that I could join the original df and the outlier df

dfccNO <- dfcc2 %>% 
          anti_join(dfcOut, by = "OUT_ID")
dfccNO <- dfccNO %>% mutate(OUT_ID = c(1:nrow(dfccNO)))
# Used anti_join() function to join the two dfs while also removing the shared
#columns and values (outliers)
#In doing so, however, it removed the "OUT_ID," "PURCHASES," and 
# "PURCHASES_INSTALLMENTS_FREQUENCY," which needed to be re-added.
#OUT_ID is added above whereas the other columns are added below

#Below block of code creates the dataframe of the selected columns I will
# use for clustering. These columns were selected based on their relatively
# small or lack of outliers using the no outlier dataframe (dfccNO)
dfcol <- dfccNO %>% right_join(dfcOut1, by = "OUT_ID") %>% 
  select(PURCHASES_FREQUENCY.x, 
         PURCHASES_INSTALLMENTS_FREQUENCY.x, 
         CASH_ADVANCE_FREQUENCY, CREDIT_LIMIT) %>% na.omit()

### clustering non-outliers
dfccNORM <- sapply(dfcol, scale)
round(head(dfccNORM), 2)                  
# Normalizing the values within the df to prep that data for clustering
d <- dist(dfccNORM, method = "euclidean")
round(d, 1)
hcl <- hclust(d, method = "complete")
plot(hcl, hang = -1, ann= TRUE)
#calculating distance and hierarical clustering to visualize how each record
# compares to one another
memb <- cutree(hcl, k=5)
memb  
table(memb)
# used above code to print to console the cluster location of each record
# within the data frame

row.names(dfccNORM) <- paste(memb, ": ", row.names(dfccNORM), sep='')  
head(dfccNORM)  
heatmap(dfccNORM, Colv = NA, hclustfun = hclust) 
#heatmap function furthervisualizes the relationship between the records 
# across the columns

clu <- kmeans(d, 3)
clu <- kmeans(d, 4)
clu <- kmeans(d, 5)
clu <- kmeans(d, 6)
# kmeans method of clustering using defined distance (d) to help dictate 
#the correct number of clusters. Picked 4 clusters because the avg. distance 
#between centers in 3 clusters is larger than that of 3,5, 6 clusters. 

clu$cluster
clu$size
clu$withinss
dist(clu$centers)
mean(clu$withinss)
mean(dist(clu$centers))
#The above 6 lines of code give me the dimensions of the cluster such as the
# size, distance between centers, location of records with respect to the 
#cluster, etc. 

FinalNH <- dfcol %>% 
              mutate(Cluster_NH = clu$cluster)

FinalH <- dfcol %>% 
                mutate(Cluster = memb)
# Created FinalNH(df using clusters with non-hierarchical) and FinalH(df using 
#clusters with hierarchical)
summary(filter(FinalH,Cluster == 1))
summary(filter(FinalH, Cluster == 2))
summary(filter(FinalH, Cluster == 3))
summary(filter(FinalH, Cluster == 4))
# 3 lines of code above show summary metrics of the values of each column
# (min, 1st Q, Med, Mean, 3rd Q, Max, NA's) for non-hierarchical clust.
summary(filter(FinalNH, Cluster == 1))
summary(filter(FinalNH, Cluster == 2))
summary(filter(FinalNH, Cluster == 3))

# 4 lines of code above show summary metrics of the values of each column
# (min, 1st Q, Med, Mean, 3rd Q, Max, NA's) for hierarchical clust.
FinalNH <- FinalNH %>% 
           mutate(NH_Value = ifelse(clu$cluster == 1, "MED",
                          ifelse(clu$cluster == 2, "HIGH", 
                          ifelse(clu$cluster == 3, "LOW", "V LOW"))))

FInal <- FinalNH %>% mutate(H_Value = FinalH$Usage,
                            Cluster_H = FinalH$Cluster)
#Above is the final data frame that indicates the differences in cluster
#location of each record and notates them as High, Med, Low

FinalNHL <- FinalNH %>% 
            filter(Usage == "LOW")
FinalNHM <- FinalNH %>% 
            filter(Usage == "MED")
FinalNHH <- FinalNH %>% 
            filter(Usage == "HIGH")
FinalNHVH <- FinalNH %>% 
             filter(Usage == "V LOW")

FinaLH <- FinalH %>% 
  mutate(Usage = ifelse(memb == 1, "MED",
                        ifelse(memb == 2, "HIGH", 
                               ifelse(memb == 3, "V LOW", "LOW"))))
FinalHL <- FinalH %>% 
            filter(Usage == "MED")
FinalNHM <- FinalNH %>% 
            filter(Usage == "HIGH")
FinalNHH <- FinalNH %>% 
  filter(Usage == "V LOW")
FinalNHVH <- FinalNH %>% 
        filter(Usage == "LOW")

# The final ounce of code creates a new column "Usage" (value derived from 
# NH summary function) which determines how much a credit card was used by
# a customer. As seen above, cluster 1 is low, cluster 2 is med, cluster
# 3 is high, and then very high. 