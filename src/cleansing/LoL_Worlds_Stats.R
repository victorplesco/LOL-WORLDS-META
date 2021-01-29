source("./SMARTDRAFT/src/packages/attach.packages.R");

worldsSTATS <- data.frame(K      = rep(NA, 10),
                          D      = rep(NA, 10),
                          A      = rep(NA, 10),
                          KDA    = rep(NA, 10),
                          CS     = rep(NA, 10),
                          CSxM   = rep(NA, 10),
                          G      = rep(NA, 10),
                          GxM    = rep(NA, 10),
                          Year   = as.integer(2011:2020));
  
#################################################################################################################################################################################################################################################################################
## nodes ########################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

nodes <- read_excel(paste0(getwd(), "/SMARTDRAFT/data/metadata/gamepedia/LoL_Worlds_PickBanHistory.xlsx"), sheet = 11);
nodes$Release <- as.numeric(gsub("\\-.*", "", as.character(nodes$Release))); nodes$Class <- as.factor(gsub("\\-.*", "", as.character(nodes$Class))); 
require(ggrepel)
for(i in 1:10)
{
  worldsLoL = as.data.frame(read_excel(paste0(getwd(), "/SMARTDRAFT/data/metadata/gamepedia/LoL_Worlds_Stats.xlsx"), sheet = i));  
  worldsLoL$Class <- as.factor(left_join(worldsLoL, nodes[, c(1:2)], by = c("Champion" = "Name"))[, ncol(worldsLoL) + 1]); rownames(worldsLoL) <- worldsLoL[, 1]; worldsLoL[, 1] <- NULL; 
  worldsLoL$WR <- gsub("\\%.*", "", as.character(worldsLoL$WR)); worldsLoL[, -ncol(worldsLoL)] <- lapply(worldsLoL[, -ncol(worldsLoL)], function(x) as.numeric(as.character(x)));
  worldsLoL <- worldsLoL[-which(worldsLoL$Games <= 2), ]; worldsLoL$Games <- NULL;
  
  regression.fit <- lm(WR ~ ., data = worldsLoL); 
  worldsSTATS[i, 1:8] <- as.numeric(regression.fit$coefficients[c(2:9)]);
}; rm(i, worldsLoL, regression.fit, nodes);

worldsSTATS$K_Label    <- NA; worldsSTATS$K_Label[nrow(worldsSTATS)]    <- "Kills";
worldsSTATS$D_Label    <- NA; worldsSTATS$D_Label[nrow(worldsSTATS)]    <- "Deaths";
worldsSTATS$A_Label    <- NA; worldsSTATS$A_Label[nrow(worldsSTATS)]    <- "Assists";
worldsSTATS$CSxM_Label <- NA; worldsSTATS$CSxM_Label[nrow(worldsSTATS)] <- "CS/M";

(g_coef <- ggplot(data = worldsSTATS, aes(x = Year)) +
  
  geom_line(aes(y = K),    col = "#f8766d",     size = 2, alpha = 0.75) + 
  geom_line(aes(y = D),    col = "#b79f00", size = 2, alpha = 0.75) + 
  geom_line(aes(y = A),    col = "#00ba38", size = 2, alpha = 0.75) + 
  geom_line(aes(y = CSxM), col = "#00bfc4",       size = 2, alpha = 0.75) + 
   
  geom_label_repel(aes(y = K,    label = K_Label),    size = 5, alpha = 0.75) +
  geom_label_repel(aes(y = D,    label = D_Label),    size = 5, alpha = 0.75) +
  geom_label_repel(aes(y = A,    label = A_Label),    size = 5, alpha = 0.75) +
  geom_label_repel(aes(y = CSxM, label = CSxM_Label), size = 5, alpha = 0.75) +

  labs(title = "Regression Coefficients for In-Game Statistics",
       x     = "Year",
       y     = "Coefficient") +
  theme_bw(base_size = 14, base_family = "Times") +
  scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020), labels = c("2012", "2014", "2016", "2018", "2020")))
ggsave(file = "./SMARTDRAFT/report/figures/regression_1120.png", g_coef, width = 20, height = 11.25);

