source("./SMARTDRAFT/src/packages/attach.packages.R");

for(i in 1:10)
{
  if(i < 7)
  {
    tmp = read_excel(paste0(getwd(), "/SMARTDRAFT/data/metadata/gamepedia/LoL_Worlds_PickBanHistory.xlsx"), sheet = i)[, c(1:5, 7:16)];  
    colnames(tmp)[6:15] <- c("BP1", "BP2", "BP3", "BP4", "BP5", "RP1", "RP2", "RP3", "RP4", "RP5"); tmp <- tmp[, c(6, 11, 12, 7, 8, 13, 14, 9, 10, 15, 1:5)];
  }
  
  if(i >= 7)
  {
    champions = TRUE; if(champions) {
      tmp = read_excel(paste0(getwd(), "/SMARTDRAFT/data/metadata/gamepedia/LoL_Worlds_PickBanHistory.xlsx"), sheet = i)[, c(15:20, 25:28, 1, 3:4, 6:7)]
    } else {tmp = read_excel(paste0(getwd(), "/SMARTDRAFT/data/metadata/gamepedia/LoL_Worlds_PickBanHistory.xlsx"), sheet = i)[, c(29:38, 1, 3:4, 6:7)][, c(c(1, 6, 7, 2, 3, 8, 9, 4, 5, 10), c(11:16))];}; rm(champions);
  }; 
  
  if(i == 1) {worldsLoL = tmp; rm(tmp);} else {worldsLoL = rbind(worldsLoL, tmp); rm(tmp);}
  
}; rm(i); worldsLoL$Year <- as.numeric(worldsLoL$Year);

#################################################################################################################################################################################################################################################################################
## combinations #################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

source("./SMARTDRAFT/src/support/build.sequence.R");
# Building the rules for each combination with relative attributes;
dtf_Combinations  <- get_Combination(x = worldsLoL[, 1:10], gattr = worldsLoL[, 11:15]) %>% mutate(GROUP_OK = paste0(PickBLUE, "-", PickRED),
                                                                                                   GROUP_KO = paste0(PickRED, "-", PickBLUE));

CHM_duplicates <- c(); IDX_duplicates <- 0; for(i in 1:nrow(dtf_Combinations))
{
  if(dtf_Combinations$GROUP_OK[i] %in% CHM_duplicates == FALSE & length(which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i])) != 0)
  {
    if(dtf_Combinations$GROUP_KO[i] %in% CHM_duplicates) {next;} else {IDX_duplicates = IDX_duplicates + 1; CHM_duplicates[IDX_duplicates] <- dtf_Combinations$GROUP_OK[i];
      dtf_Combinations[which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i]), 1] = dtf_Combinations$PickBLUE[i];
      dtf_Combinations[which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i]), 6] = ifelse(dtf_Combinations$ResultBLUE[which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i])] == "Win", "Lose", "Win");
      dtf_Combinations[which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i]), 2] = dtf_Combinations$PickRED[i];
      dtf_Combinations[which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i]), 7] = ifelse(dtf_Combinations$ResultRED[which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i])] == "Win", "Lose", "Win");
    }
  }
}; cat("Before: ", length(CHM_duplicates), "After: ", length(unique(CHM_duplicates))); rm(i, CHM_duplicates, IDX_duplicates);

{ # Visual guard;
  dtf_Combinations$GROUP_OK = paste0(dtf_Combinations$PickBLUE, "-", dtf_Combinations$PickRED);
  dtf_Combinations$GROUP_KO = paste0(dtf_Combinations$PickRED, "-", dtf_Combinations$PickBLUE);
  for(i in 1:nrow(dtf_Combinations)) {dtf_Combinations[i, "Count"] = length(which(dtf_Combinations$GROUP_OK == dtf_Combinations$GROUP_KO[i]));}; rm(i);
}; cat("Number of duplicates: ", sum(dtf_Combinations$Count)); dtf_Combinations$Count <- NULL; dtf_Combinations$GROUP_OK <- NULL; dtf_Combinations$GROUP_KO <- NULL;

#################################################################################################################################################################################################################################################################################
## nodes ########################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

nodes <- read_excel(paste0(getwd(), "/SMARTDRAFT/data/metadata/gamepedia/LoL_Worlds_PickBanHistory.xlsx"), sheet = 11);
nodes$Release <- as.numeric(gsub("\\-.*", "", as.character(nodes$Release))); nodes$Class <- as.factor(gsub("\\-.*", "", as.character(nodes$Class))); 

#################################################################################################################################################################################################################################################################################
## edges ########################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

edges <- dtf_Combinations[, c(1:3, 6)] %>% 
  rename(from   = PickBLUE,
         to     = PickRED) %>%
  mutate(GROUP = paste0(from, "-", to)) %>%
  group_by(Year, GROUP) %>% 
  mutate(ResultBLUE  = ifelse(ResultBLUE == "Win", 1, 0),
         winrate     = sum(ResultBLUE)/n(),
         frequency   = n()) %>%
  ungroup() %>%
  select(-ResultBLUE, -GROUP) %>%
  group_by(Year) %>%
  mutate(Weight = winrate * frequency/(n()/25)) %>%
  ungroup() %>%
  select(-frequency, -winrate); edges <- unique(na.omit(edges)); edges <- edges[, c(1, 2, 4, 3)]; rm(dtf_Combinations, worldsLoL);

#################################################################################################################################################################################################################################################################################
## graph plot ###################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

source("./SMARTDRAFT/src/support/build.layout.R");
  
# Possible Options:
# (1) byClass = FALSE & simplified = FALSE: general networks over years (only weights and centrality considered);
# (2) byClass = TRUE  & simplified = FALSE: general networks over years (weights, centrality and class considered);
# (3) byClass = FALSE & simplified = TRUE:  simplified networks over years (class and average centrality per class considered);
  
all_plots_byclassF <- get_Plots(nodes = nodes, edges = edges, layout = "lgl", byClass = TRUE, centrality = "Eigen", simplified = FALSE);

g_all <- ggpubr::ggarrange(all_plots_byclassF[[1]], all_plots_byclassF[[2]], all_plots_byclassF[[3]], 
                      all_plots_byclassF[[4]], all_plots_byclassF[[5]], all_plots_byclassF[[6]],
                      all_plots_byclassF[[7]], all_plots_byclassF[[8]], all_plots_byclassF[[9]], 
                      all_plots_byclassF[[10]], nrow = 2, ncol = 5, common.legend = TRUE, legend = "bottom"); g_all;
ggsave(file = "./SMARTDRAFT/report/figures/ggraph_1120.png", g_all, width = 20, height = 11.25);

#################################################################################################################################################################################################################################################################################
## graph plot (barplot) ######################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

all_plots_simplifiedT <- list(); color <- c("#f8766d", "#b79f00", "#00ba38", "#00bfc4", "#619cff", "#f564e3"); idx_Worlds <- 0; for(i in 2011:2020) 
{
  idx_Worlds = idx_Worlds + 1; tmp <- get_Layout(nodes = nodes, edges = edges, year = i, layout = "lgl", byClass = FALSE, simplified = TRUE)$Graph %>% activate(nodes) %>% data.frame();
  tmp <- tmp[order(tmp$Name),]; tmp$Index <- 1:6;
  all_plots_simplifiedT[[idx_Worlds]] <- ggplot(data = tmp, aes(x = Name, y = Eigen, fill = as.factor(as.character(Name)))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color) + 
    
    theme_minimal(base_size = 13, base_family = "Times") +
    labs(fill = "Class") +
    
    if(i == 2011 | i == 2016)
    {
      labs(title = paste0("", i),
           x     = "",
           y     = "Avg. Eigenvector Centrality")
    } else {
    labs(title = paste0("", i),
         x     = "",
         y     = "")
    }
};

g_all <- ggpubr::ggarrange(all_plots_simplifiedT[[1]], all_plots_simplifiedT[[2]], all_plots_simplifiedT[[3]], 
                           all_plots_simplifiedT[[4]], all_plots_simplifiedT[[5]], all_plots_simplifiedT[[6]],
                           all_plots_simplifiedT[[7]], all_plots_simplifiedT[[8]], all_plots_simplifiedT[[9]], 
                           all_plots_simplifiedT[[10]], nrow = 2, ncol = 5, common.legend = TRUE, legend = "bottom"); g_all;
ggsave(file = "./SMARTDRAFT/report/figures/barplot_1120.png", g_all, width = 20, height = 11.25);

#################################################################################################################################################################################################################################################################################
## top 10 eigen #################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

source("./SMARTDRAFT/src/support/build.layout.R");

n <- 50; k = 10;
worlds_LoL_TOP10 <- data.frame(Name        = rep(NA, n),
                               Class       = rep(NA, n),
                               Release     = rep(NA, n),
                               Index       = rep(NA, n),
                               Degree      = rep(NA, n),
                               PageRank    = rep(NA, n),
                               Betweenness = rep(NA, n),
                               Eigen       = rep(NA, n),
                               Year        = c(rep(2011, k), rep(2012, k), rep(2013, k), rep(2014, k), rep(2015, k),
                                               rep(2016, k), rep(2017, k), rep(2018, k), rep(2019, k), rep(2020, k))
                               
                               );

idx_Worlds <- 1; for(i in 2011:2020) 
{
  idx_Worlds = idx_Worlds + k; tmp <- get_Layout(nodes = nodes, edges = edges, year = i, layout = "lgl")$Graph %>% activate(nodes) %>% data.frame();
  tmp <- tmp[which(tmp$Class == "Marksman"),];
  worlds_LoL_TOP10[(idx_Worlds-k):(idx_Worlds-1), 1:8] <- tmp[order(tmp$Eigen, decreasing = TRUE),][1:k,]; 
}; rm(idx_Worlds, tmp, i);

dtf_TOP10 <- matrix(NA, nrow = 10, ncol = length(unique(as.factor(worlds_LoL_TOP10$Name))) + 1); colnames(dtf_TOP10) <- c("Year", as.character(unique(as.factor(worlds_LoL_TOP10$Name)))); dtf_TOP10[, 1] <- 2011:2020;
idx_Worlds <- 0; for(i in 2011:2020) 
{
  idx_Worlds = idx_Worlds + 1; tmp <- get_Layout(nodes = nodes, edges = edges, year = i, layout = "lgl")$Graph %>% activate(nodes) %>% data.frame();
  for(j in 2:ncol(dtf_TOP10))
  {
    if(length(tmp[which(tmp$Name == colnames(dtf_TOP10)[j]), "Eigen"]) == 0) {next;}
    dtf_TOP10[idx_Worlds, j] = tmp[which(tmp$Name == colnames(dtf_TOP10)[j]), "Eigen"]; 
  };
}; dtf_TOP10[is.na(dtf_TOP10)] <- 0; 

worlds_Tidy <- gather(as.data.frame(dtf_TOP10), Object, Value, -Year); worlds_Tidy$Year <- as.integer(worlds_Tidy$Year);

g_animated <- ggplot(worlds_Tidy, aes(Year, Value, group = Object, color = Object)) + 
  
  geom_line(size = 1) + geom_point(size = 2) + 
  geom_label_repel(aes(x = Year, y = Value, label = Object, fill = Object), hjust = 0, direction = "y", nudge_x = 20,
                   fontface      = 'bold',
                   color         = 'white', 
                   size          = 6,
                   segment.color = 'grey50',
                   segment.size  = 0.5) +
  
  transition_reveal(Year) + 
  theme(legend.position = "none");
animate(g_animated, height = 1920, width = 1080)
anim_save(file = "./SMARTDRAFT/report/figures/animated_top10.gif");
