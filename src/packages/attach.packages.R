requiredPackages <- scan("~/SMARTDRAFT/src/packages/list.txt", character(), quote = ""); require(tibble);

#' @description If installed, updates the packages used within the project.
tmp <- as_tibble(old.packages()[which(old.packages()[, 1] %in% requiredPackages),]); av4update <- c(); idx <- 1;

if(nrow(tmp) != 0)
{
  ifelse({ncol(tmp) == 1}, # CONDITION: a single package is found. Tibble acts as vector. 
         
         { # if TRUE: consider tmp as vector; 
           print(paste0("Package ", as.character(tmp[1, 1]), " (", as.character(tmp[3, 1]), ") is available for update (", as.character(tmp[5, 1]), ").")); av4update[1] <- 1;
           answer = readline(prompt = "Do you want to update (Y/n)? ");
           if(answer == "Y") {for(i in av4update) {if(as.character(tmp[1, 1]) %in% (.packages())) {detach(paste0("package:", as.character(tmp[1, 1])), unload = TRUE);}; install.packages(as.character(tmp[1, 1]), dependencies = TRUE);};}; rm(i, answer, av4update, tmp);
         },  
         
         { # if FALSE: consider tmp as tibble;
           for(i in 1:nrow(tmp)) {if(tmp[i, 3] != tmp[i, 5]) {print(paste0("Package ", tmp[i, 1], " (", tmp[i, 3], ") is available for update (", tmp[i, 5], ").")); av4update[idx] = i; idx = idx + 1;};}; rm(i, idx);
           answer <- readline(prompt = "Do you want to update (Y/n)? ");
           if(answer == "Y") {for(i in av4update) {if(tmp[i, 1] %in% (.packages())) {detach(paste0("package:", tmp[i, 1]), unload = TRUE);}; install.packages(as.character(tmp[i, 1]), dependencies = TRUE);};}; rm(i, answer, av4update, tmp);
         }
        );  
}; rm(tmp, av4update, idx);

#' @description After the update, loads, if present, the packages used within the project.
for(i in 1:length(requiredPackages)) 
{if(requiredPackages[i] %in% installed.packages()) {require(requiredPackages[i], character.only = TRUE);};}; print("Matching packages loaded.");
for(i in 1:length(requiredPackages)) 
{if(!requiredPackages[i] %in% installed.packages()) {print(paste0("Warning! Missing package: ", requiredPackages[i]));};}; rm(i, requiredPackages);


