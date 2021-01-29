<style>
body {
    overflow: scroll;
}
</style>
 
Meta Evolution Analysis: The Case of League of Legends Worlds
========================================================
author: Victor Plesco
date: 01-02-2021
autosize: true
transition-speed: fast
width: 1920
height: 1080

Overview
========================================================

- Introduction
- Data
- Network Analysis I
- Regression Analysis
- Network Analysis II
- Conclusion

(1) What is League of Legends? 
========================================================

<br>

- Free-to-Play 2009 MOBA (Riot Games)
- Team-Based PvP (5 Blue & 5 Red)  
- World's Largest eSport:
  - 13 Leagues (e.g. LEC, LPL, LCS, LCK, ect.)
  - 100M+ Viewers
  - ~$2.5M Prize Pool (Worlds)
  
***

<img src=".//figures/Logo.jpg" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" width="1200px" style="display: block; margin: auto;" />

(2) What is League of Legends? 
========================================================

<br>


<img src=".//figures/League_Draft.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="1000px" style="display: block; margin: auto;" />

***

Champion Selection:
- Pre-determined Sequence (1B, 2R, 2B, 2R, 2B, 1R)
- 5 Roles
- 154 Champions (2020)

(3) What is League of Legends?
========================================================

In-game Development:
- 3 Lanes, 2 Sides (Blue & Red)
- Tower, Inhibitor, Nexus
- Minions & Map Objectives (Dragon & Baron)

***

<img src=".//figures/League_Map_Tutorial.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="1000px" style="display: block; margin: auto;" />

(1) Data
========================================================

<img src=".//figures/LoL_Worlds_18.jpg" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="1000px" style="display: block; margin: auto;" />

*** 

- Complex Data
- Non User-Friendly Riot API
- Dynamic System (2 Weeks Update)

Solution?

(2) Data
========================================================

GAMEPEDIA:
- Largest Video Game Wiki (Leaguepedia)
- Random Fact: Top 500 Websites by Traffic

***

<img src=".//figures/gamepedia_picks.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="1000px" style="display: block; margin: auto;" />

<img src=".//figures/gamepedia_champions.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="1000px" style="display: block; margin: auto;" />

(1) Network Analysis I: Key Metrics
========================================================

<br>  

Weights:  

$w_{i \to j}=\underbrace{\frac{\text{matches with }i\text{ vs }j}{\text{total matches}}}_{(\text{popularity})} \times \underbrace{\frac{\text{number of matches where }i\text{ beat }j}{\text{matches with }i\text{ vs }j}}_{\text{(winrate)}}=\frac{N_{i \to j}}{M}$  

Centrality: 

$\text{EVC}_{i}=x_{i}=k^{-1}\sum_{j}^{n}A_{ij}x_{j}$  

(2) Network Analysis I: Historical PoV per Champion
========================================================

<img src=".//figures/ggraph_1120_ICONS.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="1920px" height="1080px" style="display: block; margin: auto;" />

(3) Network Analysis I: Historical PoV per Class
========================================================

<img src=".//figures/barplot_1120.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="1920px" height="1080px" style="display: block; margin: auto;" />

(1) Regression Analysis: Champion Stats
========================================================

<img src=".//figures/regression_1120.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width="1920px" height="1080px" style="display: block; margin: auto;" />

Conclusion
========================================================

- No Equilibrium Distribution of Strategies
- Real-World Open-Ended System (Biological Systems)
- Organism (Game) and Environment (Developer)

*** 

Real-world open-ended evolution: A league of legends adventure, Alyssa Adams, Sara Walker, International Journal of Design & Nature and Ecodynamics, January 2018.
