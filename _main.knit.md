--- 
title: "Cascade Rhône River catchment"
author: "Fabio Schneider"
date: "2025-07-31"
site: bookdown::bookdown_site
documentclass: book
bibliography: [references/references.bib, packages.bib]
link-citations: yes
github-repo: rstudio/bookdown-demo

---

# About {-}












<!--chapter:end:index.Rmd-->

# Introduction

<!--chapter:end:01-intro.Rmd-->

# Methods






## Study area

Located in the South-West of Switzerland and South-East of France the Rhône River flows trough 800 km, where ~525 km in French territory. The Rhône River catchment is the most important source of hydropower in France. It covers an area of approximately 97800 km2, of which 87400 km2 (about 90%) lies within French territory (Figure \@ref(fig:local)). In this study, we simulated the bedload connectivity along the main Rhône River within French territory.

\begin{figure}
\includegraphics[width=34.44in]{img/local_mapa} \caption{Location of the Rhône River catchment with the main dams along the river.}(\#fig:local)
\end{figure}

Grain size distribution (GSD) data and bedload measurements in rivers are often scarce or absent, making bedload modeling challenging. For this reason, we chose to model the bedload connectivity along the Rhône River rather than in its tributaries, where data availability even more limited.


## Cascade Model Data Input

The Cascade model is an expert-based model that uses geomorphological river characteristics to simulate bedload transport capacity.

To characterize the geomorfological properties of the river, the dataset must include information for each river reach.

### Number of Reaches

To prepare the input data for the Cascade model, the river reaches were modified to incorporate the reservoirs associated with each dam. The previous dataset contained 195 reaches, after the adjustments, we updated dataset includes a total of 67 reaches.

We adapted the number of reaches by summarizing those with similar geomorphological characteristics by integrating all reaches contained within each dam reservoir. Figure \@ref(fig:reaches) shows the integration of the reaches within the Génissiat Reservoir.


\begin{figure}
\includegraphics[width=43.33in]{img/reach} \caption{Integration of reaches affected by dam reservoirs}(\#fig:reaches)
\end{figure}



### Elevation and Slope data smoothed

To remove the effect of dams on elevation and slope along the Rhône River, we smoothed the elevation profile to approximate as closely as possible the natural river slope, without influence of dam reservoirs. The resulting smoothed elevation profile is show in Figure \@ref(fig:elev). Figure \@ref(fig:slope) shows the slope for each river reach based on the smoothed elevation profile. The slope in the upper part of the Rhône River is steeper, while downstream of the Génissiat Dam, the slope becomes more gradual and remains relatively uniform until the catchment outlet.


![(\#fig:elev)Comparision between the original and smoothed elevation profile of the Rhône River.](_main_files/figure-latex/elev-1.pdf) 


![(\#fig:slope)Longitudinal slope profile of the Rhône River derived from the smoothed elevation data.](_main_files/figure-latex/slope-1.pdf) 


### Channel width and Manning's coefficient


Figure \@ref(fig:wac) shows the channel width, and Figure \@ref(fig:n) shows the Manning’s coefficient for each river reach along the Rhône River. Figure \@ref(fig:basemaps) shows the longitudinal variation of Manning’s coefficient, channel width, and slope along the river. Channel width tends to increase at confluences with tributaries. The Manning’s coefficient was defined as relative uniform along the Rhône River.

![(\#fig:wac)Channel width of each river reach along the Rhône River.](_main_files/figure-latex/wac-1.pdf) 




![(\#fig:n)Manning’s coefficient of each river reach along the Rhône River.](_main_files/figure-latex/n-1.pdf) 


\begin{figure}
\includegraphics[width=34.44in]{img/base_maps} \caption{Longitudinal Manning’s coefficient, slope, and channel width along the Rhône River.}(\#fig:basemaps)
\end{figure}


### GSD along the Rhône River







The grain size distribution (GSD) was obtained from the data available in the report by Parrot et al. [-@parrot2014], Annexe 17: Données statistiques (D16, D50, D84, D90) des prélèvements réalisés par dragage avec la CNR dans le chenal du Rhône (campagnes 2012-13). The Figure \@ref(fig:gsd) shows the data measured (black bars) and the linearly estimated values (brown bars) for each river reach along the Rhône River.

For the reaches with more than one GSD value, the highest value observed for each GSD class was used. For the reaches without GSD data, values were linearly interpolated from neighboring reaches with available data.

![(\#fig:gsd)Grain Size Distribution (GSD) measured and estimated of the bed material along the Rhône River for each river reach.](_main_files/figure-latex/gsd-1.pdf) 





## Discharge








The discharge was obtained by the J2000 model. For each river reach of Cascade input model we selected the closer river reach of the J2000 model. Therefore, the discharge for along the Rhône River is swhowed in the Figure \@ref(fig:q).




\begin{figure}
\includegraphics[width=26.67in]{img/discharge} \caption{Discharge of each river reach along the Rhône River (2000-2019).}(\#fig:q)
\end{figure}




## Trap efficiency [@GILL1979]

Trap efficiency was estimated by the ratio of reservoir capacity (*C*) to catchment area (*A*). This relationship was proposed by Brune [-@Brune53], who presented the following Equation \@ref(eq:E):

\begin{equation}
  E = 1 - \frac{1}{1 + k \cdot \frac{C}{A}}
  (\#eq:E)
\end{equation}

Where:

- *C* is the reservoir capacity (acre-feet)  
- *A* is the catchment area (mi²)  
- *E* is the trap efficiency  
- *k* is a coefficient that varies according to sediment grain size  

The recommended values of *k* are:
- 1.0 for coarse sediments  
- 0.1 for medium sediments  
- 0.046 for fine sediments  

Equation \@ref(eq:Em) presents the same relationship using metric units:

\begin{equation}
  E = 1 - \frac{1}{1 + 0.0021 \cdot k \cdot \frac{C}{A}}
  (\#eq:Em)
\end{equation}

The *C/A* ratio provides a simple method for estimating trap efficiency. However, it is important to recognize that sediment trapping is a much more complex process than what is represented by the equations described above. Despite this simplification, the method remains useful, particularly for dams with limited available data.

Therefore, in the Rhône catchment, Equation \@ref(eq:Em) was applied to estimate the trap efficiency for the dams located along the main river.

Table \@ref(tab:trapE) presents the trap efficiency (*E*) values for fine, medium, and coarse sediments at each dam on the Rhône River.

\begin{table}

\caption{(\#tab:trapE)Trap efficiency (E) for fine, medium, and coarse sediments at each dam located on the Rhône River.}
\centering
\begin{tabular}[t]{c|c|c|c|c|c}
\hline
Dam & C (m<sup>3</sup>) & A (km<sup>2</sup>) & Fine<br>(< 0.062 mm) & Medium<br>(0.062–2 mm) & Coarse<br>(> 2 mm)\\
\hline
Verbois &  &  & 0.12 & 0.22 & 0.74\\
\hline
Chancy Pougny & 14,000,000 & 10,294 & 0.12 & 0.22 & 0.74\\
\hline
Génissiat & 72,000,000 & 10,910 & 0.39 & 0.58 & 0.93\\
\hline
Seyssel & 7,600,000 & 11,300 & 0.06 & 0.12 & 0.59\\
\hline
Chautagne &  & 12,700 & 0.21 & 0.37 & 0.85\\
\hline
Belley &  & 12,985 & 0.21 & 0.37 & 0.85\\
\hline
Brégnier-Cordon &  & 14,000 & 0.21 & 0.37 & 0.85\\
\hline
Sault-Brénaz &  & 15,380 & 0.21 & 0.37 & 0.85\\
\hline
Jons & 55,000,000 & 20,000 & 0.21 & 0.37 & 0.85\\
\hline
Pierre Bénite &  & 50,400 & 0.21 & 0.37 & 0.85\\
\hline
Vaugris & 41,000,000 & 51,700 & 0.07 & 0.14 & 0.62\\
\hline
Péage de Roussillon &  & 52,880 & 0.21 & 0.37 & 0.85\\
\hline
Saint Valiier &  & 53,850 & 0.21 & 0.37 & 0.85\\
\hline
Bourg-lès Valences &  & 66,450 & 0.21 & 0.37 & 0.85\\
\hline
Beauchastel &  & 66,700 & 0.21 & 0.37 & 0.85\\
\hline
Baix-le Logis Neuf &  & 69,960 & 0.21 & 0.37 & 0.85\\
\hline
Montélimar &  & 70,900 & 0.21 & 0.37 & 0.85\\
\hline
Donzère-Mondragon & 27,000,000 & 71,000 & 0.04 & 0.07 & 0.44\\
\hline
Caderousse & 123,000 &  & 0.07 & 0.14 & 0.62\\
\hline
Avignon &  &  & 0.04 & 0.07 & 0.44\\
\hline
\end{tabular}
\end{table}

**Where:**  
- `C` is the reservoir capacity (m<sup>3</sup>)  
- `A` is the catchment area (km<sup>2</sup>)  
- Trap efficiency values highlighted in yellow in the original source were estimated by similarity.







<!--chapter:end:02-methods.Rmd-->

# Preliminary Result

Preliminary results were obtained using three different sediment transport equations, each simulated under three distinct trap efficiency scenarios. The equations used to estimate sediment transport were developed by Wilcock and Crowe [-@WandC2003], Eugelund and Hansen [-@EandH67], and, Ackers and White [-@AandW73].

For each equation, three trap efficiency scenarios were considered: (i) no dams influence, where the model assumes no dams present along the river; (ii) 100% trap efficiency, where all the sediment reaching a dammed reach is fully retained; and (iii) variable trap efficiencies as presented in Table 1. Simulations were caried out for the period from 2000 to 2019, with annual results available in supplementary material.

Three grain size classes were used based on Krumbein's scale: sand (0.06-2 mm), gravel (2-256 mm), and boulder (>256 mm).

The following figures (from Figure \@ref(fig:TrcE0eW) to Figure \@ref(fig:budgetE2eA)) present the cumulative bedload transported, deposited and sediment budget over the 20-year period for each equation and trap efficiency scenario.


## No dams

### Wilkock and Crowe equation


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eW&C_noDams/plots_tr_cap-silt/tr_cap_res_sum_hy_E0_eW&C_noDams} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation without accounting dams trap efficiency.}(\#fig:TrcE0eW)
\end{figure}


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eW&C_noDams/plots_transp-silt/transp_res_sum_hy_E0_eW&C_noDams} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation without accounting dams trap efficiency.}(\#fig:TrE0eW)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eW&C_noDams/plots_dep-silt/dep_res_sum_hy_E0_eW&C_noDams} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation without accounting dams trap efficiency.}(\#fig:DepE0eW)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eW&C_noDams/plots_sed_budget-silt/sed_bud_res_sum_hy_E0_eW&C_noDams} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation without accounting dams trap efficiency.}(\#fig:budgetE0eW)
\end{figure}


### Eugelund and Hansen equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eE&H_noDams/plots_tr_cap-silt/tr_cap_res_sum_hy_E0_eE&H_noDams} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation without accounting dams trap efficiency.}(\#fig:TrcE0eE)
\end{figure}


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eE&H_noDams/plots_transp-silt/transp_res_sum_hy_E0_eE&H_noDams} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation without accounting dams trap efficiency.}(\#fig:TrE0eE)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eE&H_noDams/plots_dep-silt/dep_res_sum_hy_E0_eE&H_noDams} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation without accounting dams trap efficiency.}(\#fig:DepE0eE)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eE&H_noDams/plots_sed_budget-silt/sed_bud_res_sum_hy_E0_eE&H_noDams} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation without accounting dams trap efficiency.}(\#fig:budgetE0eE)
\end{figure}



### Ackers and White equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eA&W_noDams/plots_tr_cap-silt/tr_cap_res_sum_hy_E0_eA&W_noDams} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:TrcE0eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eA&W_noDams/plots_transp-silt/transp_res_sum_hy_E0_eA&W_noDams} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:TrE0eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eA&W_noDams/plots_dep-silt/dep_res_sum_hy_E0_eA&W_noDams} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:DepE0eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E0_eA&W_noDams/plots_sed_budget-silt/sed_bud_res_sum_hy_E0_eA&W_noDams} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:budgetE0eA)
\end{figure}



## 100% Trap efficiency

### Wilkock and Crowe equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eW&C/plots_tr_cap-silt/tr_cap_res_sum_hy_E3_eW&C} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of 100\%.}(\#fig:TrcE3eW)
\end{figure}


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eW&C/plots_transp-silt/transp_res_sum_hy_E3_eW&C} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of 100\%.}(\#fig:TrE3eW)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eW&C/plots_dep-silt/dep_res_sum_hy_E3_eW&C} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of 100\%.}(\#fig:DepE3eW)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eW&C/plots_sed_budget-silt/sed_bud_res_sum_hy_E3_eW&C} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of 100\%.}(\#fig:budgetE3eW)
\end{figure}


### Eugelund and Hansen equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eE&H/plots_tr_cap-silt/tr_cap_res_sum_hy_E3_eE&H} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of 100\%.}(\#fig:TrcE3eE)
\end{figure}


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eE&H/plots_transp-silt/transp_res_sum_hy_E3_eE&H} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of 100\%.}(\#fig:TrE3eE)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eE&H/plots_dep-silt/dep_res_sum_hy_E3_eE&H} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of 100\%.}(\#fig:DepE3eE)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eE&H/plots_sed_budget-silt/sed_bud_res_sum_hy_E3_eE&H} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of 100\%.}(\#fig:budgetE3eE)
\end{figure}



### Ackers and White equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eA&W/plots_tr_cap-silt/tr_cap_res_sum_hy_E3_eA&W} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation accounting dams trap efficiency of 100\%.}(\#fig:TrcE3eA)
\end{figure}


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eA&W/plots_transp-silt/transp_res_sum_hy_E3_eA&W} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation accounting dams trap efficiency of 100\%.}(\#fig:TrE3eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eA&W/plots_dep-silt/dep_res_sum_hy_E3_eA&W} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation accounting dams trap efficiency of 100\%.}(\#fig:DepE3eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E3_eA&W/plots_sed_budget-silt/sed_bud_res_sum_hy_E3_eA&W} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation accounting dams trap efficiency of 100\%.}(\#fig:budgetE3eA)
\end{figure}



## Trap efficiency (Table 1)

### Wilkock and Crowe equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eW&C/plots_tr_cap-silt/tr_cap_res_sum_hy_E2_eW&C} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of Table 1.}(\#fig:TrcE2eW)
\end{figure}


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eW&C/plots_transp-silt/transp_res_sum_hy_E2_eW&C} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of Table 1.}(\#fig:TrE2eW)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eW&C/plots_dep-silt/dep_res_sum_hy_E2_eW&C} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of Table 1.}(\#fig:DepE2eW)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eW&C/plots_sed_budget-silt/sed_bud_res_sum_hy_E2_eW&C} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Wilkock and Crowe equation accounting dams trap efficiency of Table 1.}(\#fig:budgetE2eW)
\end{figure}


### Eugelund and Hansen equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eE&H/plots_tr_cap-silt/tr_cap_res_sum_hy_E2_eE&H} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of Table 1.}(\#fig:TrcE2eE)
\end{figure}


\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eE&H/plots_transp-silt/transp_res_sum_hy_E2_eE&H} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of Table 1.}(\#fig:TrE2eE)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eE&H/plots_dep-silt/dep_res_sum_hy_E2_eE&H} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of Table 1.}(\#fig:DepE2eE)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eE&H/plots_sed_budget-silt/sed_bud_res_sum_hy_E2_eE&H} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Eugelund and Hansen equation accounting dams trap efficiency of Table 1.}(\#fig:budgetE2eE)
\end{figure}



### Ackers and White equation

\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eA&W/plots_tr_cap-silt/tr_cap_res_sum_hy_E2_eA&W} \caption{Cumulative discharge and bedload transport capacity for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:TrcE2eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eA&W/plots_transp-silt/transp_res_sum_hy_E2_eA&W} \caption{Cumulative discharge and bedload transported for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:TrE2eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eA&W/plots_dep-silt/dep_res_sum_hy_E2_eA&W} \caption{Cumulative discharge and bedload deposited for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:DepE2eA)
\end{figure}



\begin{figure}
\includegraphics[width=26.24in]{img/res_cascade/res_E2_eA&W/plots_sed_budget-silt/sed_bud_res_sum_hy_E2_eA&W} \caption{Cumulative discharge and sediment budget for sand (0.63-2 mm), gravel (2-256 mm), boulders (>256 mm), and the total of all three classes for each river reach along the Rhône River, calculated using the Ackers and White equation without accounting dams trap efficiency.}(\#fig:budgetE2eA)
\end{figure}





## Bedload transport capacity

The results of transport capacity using the three equations along the Rhône River are shown in Figure \@ref(fig:Trcmap). The scale was adjusted to be consistent with the report by Laval [-@Laval20] (Figure \@ref(fig:Laval20)).


\begin{figure}
\includegraphics[width=31.21in]{img/res_cascade_trc} \caption{Bedload transport capacity along the Rhône River calculated using three different equations: Wilcock and Crowe, Engelund and Hansen, and Ackers and White.}(\#fig:Trcmap)
\end{figure}

\begin{figure}
\includegraphics[width=0.6\linewidth]{img/laval_2020} \caption{Bedload transport capacity actual according Laval [-@Laval20].}(\#fig:Laval20)
\end{figure}

Analyzing the bedload transport capacity described in the report by Laval [-@Laval20], the equation that produced the most consistent pattern in the CASCADE model was that of Engelund and Hansen [-@EandH67]. The Ackers and White equation [-@AandW73] also showed a reasonable pattern; however, when comparing the results (Figure \@ref(fig:TrcE2eA)), the transport capacity simulated by the CASCADE model was significantly higher.

Similarly, although the Engelund and Hansen equation [-@EandH67] tended to overestimate transport capacity, its results were more consistent with the reference than those from the other equations.

Figure \@ref(fig:EHxLaval20) shows a comparison between the results from Laval [-@Laval20] and those obtained using the Engelund and Hansen [-@EandH67] equation, presented in quantiles.


\begin{figure}
\includegraphics[width=33.88in]{img/res_cascade_trcxlaval} \caption{Bedload transport capacity actual according Laval [-@Laval20] and Engelund and Hansen [-@EandH67].}(\#fig:EHxLaval20)
\end{figure}




<!--chapter:end:03-results.Rmd-->

# Discussion

<!--chapter:end:04-discussion.Rmd-->

# Conclusion

<!--chapter:end:05-conclusion.Rmd-->



<!--chapter:end:06-references.Rmd-->

