# energy-droughts

Code to reproduce the analysis and figures in the paper: 

Cameron Bracken, Nathalie Voisin, Casey D. Burleyson, Allison M. Campbell, Z. Jason Hou, Daniel Broman, Standardized benchmark of historical compound wind and solar energy droughts across the Continental United States, Renewable Energy, Volume 220, 2024, 119550, ISSN 0960-1481, https://doi.org/10.1016/j.renene.2023.119550.

https://www.sciencedirect.com/science/article/pii/S0960148123014659

1. Download the [generation data](https://zenodo.org/records/7901615) and edit the `gen_data_path` in the `1-prep-data.R` script.
2. Download the [load data](https://zenodo.org/records/13948477/files/tell-historic-load.zip?download=1) and edit the `load_data_path` in the `1-prep-data.R` script.
3. Run `1-plots.R` to produce ba average generation and load data. 
3. Run `2-energy-droughts.R` to compute energy droughts. 
4. Run `3-postprocess-energy-droughts.R` to produce numerous plots, including some used in the paper. 
5. Run `4-plots.R` to produce some extra plots, some of which are used in the paper.