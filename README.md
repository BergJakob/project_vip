Celebrities and Political Communication on Instagram
This repository contains the code and analysis scripts for the paper:
“German Celebrities and Political Communication on Social Media – Opinion Leadership and Endorsements”
Jakob Berg (University of Regensburg)

Project Overview
This project analyzes political communication by celebrities on Instagram in Germany and the United States, as well as the performance effects of celebrity support in the political communication of German politicians during the 2025 federal election campaign.
The study examines:

The frequency of political communication by celebrities (Germany vs. United States)
Performance differences between modes of political presentation (issue‑based, personal, party‑based)
Differences between political and non‑political celebrity posts
The effect of visible celebrity support (VIP endorsements) on the engagement of politicians’ Instagram posts


Data
The analysis is based on two Instagram datasets:


Celebrities (Germany & United States)

~330,000 posts
131 German and 69 U.S. celebrity accounts



Politicians (German Federal Election 2025)

~149,000 posts
Candidates and party accounts related to the 2025 Bundestag election



The datasets are documented separately and available via DOI (see paper).

Methodological Approach

Performance measure: Engagement rate (likes + comments / number of followers)
Log transformation of the engagement rate to address skewed distributions
Machine‑learning‑based classification of political content and celebrity support

GPT‑based classification
5‑fold cross‑validation against human-coded ground truth
Manual correction of false‑positive classifications


Analytical methods:

Descriptive analyses
Exploratory Welch t‑tests
OLS regression models with Newey–West robust standard errors (H2)
Linear mixed‑effects models with random intercepts at the account level (H3, H4)
Sensitivity analyses addressing classification uncertainty


Reproducibility

The full analysis workflow is documented in scripted form.
Due to platform restrictions, raw Instagram data cannot be shared directly.
Aggregated data, analysis scripts, and model outputs allow reproducibility of all results.


Citation
If you use this repository or parts of the code, please cite:

Berg, J. (2026). German Celebrities and Political Communication on Social Media – Opinion Leadership and Endorsements. University of Regensburg.


Contact
Jakob Berg, M.A.
University of Regensburg
📧 jakob.berg@politik.uni-regensburg.de
