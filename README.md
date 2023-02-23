# Data and analysis codes for "..." (Kuroda, Takahashi, Kameda)

## Directories

- `/analysis`
  - The main folder where the analysis codes are saved. If you would like to reproduce our results, please execute the Rmd files from `00_merge.Rmd` to `05_figures,Rmd`. 
  - You can quickly take a look at our results by reading the HTML files, which were generated from the Rmd files.
- `/data`
  - Raw data is saved here. Each folder's name indicates each task in our experiment.
  - `/analysis/00_merge.Rmd` generated the preprocessed data files in `/data/preprocessed`, so you don't have to touch the raw data.
- `/function`
  - Functions in each language are saved here.
- `/output`
  - We saved the output here, which was generated in the analysis.
  - Figures in the manuscript are also saved here.
- `/css` and `/html`
  - You don't have to touch these folders.

## Coding table for the experiment data

For the data files, please see the folder `/data/preprocessed`.

### questionnaire.csv

This file contains the data from the post-session questionnaire and participants' demographic data.

| Variable | Data type | Description | Values |
| --- | --- | --- | --- |
| id | dbl | Participant's ID | 1 to 63 |
| difficulty | dbl | Difficulty of the task | 1 to 7 |
| easy_other | dbl | How much participants thought that previous 24 participants had answered correctly when the task was easy | 1 to 7 |
| hard_other | dbl | How much participants thought that previous 24 participants had answered correctly when the task was difficult | 1 to 7 |
| influence | dbl | How much participants thought that their own vote affected the majority’s accuracy | 1 to 7 |
| variety | dbl | How much participants thought that the majority’s accuracy depended on who voters were | 1 to 7 |
| worsen | dbl | How much participants thought that their own vote worsened the majority’s accuracy | 1 to 7 |
| improve | dbl | How much participants thought that their own vote improved the majority’s accuracy | 1 to 7 |
| age | dbl | Participant's age |  |
| gender | chr | Participant's gender | "man" or "woman" |
| vision | chr | Participant's vision | "unaided", "glasses", or "contact" |
| hand | chr | Participant's dominant hand | Only "right" |
| svo_score | dbl | Score of social value orientation (svo) |  |
| svo | chr | SVO | "prosocial" or "individualist"  |


### gamble.csv

This file contains the data from the gambling task.

| Variable | Data type | Description | Values |
| --- | --- | --- | --- |
| task | chr | Task | Only "gamble" |
| id | dbl | Participant's ID | 1 to 63 |
| trial | dbl | Trial number | 1 to 47 |
| payoff_sure | dbl | Payoff of the sure option | Only 500 (JPY) |
| payoff_risky | dbl | Payoff of the risky option | Amount in JPY |
| prob | dbl | Reward probability of the risky option | 0.3, 0.4, 0.5, or 1.0|
| choice | chr | Participant's choice | "sure" or "risky" |
| rt | dbl | Response time (sec.) |  |
| pos | chr | Left-right positions of the sure and risky options. The first letter indicates the left-sided option; the second indicates the right. | "sr" or "rs" |


### solo.csv

This file contains the data from the solo block.

| Variable | Data type | Description | Values |
| --- | --- | --- | --- |
| task | chr | Task | Only "solo" |
| id | dbl | Participant's ID | 1 to 63 |
| trial | dbl | Trial number | 1 to 47 |
| ori_mean | dbl | mean orientation | 3 or -3 |
| ori_var | dbl | variance of orientations | 8, 16, 32, or 64 |
| judge | chr | The participant's judgment | "clockwise" or "anticlockwise" |
| answer | chr | The correct answer in the trial | "clockwise" or "anticlockwise" |
| result | chr | Whether the judgment was correct or wrong | "correct" or "wrong" |
| judge_rt | dbl | Response time for the judgment (sec.) |  |
| line_pos | chr | Left-right positions of the clockwise and anticlockwise options. The first letter indicates the left-sided option; the second indicates the right. | "ca" or "ac" |
| payoff_sure | dbl | Payoff for the sure option | Only 500 (JPY) |
| payoff_risky | dbl | Payoff for the risky option | Amount in JPY |
| payoff_jitter | dbl | The additional amount for the risky option | Amount in JPY |
| choice | chr | The option the participant chose in the trial | "sure" or "risky" |
| choice_rt | dbl | Response time for the choice (sec.) |  |
| option_pos | chr | Left-right positions of the risky and sure options. The first letter indicates the left-sided option; the second indicates the right. | "sr" or "rs" |
| rating | dbl | Confidence rating in the trial | 1 to 6 |
| rating_rt | dbl | Response time for confidence rating (sec.) |  |
| rating_init | dbl | Initial position of the cursor in the rating | 1 to 4 |
| rating_history | chr | The history of the cursor's movement in the rating | list as character |
| iti | dbl | ITI (sec.) |  |


### group.csv

This file contains the data from the opt-in/out block. The format is almost the same as `solo.csv`, so here we only list the different/additional columns in this file.

| Variable | Data type | Description | Values |
| --- | --- | --- | --- |
| order | chr | Whether participants decided opt-in/out before or after the orientation-judgment task. In this paper we only report the results from the trials where participants chose opt-in/out after the task. | "pre" or "post". You can ignore the rows "pre". |
| payoff_group | dbl | The payoff for the opt-in option | Only 500 (JPY) |
| choice | chr | Whether the participant chose the opt-in or opt out | "solo" (opt-out) or "group" (opt-in) |


### solo_gabor.csv and group_gabor.csv

This file contains the data on the orientations of the stimuli.

| Variable | Data type | Description | Values |
| --- | --- | --- | --- |
| task | chr | The name of the block | "solo" or "group" |
| id | dbl | The participant's ID | 1 to 63 |
| trial | dbl | Trial number |  |
| order | dbl | The order in which the patch appeared in the trial | 1 to 30 |
| ori | dbl | The orientation of the patch from the vertical line (in degrees) |  |