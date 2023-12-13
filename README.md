# R-demo

### Manual
This repo shows some work I did in R during my phd. See the abstract and link to the publication below to understand the study aim. 
The following scripts are included:

*GutBrain_Analysis.R*

Here I performed different statistical analyses, including mixed effects models, multiple mediation, correlation with correction for multiple testing, multiple regression, etc. The original data is included (allData[...].csv) in the repo, so the tests and graphics are reproducable. You can also find some graphics within the original publication (link below). *Funktionen1.R and config.R* are outsourced dependencies that work on other scripts as well.

*Ospan_ext.R*

This script extracts, computes and analyses data from external experiment protocols. The Operation Span Task (Ospan) is a neurocognitive experiment used to quantify working memory capacity (WMC) by testing which letters of a letter sequence can be remembered while solving simple math problems at the same time. We proposed a new WMC scoring method, which needed to be validated by comparing it to a number of datasets that was given to us by other institutes (see https://doi.org/10.31234/osf.io/ue3j8 for more info an acknowledgements). This script first mines certain data points, then calculates different scoring methods and finally correlates those methods. Due to data privacy commitment the original data is not included. 


### Publication

https://doi.org/10.1016/j.physbeh.2023.114279

Abstract:
Background: Bariatric surgery has been widely recognized as the most efficient long-term treatment method in severe obesity, yet therapy success shows considerable interindividual variability. Postoperative metabolic adaptations, including improved gut hormone secretion (GLP-1, PYY and ghrelin), and restored executive function may play an explanatory role in weight loss, yet causes for poor success in individual patients remain unknown. This study investigates gut-hormonal and cognitive characteristics in extreme weight loss responders to bariatric surgery. Methods: Patients (n = 47) with high or low excessive weight loss (EWL) at least 2 years after Roux-en-Y-gastric bypass or sleeve gastrectomy were allocated into good responders (GR, EWL 82.4 ± 11.6%) and poor responders (PR, EWL 24.0 ± SD 12.8%) to study differences in postprandial secretion of GLP-1, PYY, ghrelin and in working memory (WM). Results: Mean BMI was 47.1 ± 6.2 kg/m² in PR (n = 21) and 28.9 ± 3.1 kg/m² in GR (n = 26, p < 0.001). Fasted GLP-1 and PYY were comparable for GR and PR (p > 0.2) and increased strongly after a standardized test meal (300 kcal liquid meal) with a peak at 15 to 30 min. The increase was stronger in GR compared to PR (GLP-1, PYY: Time x Group p < 0.05). Plasma ghrelin levels already differed between groups at fasted state, showing significantly higher levels for GR (p < 0.05). Postprandially, ghrelin secretion was suppressed in both groups, but suppression was higher in GR (Time x Group p < 0.05). GR showed significantly higher WM scores than PR (p < 0.05). Postprandial ghrelin (iAUC), but not GLP-1 or PYY plasma levels, significantly mediated the relationship between EWL and a WM subscore (IS score, CI = 0.07 - 1.68), but not WM main score (MIS score, CI = -0.07 - 1.54), in mediation analyses. Conclusion: Excess weight loss success after bariatric surgical procedures is associated with distinct profiles of gut-hormones at fasted and postprandial state, and differences in working memory. Better working memory performance in GR might be mediated by higher postprandial reduction in ghrelin plasma levels. Future studies need to integrate longitudinal data, larger samples and more sensitive cognitive tests.


