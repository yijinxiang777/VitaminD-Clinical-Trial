# VitaminD-Clinical-Trial
The study aimed to figure out the optimal daily dose of vitamin D to achieve or maintain vitamin D sufficiency.   

We conducted a phase III, double-blind, randomized trial of two doses of vitamin D3 in children ≥9 years of age with CKD stages 3-5 or kidney transplant recipients. Patients were randomized to 1000 IU or 4000 IU of daily vitamin D3 orally. We measured 25-hydroxvitamin D (25(OH)D) levels at baseline, 3 months and 6 months. The primary efficacy outcome was the percentage of patients who were vitamin D replete (25(OH)D ≥ 30 ng/mL) at 6 months.   

This is the largest cohort of children with choric kidney disease (CKD) undergoing a randomized controlled trial using two different daily doses of vitamin D therapy.

There are three R script for this study:  

**Analysis00** - load the csv file, clean the data, reformat the dataset (wide formart to long format) and prepare variables for longitudinal anlysis  

**Analysis01** - generate discriptive statistictis, paired t-test, ANCOVA, Cochran-Mantel-Haenszel, Linear Mixed Models, Generalized Linear mixed Models  

**Analysis_Plot** - Generate plots with model-based mean and 95% confidence interval over time and by treatment  

