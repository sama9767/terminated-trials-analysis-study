# terminated-trials-analysis-study [IN DEVELOPMENT]
Data processing and analysis: Research project on analyzing terminated trials using two datasets from intovlaue (German UMCs) and contrast (Californian UMCs). See protocol https://osf.io/n4ujs/.

- Preprint: TBD
- Publication: TBD

## Inclusion Criteria:
Trial status reported as 'Terminated' in the registry and enrollment > 0.

## Step 1: Data Download and Processing
The study begins by downloading historical records for ClinicalTrials.gov registered terminated trials using the ['cthist'](https://github.com/bgcarlisle/cthist) R package, developed by BG Carlisle. Additionally, we use ['terminatedtrialsstudy'](https://github.com/sama9767/terminated-trials-study/tree/main) R package to retrieve characteristic data for these terminated trials. We manually categorized the reasons for trial termination into one of four categories: scientific, non-scientific, other, and reason not provided, based on a categorization table developed during the project.

### Variables Generated:
| variable               | description                                                                                                    | level         |
|------------------------|----------------------------------------------------------------------------------------------------------------|---------------|
| `nctid`                | Unique identifier for the clinical trial (Trial ID)                                                            | Character     |
| `reason_for_termination`| The reason for the trial's termination, as entered by the data submitter                                       | Character          |
| `reason_category`      | Categorized reason for trial termination (`scientific`, `non_scientific`, `other`, or `reason not provided`)   | Categorical   |
| `source`               | Identifies the source dataset for the trial ID (e.g., `intovalue`, `contrast`)                                 | Character     |
| `has_summary_result`   | Boolean indicating whether summary results are available on ClinicalTrials.gov for the trial                   | Boolean       |
| `has_summary_result_updated`| Boolean indicating whether summary results are available on ClinicalTrials.gov or EUCTR for the trial |Boolean|
| `start_date`           | The date the trial started (format: `YYYY-MM-DD`)                                                              | Date          |
| `stop_date`            | The date when the trial's status was first updated to "Terminated" in the registry (format: `YYYY-MM-DD`)      | Date          |
| `trial_days`           | Number of days the trial was ongoing until termination, calculated as `stop_date - start_date`                 | Integer       |
| `anticipated_enrollment`| The expected number of participants the trial aimed to enrol                                                  | Integer       |
| `actual_enrollment`    | The actual number of participants enrolled in the trial                                                        | Integer       |
| `enrollment_percentage`| The percentage of enrollment achieved at the time of termination, calculated as `(actual_enrollment / anticipated_enrollment) * 100` | Percentage |
|`trial_foci_table_list`| List of therapeutic foci of the trial | Character|
| `therapeutic_focus`    | The final assigned therapeutic focus of the trial (e.g., oncology, cardiology, etc.)                                          | Character     |

## Related scripts
- 01-get-characterisrics_terminated_trials.R: Generates descriptive statistics for terminated trials.
- 02-categorize_terminated_trials.R: Categorizes trials based on the reason for termination.


## Step 2: Patient Harm Analysis
In the second phase, we estimated patient harm in terminated trials by comparing the risk of serious adverse events (SAEs) between intervention and control groups. To achieve this, we focus on trials reporting adverse events on ClinicalTrials.gov, with at least two arms where comparisons between groups are feasible, for details [see](https://charitede.sharepoint.com/:w:/r/sites/ClinicalResearchAGStrech-IntoValueTerminatedTrialsStudy/Shared%20Documents/The%20Terminated%20Trials%20Study/03_SAE-data-and-analysis/Terminated-Trial-Risk-Protocol.docx?d=w5a70c8271d6c4ed4930bc0cd43cce2cd&csf=1&web=1&e=ogrA7Z).

For Intovalue trials that are cross-registered in EUCTR but lack summary results on ClinicalTrials.gov, we cross-checked the EUCTR registry to obtain comparable SAE data.

Related Scripts:
- 03-prepare_patient_harm_data.R: Data preparation for harm analysis.
- 04-analyse_patient_harm_data.R: Conducts analysis to estimate patient harm.

