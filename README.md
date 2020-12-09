
## Interpreting VE against infection

My goal is to detail one of the challenges in interpreting vaccine
efficacy (VE) against the SARS-CoV2 infection. Recall that most
(possibly all) ongoing large-scale trials have specified symptomatic
disease (COVID-19) as the primary endpoint. However, many will also
report VE against infection with SARS-CoV2. Depending on the trial
design, these endpoints may be ascertained in different ways. COVID-19
endpoints are often recorded after a symptomatic individual contacts a
study site to report symptoms. At the study site, the participant
receives an RT PCR test. If positive, they are adjudicated as a case. On
the other hand, asymptomatic cases are ascertained through active
surveillance at scheduled clinic visits. After receipt of the second
dose, participants attend several scheduled clinic visits where they are
tested for past SARS-CoV2 infection using a serology test. If positive,
the participant is adjudicated a SARS-CoV2 endpoint. It will be common
in the upcoming trial results to see VE reported against any infection
(ascertained either through RT PCR during passive follow up *or* via
serology during active follow up).

The challenge in interpreting these reported VEs is that **imperfect
serology tests will tend to bias VE towards the null**. This phenomenon
can be understood through classic statistics of screening for rare
diseases. The result is counterintuitive, but well known – even if your
assay has high sensitivity (probability of positive test given you were
previously infected) and high specificity (probability of having a
negative test given you were *not* previously infected), if the
prevalence (rate of SARS-CoV2 infection) is low then you will end up
with low positive predictive value (probability you were previously
infected given positive test).

This phenomenon is illustrated in the function below. Here, one inputs a
level of sensitivity, specificity, prevalence of infection and true VE.
In return, one receives the VE against infection that one would compute
based on trial data.

``` r
ve_inf <- function(
  sensitivity = 0.99,
  specificity = 0.995,
  prevalence = 0.02,
  true_ve = 0.75
){
  placebo_true_positive <- prevalence
  placebo_true_negative <- 1 - placebo_true_positive
  placebo_test_positive_true_positive <- placebo_true_positive * sensitivity
  placebo_test_positive_true_negative <- placebo_true_negative * (1 - specificity)

  vax_true_positive <- (1 - true_ve) * prevalence
  vax_true_negative <- 1 - vax_true_positive
  vax_test_positive_true_positive <- vax_true_positive * sensitivity
  vax_test_positive_true_negative <- vax_true_negative * (1 - specificity)

  observed_incidence_placebo <- placebo_test_positive_true_negative + placebo_test_positive_true_positive
  observed_incidence_vax <- vax_test_positive_true_negative + vax_test_positive_true_positive
  
  observed_ve <- 100 * (1 - observed_incidence_vax / observed_incidence_placebo)
  return(observed_ve)
}
```

Using these results, we can compute the trial VE for different settings.
Here are results if ascertainment of cases is 99% sensitive and specific
with prevalence of 0.5% and 1%.

| Sensitivity | Specificity | Prevalence | True VE | Trial VE |
| ----------: | ----------: | ---------: | ------: | -------: |
|        0.99 |        0.99 |      0.005 |      50 |     16.4 |
|        0.99 |        0.99 |      0.010 |      50 |     24.7 |
|        0.99 |        0.99 |      0.005 |      75 |     24.7 |
|        0.99 |        0.99 |      0.010 |      75 |     37.1 |
|        0.99 |        0.99 |      0.005 |      95 |     31.2 |
|        0.99 |        0.99 |      0.010 |      95 |     47.0 |

We see that VE observed in a trial is biased dramatically downwards.
Here are results if ascertainment of cases is 99.9% sensitive and
specific for two levels of prevalence.

| Sensitivity | Specificity | Prevalence | True VE | Trial VE |
| ----------: | ----------: | ---------: | ------: | -------: |
|       0.999 |       0.999 |      0.005 |      50 |     41.7 |
|       0.999 |       0.999 |      0.010 |      50 |     45.4 |
|       0.999 |       0.999 |      0.005 |      75 |     62.5 |
|       0.999 |       0.999 |      0.010 |      75 |     68.2 |
|       0.999 |       0.999 |      0.005 |      95 |     79.1 |
|       0.999 |       0.999 |      0.010 |      95 |     86.3 |

The bias is less extreme, but still present.
