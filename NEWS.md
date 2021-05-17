---
title: domir NEWS
---

# domir 0.1.0

- bug fix to `Complete_Dominance` matrix computation
  - Too many models considered for complete dominance - inconsistent with standard dominance analysis methodology
- update to format of `Complete_Dominance` from integers to logicals
- extensive re-write of computation methods
  - pre-allocates container objects to improve performance
  - increased functional-ization of subroutines and overall code readability

# domir 0.0.1

- bug fix for `Ensemble_Coordinator` that submitted factor #'s instead of text in R versions < 4

# domir 0.0.0

- initial working version of `domin`
- initial working version of `print.domin`
