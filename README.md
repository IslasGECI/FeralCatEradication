<img src="https://www.islas.org.mx/img/logo.svg" align="right" width="256" />

# population_dynamics

**Stochastic models for predicting feral cat eradication and costs on large islands.**

This repository is a fork of
[FeralCatEradication](https://github.com/KathrynVenning/FeralCatEradication) by [Kathryn
Venning](https://github.com/KathrynVenning) (<kathryn.venning@flinders.edu.au>) at Flinders
University, Adelaide, Australia, in collaboration with/under supervision of Corey Bradshaw and
Frédérik Saltré.

> Kathryn R. W. Venning, Frédérik Saltré, Corey J. A. Bradshaw. 2021. Predicting targets and costs
> for feral-cat reduction on large islands using stochastic population models. Conservation Science
> and Practice. DOI: [10.1111/csp2.448](https://doi.org/10.1111/csp2.448)

---

```shell
docker build . --tag feral_cat
docker run --name feral_cat feral_cat make reports/predicting_targets_and_costs.pdf
docker cp feral_cat:/workdir/reports/predicting_targets_and_costs.pdf reports/
xdg-open reports/predicting_targets_and_costs.pdf
```
