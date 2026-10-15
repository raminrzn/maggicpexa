# maggicpexa

The **MAGGIC heart failure risk score** as a ModelsCloud service: an integer
score and the 1- and 3-year mortality for patients with heart failure.

Pocock SJ, et al. *Eur Heart J.* 2013;34(19):1404–1413.
[doi:10.1093/eurheartj/ehs337](https://doi.org/10.1093/eurheartj/ehs337)

## Inputs

| Field | Coding |
|---|---|
| `age` | years |
| `sex` | `"male"` / `"female"` |
| `ef` | ejection fraction, % |
| `nyha` | NYHA class 1–4 |
| `creatinine` | µmol/L (or set `creatinine_units = "mg/dL"`) |
| `bmi` | kg/m² |
| `sbp` | systolic BP, mmHg |
| `smoker`, `diabetes`, `copd` | yes / no (smoker = current smoker) |
| `hf_18_months` | yes / no: heart failure first diagnosed 18 or more months ago |
| `beta_blocker`, `acei_arb` | yes / no: currently prescribed |

Returns `score` and `risk_1y` and `risk_3y` (0–1) from the paper's Table 4.
Above 50 points the table ends, so the risks are `NA` and `note` says so.

## Notes on the chart

- Age 55 falls between the chart's printed bands (`<55`, `56–59`); it scores 0.
- Figure 2's wording "first diagnosis in the past 18 months" refers to the
  paper's "HF duration > 18 months" risk factor, so +2 applies to longer-standing
  heart failure.
- Creatinine of exactly 90 µmol/L scores 1, as in the chart's 90–109 band.
  MDCalc gives it 0.

## Validation

Matches MDCalc's MAGGIC calculator on 8 patients (scores 0–57), except the
creatinine = 90 case above.

## Use

```r
remotes::install_git("https://github.com/raminrzn/maggicpexa", ref = "main")
maggicpexa::model_run(maggicpexa::get_default_input())
```

For research use. Not a medical device.

## License

GPL-3.
