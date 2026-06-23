import numpy as np
import pandas as pd
from pathlib import Path

options = ["no_duet", "duet"]
runs = ["Caldor_Camp2_500m", "Caldor_Camp2_500m_duet"]
outputs = [
    "mass_burnt_pct",
    "surface_consumption",
    "canopy_consumption",
    "max_power",
    "residence_time_power",
    "residence_time_consumption",
    "max_reaction_rate",
]
runs_path = Path(__file__).parent.parent / "Arrays" / "Caldor"

all_dat = pd.DataFrame()
for i in range(len(runs)):
    dat = {}
    for output in outputs:
        out_arr = np.fromfile(
            runs_path / runs[i] / "Arrays" / f"{output}.txt", dtype=np.float32
        )
        dat[output] = list(out_arr)
    dat["option"] = options[i]
    dat = pd.DataFrame(dat)
    all_dat = pd.concat([all_dat, dat])

all_dat.to_csv(Path(__file__).parent / "duet_compare_Camp2.csv", index=False)
