from pathlib import Path
import pandas as pd
import numpy as np
from duet_tools.utils import read_dat_to_array, write_array_to_dat
from matplotlib import pyplot as plt

WRITE_FUELLIST = True
COMBINE_FUELS = False

if WRITE_FUELLIST:
    xtb = list(np.linspace(2, 602, num=round(604 / 4)))
    xl_yb = list(np.linspace(2, 52, num=round(52 / 4)))
    xr_yt = list(np.linspace(556, 602, num=round((604 - 552) / 4)))
    ylr = list(np.linspace(52, 556, num=round((552 - 52) / 4)))

    xu = xtb + xl_yb + xr_yt
    yu = ylr + xl_yb + xr_yt

    num_trees = len(xu) * len(yu)

    x = list(np.repeat(xu, len(yu)))
    y = list(np.repeat(yu, len(xu)))
    id = list(np.repeat(1, num_trees))
    cbh = list(np.repeat(1.0, num_trees))
    ht = list(np.repeat(5.0, num_trees))
    cd = list(np.repeat(3.0, num_trees))
    hcd = list(np.repeat(2.0, num_trees))
    cbd = list(np.repeat(0.5, num_trees))
    cmc = list(np.repeat(1.0, num_trees))
    ss = list(np.repeat(0.0005, num_trees))

    df = pd.DataFrame(
        {
            "id": id,
            "x": x,
            "y": y,
            "ht": ht,
            "cbh": cbh,
            "cd": cd,
            "hcd": hcd,
            "cbd": cbd,
            "cmc": cmc,
            "ss": ss,
        }
    )

    HERE = Path(__file__).parent
    out_path = HERE / "QF_runs" / "Dixie" / "Dixie_Yellow_500m" / "ladder_fuellist.txt"

    df.to_csv(out_path, sep=" ", header=False, index=False)

### Run trees on ladder_fuellist.txt, then copy rhof and moist into path, naming them ladder_*.dat

if COMBINE_FUELS:
    # Combine ladder fuels with og dat files
    run_dir = HERE / "QF_runs" / "Dixie" / "Dixie_Yellow_500m"

    rhof_og = read_dat_to_array(run_dir, "treesrhof.dat", 302, 302, 78, order="C")
    rhof_ladder = read_dat_to_array(run_dir, "ladder_rhof.dat", 302, 302, 78, order="C")
    moist_og = read_dat_to_array(run_dir, "treesmoist.dat", 302, 302, 78, order="C")
    moist_ladder = read_dat_to_array(
        run_dir, "ladder_moist.dat", 302, 302, 78, order="C"
    )

    rhof_out = np.add(rhof_og, rhof_ladder)
    moist_out = np.maximum(moist_og, moist_ladder)

    def plot_array(x: np.ndarray, title: str):
        plt.figure(2)
        plt.set_cmap("viridis")
        plt.imshow(x, origin="lower")
        plt.colorbar()
        plt.title(title, fontsize=18)
        plt.show()

    plot_array(np.sum(rhof_og[1:, :, :], axis=2), "og")
    plot_array(np.sum(rhof_ladder[1:, :, :], axis=2), "ladder")
    plot_array(np.sum(rhof_out[1:, :, :], axis=2), "both")
