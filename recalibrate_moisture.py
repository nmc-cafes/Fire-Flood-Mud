from pathlib import Path
import duet_tools as duet
from duet_tools.utils import read_dat_to_array, write_array_to_dat
import zarr
from matplotlib import pyplot as plt
import numpy as np
import geopandas as gpd
from shutil import copy
from quicfire_tools import SimulationInputs


def plot_array(x: np.ndarray, title: str):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    plt.show()


def find_nsp(path) -> int:
    species = []
    with open(path / "surface_species.dat", "r") as file:
        for line in file:
            species.append(line.strip())
    return len(species) + 1


grass_val = [0.05, 0.15, 0.25]
litter_max = [0.10, 0.25, 0.40]
litter_min = [0.05, 0.10, 0.15]

run_dict = {
    "dry": [grass_val[0], litter_max[0], litter_min[0]],
    "mod": [grass_val[1], litter_max[1], litter_min[1]],
    "wet": [grass_val[2], litter_max[2], litter_min[2]],
}

qf_exe = Path(__file__).parent.parent.parent / "Quicfire" / "QF_6.0.1" / "exe"

sites = ["Cal5", "Dix5"]
fires = ["Caldor", "Dixie"]
for i in range(len(sites)):
    site_dict = Path(__file__).parent / fires[i] / "Sample_Basins" / sites[i]
    shp = gpd.read_file(site_dict / f"{sites[i]}.shp")
    query = duet.query_landfire(shp.geometry[0], site_dict, 5070)
    fg = zarr.open(site_dict / f"{sites[i]}_fastfuels.zarr", mode="r")
    nx = fg.attrs["nx"]
    ny = fg.attrs["ny"]
    nz = fg.attrs["nz"]
    nsp = find_nsp(site_dict)
    to_copy = [
        "ignite.dat",
        "topo.dat",
        f"{sites[i]}.json",
        "class_def.py",
        "class_viz.py",
        "drawfire.py",
        "drawsmoke.py",
        "fuel_voxels.py",
        "gen_images.py",
        "misc.py",
        "quicfire_vis.py",
        "read_inputs.py",
        "PyVistaQF.py",
    ]
    for cond in run_dict.keys():
        qf_dict = (
            Path(__file__).parent
            / "QF_runs"
            / "Moisture_Sensitivity"
            / f"{sites[i]}_{cond}"
        )
        qf_dict.mkdir(exist_ok=True)

        json_dict = Path(__file__).parent / "QF_runs" / fires[i] / sites[i]

        conditions = run_dict.get(cond)

        duet_run = duet.import_duet(site_dict, nx, ny, nsp)

        grass_moisture = duet.assign_targets(method="constant", value=conditions[0])
        litter_moisture = duet.assign_targets(
            method="maxmin", max=conditions[1], min=conditions[2]
        )
        grass_density = duet.assign_targets_from_sb40(query, "grass", "density")
        litter_density = duet.assign_targets_from_sb40(query, "litter", "density")
        grass_height = duet.assign_targets_from_sb40(query, "grass", "height")
        litter_height = duet.assign_targets_from_sb40(query, "litter", "height")

        dens_params = duet.set_density(grass=grass_density, litter=litter_density)
        moist_params = duet.set_moisture(grass=grass_moisture, litter=litter_moisture)
        height_params = duet.set_height(grass=grass_height, litter=litter_height)

        calibrated = duet.calibrate(
            duet_run, [dens_params, moist_params, height_params]
        )
        moist = calibrated.to_numpy("integrated", "moisture")
        rhof = calibrated.to_numpy("integrated", "density")
        height = calibrated.to_numpy("integrated", "height")

        treesrhof = read_dat_to_array(site_dict, "treesrhof.dat", nx, ny, nz)
        treesmoist = read_dat_to_array(site_dict, "treesmoist.dat", nx, ny, nz)
        treesfueldepth = read_dat_to_array(site_dict, "treesfueldepth.dat", nx, ny, nz)

        treesrhof[0, :, :] = rhof
        treesmoist[0, :, :] = moist
        treesfueldepth[0, :, :] = height

        write_array_to_dat(treesrhof, "treesrhof.dat", qf_dict, reshape=False)
        write_array_to_dat(treesmoist, "treesmoist.dat", qf_dict, reshape=False)
        write_array_to_dat(treesfueldepth, "treesfueldepth.dat", qf_dict, reshape=False)

        for file in to_copy:
            copy(json_dict / file, qf_dict / file)

        sim = SimulationInputs.from_json(json_dict / f"{sites[i]}.json")
        sim.set_output_files(fuel_dens=True)
        sim.write_inputs(qf_dict)

        copy(qf_exe / "quicfire_MACI.exe", qf_dict / "quicfire_MACI.exe")
