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


run_dict = {
    "Caldor": [0.07, 0.18, 0.07],
    "Dixie": [0.07, 0.18, 0.07],
    "KNP": [0.07, 0.17, 0.07],
    "CedarCreek": [0.09, 0.18, 0.09],
    "CubCreek2": [0.08, 0.18, 0.08],
}

qf_exe = Path(__file__).parent.parent.parent / "Quicfire" / "QF_6.0.1" / "exe"

fires = ["Caldor", "Dixie", "KNP", "CedarCreek", "CubCreek2"]
for i in range(len(fires)):
    if i == 1:
        for j in range(1, 21):
            if j == 5:
                print(f"{fires[i][:3]}{j}")
                site_dict = (
                    Path(__file__).parent
                    / fires[i]
                    / "Sample_Basins"
                    / f"{fires[i][:3]}{j}"
                )
                shp = gpd.read_file(site_dict / f"{fires[i][:3]}{j}.shp")
                # query = duet.query_landfire(shp.geometry[0], site_dict, 5070)
                fg = zarr.open(
                    site_dict / f"{fires[i][:3]}{j}_fastfuels.zarr", mode="r"
                )
                nx = fg.attrs["nx"]
                ny = fg.attrs["ny"]
                nz = fg.attrs["nz"]
                nsp = find_nsp(site_dict)
                to_copy = [
                    "ignite.dat",
                    "topo.dat",
                    f"{fires[i][:3]}{j}.json",
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
                for fire in run_dict.keys():
                    qf_dict = (
                        Path(__file__).parent
                        / "QF_runs"
                        / fires[i]
                        / f"{fires[i][:3]}{j}"
                    )
                    qf_dict.mkdir(exist_ok=True)

                    json_dict = (
                        Path(__file__).parent
                        / "QF_runs"
                        / fires[i]
                        / f"{fires[i][:3]}{j}"
                    )

                    conditions = run_dict.get(fire)

                    duet_run = duet.import_duet(site_dict, nx, ny, nsp)

                    grass_moisture = duet.assign_targets(
                        method="constant", value=conditions[0]
                    )
                    litter_moisture = duet.assign_targets(
                        method="maxmin", max=conditions[1], min=conditions[2]
                    )
                    # grass_density = duet.assign_targets_from_sb40(query, "grass", "density")
                    # litter_density = duet.assign_targets_from_sb40(query, "litter", "density")
                    # grass_height = duet.assign_targets_from_sb40(query, "grass", "height")
                    # litter_height = duet.assign_targets_from_sb40(query, "litter", "height")

                    # dens_params = duet.set_density(grass=grass_density, litter=litter_density)
                    moist_params = duet.set_moisture(
                        grass=grass_moisture, litter=litter_moisture
                    )
                    # height_params = duet.set_height(grass=grass_height, litter=litter_height)

                    # calibrated = duet.calibrate(
                    #     duet_run, [dens_params, moist_params, height_params]
                    # )
                    calibrated = duet.calibrate(duet_run, moist_params)
                    moist = calibrated.to_numpy("integrated", "moisture")
                    # rhof = calibrated.to_numpy("integrated", "density")
                    # height = calibrated.to_numpy("integrated", "height")

                    # treesrhof = read_dat_to_array(site_dict, "treesrhof.dat", nx, ny, nz)
                    treesmoist = read_dat_to_array(
                        site_dict, "treesmoist.dat", nx, ny, nz
                    )
                    # treesfueldepth = read_dat_to_array(
                    #     site_dict, "treesfueldepth.dat", nx, ny, nz
                    # )

                    # treesrhof[0, :, :] = rhof
                    treesmoist[0, :, :] = moist
                    # treesfueldepth[0, :, :] = height

                    # write_array_to_dat(treesrhof, "treesrhof.dat", qf_dict, reshape=False)
                    write_array_to_dat(
                        treesmoist, "treesmoist.dat", qf_dict, reshape=False
                    )
                    # write_array_to_dat(
                    #     treesfueldepth, "treesfueldepth.dat", qf_dict, reshape=False
                    # )

                    # for file in to_copy:
                    #     copy(json_dict / file, qf_dict / file)

                    # sim = SimulationInputs.from_json(json_dict / f"{fires[:3]}{j}.json")
                    # sim.set_output_files(fuel_dens=True)
                    # sim.quic_fire.nz = nz
                    # sim.quic_fire.sim_time = 10800
                    # sim.write_inputs(qf_dict)

                    # copy(qf_exe / "quicfire_MACI.exe", qf_dict / "quicfire_MACI.exe")
