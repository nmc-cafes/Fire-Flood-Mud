from pathlib import Path
from quicfire_tools import SimulationInputs
from shutil import copy

qf_dir = Path(__file__).parent / "QF_runs" / "Expanded_Sampling"

# fire_site_dict = {
#     "Caldor": ["American", "Camp1", "Camp2", "Camp3", "Strawberry"],
#     "CedarCreek": ["Cedar1", "Cedar2", "Wolf1", "Wolf2", "Wolf3"],
#     "CubCreek2": ["Chewuch", "Eightmile", "Falls", "LambButte", "Sherwood"],
#     "Dixie": ["Genesee", "Kings", "Rush", "Ward", "Yellow"],
#     "KNP": ["AshMountain", "KaweahMiddle", "KaweahNorth", "Potwisha", "Yucca"],
# }

fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]
ensemble_dir = Path(__file__).parent / "QF_runs" / "Expanded_Sampling"
qf_path = (
    Path(__file__).parent.parent.parent
    / "Quicfire"
    / "QF_6.0.0"
    / "exe"
    / "quicfire_LIN64.exe"
)

for fire in fires:
    sites = [f"{fire}{i}" for i in range(1, 16)]
    for site in sites:
        print(f"{fire} - {site}")
        run_path = qf_dir / fire / f"{fire}_{site}_duet"
        json_path = run_path / f"{site}.json"
        sim = SimulationInputs.from_json(json_path)
        sim.write_inputs(run_path)
        dst = run_path / "quicfire_LIN64.exe"
        copy(qf_path, dst)
        mac_path = run_path / "quicfire_MACI.exe"
        mac_path.unlink(missing_ok=True)
