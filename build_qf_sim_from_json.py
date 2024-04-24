from pathlib import Path
from quicfire_tools import SimulationInputs

qf_dir = Path(__file__).parent / "QF_runs"

fire_site_dict = {
    "Caldor": ["American", "Camp1", "Camp2", "Camp3", "Strawberry"],
    "CedarCreek": ["Cedar1", "Cedar2", "Wolf1", "Wolf2", "Wolf3"],
    "CubCreek2": ["Chewuch", "Eightmile", "Falls", "LambButte", "Sherwood"],
    "Dixie": ["Genesee", "Kings", "Rush", "Ward", "Yellow"],
    "KNP": ["AshMountain", "KaweahMiddle", "KaweahNorth", "Potwisha", "Yucca"],
}

for fire in fire_site_dict.keys():
    for site in fire_site_dict[fire]:
        print(f"{fire} - {site}")
        run_path = qf_dir / fire / f"{fire}_{site}_duet"
        json_path = run_path / f"f{site}.json"
        sim = SimulationInputs.from_json(json_path)
        sim.write_inputs(run_path)
