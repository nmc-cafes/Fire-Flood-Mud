from pathlib import Path
from quicfire_tools import SimulationInputs
from shutil import copy

qf_dir = Path("D:/Fire-Flood-Mud/QF_runs/SBS")

# fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]
fires = ["CedarCreek", "CubCreek2"]
qf_path = (
    Path(__file__).parent.parent.parent
    / "Quicfire"
    / "QF_6.0.1"
    / "exe"
    / "quicfire_LIN64.exe"
)

for fire in fires:
    # sites = [f"{fire[:3]}{i}" for i in range(1, 21)]
    sites = [f"{fire[:3]}{i}_COR" for i in range(1, 4)]
    for site in sites:
        run_path = qf_dir / fire / site
        if run_path.exists():
            print(f"{fire} - {site}")
            json_path = run_path / f"{site}.json"
            sim = SimulationInputs.from_json(json_path)
            sim.write_inputs(run_path)
            dst = run_path / "quicfire_LIN64.exe"
            copy(qf_path, dst)
            mac_path = run_path / "quicfire_MACI.exe"
            mac_path.unlink(missing_ok=True)
