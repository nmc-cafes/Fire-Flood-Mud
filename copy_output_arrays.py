from pathlib import Path
from shutil import copytree

dst_dir = Path(__file__).parent / "Arrays"
dst_dir.mkdir(exist_ok=True)

fires = ["Caldor", "CedarCreek", "Dixie", "KNP"]

for fire in fires:
    fire_src = Path(__file__).parent / "QF_runs" / "Severe_Steep" / fire
    fire_dst = dst_dir / fire
    fire_dst.mkdir(exist_ok=True)
    sites = [f"{fire[:3]}{i}" for i in range(1, 21)]
    for run in sites:
        site_dst = fire_dst / run
        site_dst.mkdir(exist_ok=True)
        src = fire_src / run / "Arrays"
        dst = site_dst / "Arrays"
        copytree(src, dst)
