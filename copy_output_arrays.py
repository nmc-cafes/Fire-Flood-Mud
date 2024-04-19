from pathlib import Path
from shutil import copytree

dst_dir = Path(__file__).parent / "Arrays"
dst_dir.mkdir(exist_ok=True)

fire_site_dict = {
    "Caldor": ["American", "Camp1", "Camp2", "Camp3", "Strawberry"],
    "CedarCreek": ["Cedar1", "Cedar2", "Wolf1", "Wolf2", "Wolf3"],
    "CubCreek2": ["Chewuch", "Eightmile", "Falls", "LambButte", "Sherwood"],
    "Dixie": ["Genesee", "Kings", "Rush", "Ward", "Yellow"],
    "KNP": ["AshMountain", "KaweahMiddle", "KaweahNorth", "Potwisha", "Yucca"],
}

for fire in fire_site_dict.keys():
    fire_src = Path(__file__).parent / "QF_runs" / fire
    fire_dst = dst_dir / fire
    fire_dst.mkdir(exist_ok=True)
    sites = fire_site_dict.get(fire)
    runs = [f"{fire}_{site}_duet" for site in sites]
    for run in runs:
        site_dst = fire_dst / run
        site_dst.mkdir(exist_ok=True)
        src = fire_src / run / "Arrays"
        dst = site_dst / "Arrays"
        copytree(src, dst)
