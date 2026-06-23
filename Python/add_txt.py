from pathlib import Path

arrays_path = Path(__file__).parent.parent / "Arrays"

fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]
for fire in fires:
    sites = [f"{fire}{i}" for i in range(1, 19)]
    for site in sites:
        run = f"{fire}_{site}_duet"
        file = arrays_path / fire / run / "Arrays" / "surface_consumption"
        newname = arrays_path / fire / run / "Arrays" / "surface_consumption.txt"
        file.rename(newname)
