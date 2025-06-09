#!/usr/bin/env python3
import os
import shutil
import sys
import subprocess
from pathlib import Path

def create_model_structure(model_name):
    base = Path(__file__).resolve().parent.parent
    model_path = base / "models" / model_name

    input_dir = model_path / "Input"
    output_dir = model_path / "Output"
    result_dir = model_path / "Result"
    resultrsv_dir = model_path / "ResultRsv"

    # Create directories
    for d in [input_dir, output_dir, result_dir, resultrsv_dir]:
        d.mkdir(parents=True, exist_ok=True)

    # Copy Init files
    init_src = base / "Data" / "Init"
    if not init_src.exists():
        print("Error: Data/Init folder not found.")
        sys.exit(1)

    for file in init_src.iterdir():
        if file.is_file():
            shutil.copy(file, input_dir)

    # Copy d.Qmie files
    qfile_src = base / "Qfile"
    for fname in ["d.Qmie_vGr", "d.Qmie_vSi"]:
        src_file = qfile_src / fname
        if src_file.exists():
            shutil.copy(src_file, input_dir)
        else:
            print(f"Warning: {fname} not found in Qfile")

    # Copy af90.j → a.j
    af90_path = base / "build" / "af90.j"
    if af90_path.exists():
        shutil.copy(af90_path, model_path / "a.j")
    else:
        print("Warning: build/af90.j not found")

    # Copy run_jismo.sh
    jismo_sh = base / "scripts" / "run_jismo.sh"
    if jismo_sh.exists():
        shutil.copy(jismo_sh, model_path / "run_jismo.sh")
    else:
        print("Warning: scripts/run_jismo.sh not found")

    print(f"Model structure for '{model_name}' created successfully.")

    # Call copy_qfiles.py interactively
    copy_script = base / "scripts" / "copy_qfiles.py"
    if copy_script.exists():
        print("\nNow running copy_qfiles.py to configure d.Qellip* files.\n")
        subprocess.run([str(copy_script), str(model_path)])
    else:
        print("Warning: scripts/copy_qfiles.py not found — skipping.")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: ./scripts/create_model.py MODELNAME")
        sys.exit(1)

    create_model_structure(sys.argv[1])