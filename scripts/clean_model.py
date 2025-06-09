#!/usr/bin/env python3
import sys
import os
import shutil
from pathlib import Path

def clean_model(model_path):
    model_dir = Path(model_path).resolve()

    if not model_dir.exists():
        print(f"Error: {model_dir} does not exist.")
        sys.exit(1)

    # Folders to clear
    folders_to_clean = ['Output', 'Result', 'ResultRsv']
    files_to_delete = [
        model_dir / 'Input' / 'jsm12fit.inp',
        model_dir / 'Input' / 'jsmTaufit.inp',
        model_dir / 'Input' / 'tmp_parain.xdr',
        model_dir / 'pl_allRedd.pdf',
        model_dir / 'idl.ps',
        model_dir / 'temp_idl_input.pro'
    ]

    for folder_name in folders_to_clean:
        folder_path = model_dir / folder_name
        if folder_path.exists() and folder_path.is_dir():
            for item in folder_path.iterdir():
                try:
                    if item.is_file():
                        item.unlink()
                    elif item.is_dir():
                        shutil.rmtree(item)
                    print(f"Deleted: {item}")
                except Exception as e:
                    print(f"Failed to delete {item}: {e}")

    for file_path in files_to_delete:
        if file_path.exists():
            try:
                file_path.unlink()
                print(f"Deleted: {file_path}")
            except Exception as e:
                print(f"Failed to delete {file_path}: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: ./clean_model.py path/to/model_directory/")
        sys.exit(1)

    clean_model(sys.argv[1])