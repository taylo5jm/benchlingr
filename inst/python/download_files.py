# download_file.py
## Download file attachments from Benchling
## ===================================
import json
import os
from pathlib import Path
from typing import Dict


def download_files(client, file_map: Dict[str, str]) -> None:
    """
      Download file attachments from Benchling
      
      :param client: Benchling API client object. 
        See `benchling_sdk_entrypoint.py`
      :param file_map: Dictionary where keys are the blob identifiers and 
        values are the corresponding file paths to be created on the local machine.
    """
    for blob_id,destination_path in file_map.items():
        # Get the notebook entry
        try:
            client.blobs.download_file(blob_id=blob_id, destination_path=Path(destination_path))
        except FileExistsError:
            pass
