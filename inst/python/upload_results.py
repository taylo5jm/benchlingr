
from benchling_sdk.helpers.serialization_helpers import fields
from benchling_sdk.models import AssayResultCreate
from benchling_sdk.auth.api_key_auth import ApiKeyAuth
from benchling_sdk.benchling import Benchling
# Setup an instance of Benchling
import os
from benchling_sdk.benchling import Benchling
from benchling_sdk.auth.api_key_auth import ApiKeyAuth


def upload_results(client, results):
  
    created_result_ids = []
    for result in results:
        # Use a context manager to commit results in a transaction. Will automatically
        # rollback on error. Otherwise commits the transaction on exit
        with benchling.assay_results.transaction_manager() as results_manager:
            created_result = AssayResultCreate(
                project_id = result['project_id'],
                schema_id = result['schema_id'],
                fields = fields(result['fields'])
            )
            
        # Add this result to the transaction to be processed
        # You could also use extend() with up to 100 results at once
        results_manager.append(created_result)
        # Track IDs I added, if desired
        created_result_ids.append(created_result.id)
        # Continue to create and append more AssayResultCreate if desired
        # Check to see if my ID was created
        transaction_result = benchling.assay_results.get_by_id(created_result.id)
