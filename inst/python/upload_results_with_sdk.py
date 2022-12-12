# upload_results_with_sdk.py
#
import os

from benchling_sdk.helpers.serialization_helpers import fields
from benchling_sdk.models import AssayResultCreate

def upload_results_with_sdk(client, results):
  
    created_results = []
    for result in results:
        # Use a context manager to commit results in a transaction. Will automatically
        # rollback on error. Otherwise commits the transaction on exit
        # with client.assay_results.transaction_manager() as results_manager:
        created_result = AssayResultCreate(
            project_id = result['project_id'],
            schema_id = result['schema_id'],
            fields = fields(result['fields'][0])
        )
            
        # Add this result to the transaction to be processed
        # You could also use extend() with up to 100 results at once
        #results_manager.append(created_result)
        # Track IDs I added, if desired
        created_results.append(created_result)
        # Continue to create and append more AssayResultCreate if desired
        # Check to see if my ID was created
        # transaction_result = client.assay_results.get_by_id(created_result.id)
    
    client.assay_results.create(created_results)
    return(created_results)
