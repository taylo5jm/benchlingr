# upload_results_with_sdk.py
#
from benchling_sdk.benchling import Benchling
from benchling_sdk.helpers.serialization_helpers import fields
from benchling_sdk.models import AssayResultCreate
from benchling_sdk.errors import BenchlingError


def upload_results_with_sdk(client: Benchling, results: list) -> list:
    """
    This method creates 100-items chunk and upload results as chunks due to API limitations
    :param client: Benchling client
    :param results: Assay results list
    :return: Created results benchling ID.
    """

    chunk_size = 99
    response_results = []
    chunked_list = [results[i:i + chunk_size] for i in range(0, len(results), chunk_size)]

    for result_chunk in chunked_list:
        created_results = []
        for result in result_chunk:
            # Use a context manager to commit results in a transaction. Will automatically
            # rollback on error. Otherwise commits the transaction on exit
            # with client.assay_results.transaction_manager() as results_manager:
            created_result = AssayResultCreate(
                project_id=result['project_id'],
                schema_id=result['schema_id'],
                fields=fields(result['fields'][0])
            )

            # Add this result to the transaction to be processed
            # You could also use extend() with up to 100 results at once
            # results_manager.append(created_result)
            # Track IDs I added, if desired
            created_results.append(created_result)
            # Continue to create and append more AssayResultCreate if desired
            # Check to see if my ID was created
            # transaction_result = client.assay_results.get_by_id(created_result.id)

        response = None
        try:
            response = client.assay_results.create(created_results)
        except BenchlingError as be:
            print(be)

        if response:
            response_results.extend(response.assay_results)  # benchling respond

    return response_results
