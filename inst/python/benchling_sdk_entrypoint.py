# benchling_sdk_entrypoint.py
# Initialize a Benchling API client object.
from benchling_sdk.benchling import Benchling
from benchling_sdk.auth.api_key_auth import ApiKeyAuth

def benchling_sdk_entrypoint(tenant, api_key):
    """
    Initialize a Benchling API client object
    
    :param tenant: URL for the Benchling tenant
    :param api_key: API key for the Benchling tenant
    
    """
    return Benchling(url=tenant, auth_method=ApiKeyAuth(api_key))
    
