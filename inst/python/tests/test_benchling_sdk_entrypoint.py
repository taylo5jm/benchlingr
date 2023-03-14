import unittest
from unittest.mock import patch
from inst.python.benchling_sdk_entrypoint import benchling_sdk_entrypoint


class TestBenchlingSdkEntrypoint(unittest.TestCase):

    @patch('inst.python.benchling_sdk_entrypoint.Benchling')
    @patch('inst.python.benchling_sdk_entrypoint.ApiKeyAuth')
    def test_benchling_sdk_entrypoint(self, mock_auth, mock_benchling):
        tenant = 'https://example.benchling.com'
        api_key = 'my_api_key'

        benchling_sdk_entrypoint(tenant, api_key)

        mock_auth.assert_called_once_with(api_key)
        mock_benchling.assert_called_once_with(url=tenant, auth_method=mock_auth.return_value)
