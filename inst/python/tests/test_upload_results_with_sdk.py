import unittest
from unittest.mock import MagicMock
from inst.python.upload_results_with_sdk import upload_results_with_sdk


class TestUploadResultsWithSdk(unittest.TestCase):
    def setUp(self):
        self.client = MagicMock()
        self.results = [
            {
                'project_id': 'src_ZRvTYOgM',
                'schema_id': 'assaysch_eBsoKyRO',
                'fields': [{'plate': {'value': 15}}]
            },
            {
                'project_id': 'src_ZRvTYOgM',
                'schema_id': 'assaysch_eBsoKyRO',
                'fields': [{'plate': {'value': 16}}]
            }
        ]

    def test_upload_results_with_sdk(self):
        created_result_id = '1234'
        created_results = [
            MagicMock(id=created_result_id),
            MagicMock(id='5678')
        ]
        response = MagicMock(assay_results=created_results)
        self.client.assay_results.create.return_value = response

        response_results = upload_results_with_sdk(self.client, self.results)

        self.assertEqual(len(response_results), 2)
        self.assertEqual(response_results[0].id, created_result_id)
        self.assertEqual(response_results[1].id, '5678')
