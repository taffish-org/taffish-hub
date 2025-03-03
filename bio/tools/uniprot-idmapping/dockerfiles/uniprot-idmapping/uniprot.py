import os, re, time, zlib, requests  # noqa: E401
from abc import ABC
import pandas as pd
from requests.adapters import HTTPAdapter, Retry
from utils import *


class UniProtBase(ABC):
    '''Implementation of the base class for the UniProt REST API.'''

    _API_URL = 'https://rest.uniprot.org'

    def __init__(
        self,
        pooling_interval=3,
        total_retries=5,
        backoff_factor=0.25,
    ):
        '''Initialize the class. This will set up the session and retry mechanism.

        Parameters
        ----------
        pooling_interval : int, optional
            The interval in seconds between polling the API, by default 3
        total_retries : int, optional
            The total number of retries to attempt, by default 5
        backoff_factor : float, optional
            The backoff factor to use when retrying, by default 0.25
        '''
        self._POLLING_INTERVAL = pooling_interval
        self.retries = Retry(
            total=total_retries,
            backoff_factor=backoff_factor,
            status_forcelist=[500, 502, 503, 504],
        )
        self.session = requests.Session()
        self.session.mount('https://', HTTPAdapter(max_retries=self.retries))
        self._re_next_link = re.compile(r'<(.+)>; rel="next"')

    def submit_id_mapping(self, from_db: str, to_db: str, ids: list[str]):
        response = requests.post(
            f'{self._API_URL}/idmapping/run',
            data={'from': from_db, 'to': to_db, 'ids': ','.join(ids)},
        )
        check_response(response)
        return response.json()['jobId']

    def get_next_link(self, headers):
        if 'Link' in headers:
            match = self._re_next_link.match(headers['Link'])
            if match:
                return match.group(1)

    def check_id_mapping_ready(self, job_id: str, from_db: str, to_db: str):
        total_start = time.time()
        while True:
            request = self.session.get(f'{self._API_URL}/idmapping/status/{job_id}')
            check_response(request)
            j = request.json()
            if 'jobStatus' in j:
                if j['jobStatus'] in ('NEW', 'RUNNING'):
                    print(
                        f'Waiting for id mapping job ready, job status: {j["jobStatus"]}. '
                        f'Retrying in {self._POLLING_INTERVAL}s. '
                        f'Total waiting time: {time.time() - total_start:.2f}s.'
                    )
                    time.sleep(self._POLLING_INTERVAL)
                else:
                    raise Exception(j['jobStatus'])
            else:
                try:
                    ready = bool(j['results'] or j['failedIds'])
                except KeyError:
                    raise requests.RequestException(
                        f'Unexpected response from {from_db} to {to_db}.\n'
                        'request.json() missing "results" and "failedIds"'
                    )
                return ready

    def get_batch(self, batch_response: requests.Response, compressed=False):
        batch_url = self.get_next_link(batch_response.headers)
        while batch_url:
            batch_response = self.session.get(batch_url)
            batch_response.raise_for_status()
            yield self.decode_results(batch_response, compressed)
            batch_url = self.get_next_link(batch_response.headers)

    def get_id_mapping_results_link(self, job_id: str):
        url = f'{self._API_URL}/idmapping/details/{job_id}'
        request = self.session.get(url)
        check_response(request)
        return request.json()['redirectURL']

    def decode_results(self, response: requests.Response, compressed: bool):
        if compressed:
            decompressed = zlib.decompress(response.content, 16 + zlib.MAX_WBITS)
            return [line for line in decompressed.decode('utf-8').split('\n') if line]
        else:
            return [line for line in response.text.split('\n') if line]


class IDMapper(UniProtBase):
    '''Class for retrieving specific UniProt return fields. For the available fields,
    check the `fields_table` attribute or https://www.uniprot.org/help/return_fields.

    Returns:
        Tuple[pd.DataFrame, list]: A tuple containing a data frame with the results
        and a list of IDs that were not found.
    '''

    def __init__(
        self,
        pooling_interval=3,
        total_retries=5,
        backoff_factor=0.25,
    ) -> None:
        '''Initialize the class. This will set up the session and retry mechanism.

        Args:
            pooling_interval: The interval in seconds between polling the API.
                Defaults to 3.
            total_retries: The total number of retries to attempt. Defaults to 5.
            backoff_factor: The backoff factor to use when retrying. Defaults to 0.25.
        '''
        super().__init__(
            pooling_interval=pooling_interval,
            total_retries=total_retries,
            backoff_factor=backoff_factor,
        )
        self.default_fields = (
            ['accession', 'id', 'gene_names', 'protein_name', 'organism_name']
            + ['organism_id', 'go_id', 'go_p', 'go_c', 'go_f']
            + ['cc_subcellular_location', 'sequence']
        )

    @property
    def fields_table(self):
        '''https://www.uniprot.org/help/return_fields

        Returns
        -------
        DataFrame
            supported uniprot fields
        '''
        return pd.read_csv(
            os.path.join(
                os.path.dirname(__file__), 'resources', 'uniprot_return_fields.csv'
            )
        )

    @property
    def supported_dbs(self):
        '''Return a list of the supported databases as UniProt cross references.'''
        dbs = read_json(
            os.path.join(
                os.path.dirname(__file__), 'resources', 'uniprot_crossref_dbs.json'
            )
        )
        return sorted([dbs[k][i] for k in dbs for i in range(len(dbs[k]))])

    def get_id_mapping_results_stream(self, url: str, query_params: dict):
        if '/stream/' not in url:
            url = url.replace('/results/', '/results/stream/')
        response = self.session.get(url, params=query_params)
        check_response(response)
        results = self.decode_results(response, query_params['compressed'] == 'true')
        data = [d.split('\t') for d in results]
        return pd.DataFrame(data[1:], columns=data[0])

    def get_id_mapping_results_search(self, url: str, query_params: dict):
        '''Get the id mapping results from the UniProt API.'''
        compressed = query_params['compressed'] == 'true'
        check_response(self.session.get(url, allow_redirects=False))
        response = self.session.get(url, params=query_params)
        results = self.decode_results(response, compressed=compressed)
        for batch in self.get_batch(response, compressed):
            results += batch[1:]
        data = [d.split('\t') for d in results]
        return pd.DataFrame(data[1:], columns=data[0])

    def print_progress_batches(
        self, batch_index: int, size: int, n_retrieved: int, n_failed: int
    ):
        '''Prints the progress of a batch process.'''
        n_fetched = min((batch_index + 1) * size, n_retrieved)
        print(f'Fetched: {n_fetched} / {n_retrieved + n_failed}')

    def get(
        self,
        ids: list[str] | str,
        from_db='UniProtKB_AC-ID',
        to_db='UniParc',
        size=500,
        stream=True,
        compressed=False,
        fields: str | list[str] = 'default',
    ):
        '''Gets the requested fields from the UniProt ID Mapping API.
        Supported fields are listed in the `fields_table` attribute.
        For a complete list of the supported fields, check:
            https://www.uniprot.org/help/return_fields

        Args:
            ids: list of IDs to be mapped or single string.
            fields: list of UniProt fields to be retrieved. If None, will return the API's
                default fields. `Note:` parameter not supported for datasets that aren't
                strictly UniProtKB, e.g.: UniParc, UniRef... Defaults to None.
            from_db: database for the ids. Defaults to 'UniProtKB_AC-ID'.
            to_db: UniProtDB to query to. For reviewed-only accessions, use default. If
                you want to include unreviewed accessions, use 'UniProtKB'. Defaults to
                'UniParc'.
            compressed: compressed API request. Defaults to False.

        Raises:
            ValueError: If parameters `from_db`or `to_db` are not supported.

        Returns:
            Tuple[pd.DataFrame, list]: First element is a data frame with the
            results, second element is a list of failed IDs.
        '''
        if any([from_db not in self.supported_dbs, to_db not in self.supported_dbs]):
            raise ValueError(
                f'either {from_db} or {to_db} is not available. '
                f'Supported databases are {self.supported_dbs}'
            )
        if fields == 'default':
            fields = self.default_fields
        else:
            fields = [i.lower() for i in fields]
            fields_table = self.fields_table['returned_field'].values
            invalid = isin(fields, fields_table, invert=True)
            if invalid.any():
                raise ValueError(
                    f'Invalid fields. Invalid fields are: {compress(invalid, fields)}'
                )
        if to_db not in ['UniProtKB-Swiss-Prot', 'UniProtKB']:
            Warning(
                f'Custom fields not available when querying {to_db}.\n'
                'Using `.fields_table` to retrieve all available fields...'
            )
            fields = None
        else:
            fields = ','.join(fields)

        if isinstance(ids, str):
            ids = [ids]

        query_params = {
            'format': 'tsv',
            'fields': fields,
            'includeIsoform': 'false',
            'size': size,
            'compressed': 'true' if compressed else 'false',
        }
        if fields is None:
            query_params.pop('fields')
        if stream:
            query_params.pop('size')

        def _get_results(ids: list[str]):
            job_id = self.submit_id_mapping(from_db, to_db, ids)
            if self.check_id_mapping_ready(job_id, from_db, to_db):
                link = self.get_id_mapping_results_link(job_id)
                if stream:
                    df = self.get_id_mapping_results_stream(link, query_params)
                else:
                    df = self.get_id_mapping_results_search(link, query_params)
                failed = isin(ids, df['From'].values, invert=True)
                failed_ids = compress(failed, ids)
                self.print_progress_batches(
                    0, len(ids) if stream else size, df.shape[0], sum(failed)
                )
                return df, failed_ids

        if stream:
            return _get_results(ids)
        elif len(ids) > size:
            all_dfs, failed_ids = [], []
            for batch in divide_batches(ids, size=size):
                df, failed = _get_results(batch)
                all_dfs.append(df)
                failed_ids = failed_ids + failed
            df = pd.concat(all_dfs, ignore_index=True) if all_dfs else pd.DataFrame()
            return df, failed_ids
        else:
            return _get_results(ids)


def get_cross_referenced_databases(
    output_path: str = None, save=True, return_result=False
):
    '''Downloads the latest details on UniProt cross-referenced databses and stores it.
    This list of cross references can be found here:
        https://www.uniprot.org/database?query=*

    Args:
        output_path: the path to save the downloaded file with the cross references details.
            If left as None, will update the file stored in the package. Defaults to None.
            If not None, `save` will be set to True.
        save: whether to save or not the retrieved json. Defaults to True.

    Returns:
        dict: the json with the cross references details.
    '''

    url = 'https://rest.uniprot.org/database/stream?format=json&query=*'
    response = requests.get(url)
    result: dict = response.json()
    if response.status_code == 200:
        if save or output_path is not None:
            if output_path is None:
                output_path = os.path.join(
                    os.path.dirname(os.path.dirname(__file__)),
                    'resources',
                    'uniprot_crossref_db_detail.json',
                )
            save_json(result['results'], output_path)
            print('File downloaded successfully.')
    else:
        print(f'Failed to download the file. Status code: {response.status_code}')
    if return_result:
        return result
