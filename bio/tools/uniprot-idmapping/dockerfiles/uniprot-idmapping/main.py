import os
import sys
import pandas as pd
from argparse import ArgumentParser
from uniprot import IDMapper


def main(args):
    args = vars(args)
    mapper = IDMapper()
    match args['info']:
        case 'fields':
            with pd.option_context('display.max_rows', None, 'display.max_columns', None):
                print(mapper.fields_table.iloc[:,:2])
            return
        case 'databases':
            print(mapper.supported_dbs)
            return
        case _:
            pass

    params = {
        'ids': pd.read_csv(args['input'], header=None)[0].tolist(),
        'from_db': args['from'],
        'to_db': args['to'],
        'size': args['size'],
        'stream': args['stream'],
        'compressed': args['compressed'],
    }
    if args['task'] == 'mapping':
        results, failed = mapper.get(**params)
    else:
        if os.path.exists(args['fields']):
            fields = pd.read_csv(args['fields'], header=None)[0].tolist()
        else:
            fields = args['fields']
        results, failed = mapper.get(**params, fields=fields)

    results.to_csv(args['output'], index=False)
    print(f'Results are saved in {args["output"]}')
    if failed:
        print(f'{len(failed)} failed: \n{failed}')


def has_non_default_args(args, parser):
    non_defaults = [k for k, v in vars(args).items() if v != parser.get_default(k)]
    return len(non_defaults) > 0


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument(
        '--info',
        type=str,
        required=False,
        choices=['fields', 'databases'],
        help='Show supported fields or databases',
    )

    parser.add_argument(
        '-i', '--input', type=str, help='Input file, each line as the id to map'
    )
    parser.add_argument(
        '-o',
        '--output',
        type=str,
        default='idmapping_results.csv',
        help='Output file, the mapped ids. Defaults to `idmapping_results.csv`',
    )
    parser.add_argument(
        '--from',
        type=str,
        default='UniProtKB_AC-ID',
        help='From database. Defaults to `UniProtKB_AC-ID`. Use --info to check supported databases',
    )
    parser.add_argument(
        '--to',
        type=str,
        default='UniParc',
        help='To database. Defaults to `UniParc`. Use --info to check supported databases',
    )
    parser.add_argument(
        '--task',
        type=str,
        default='mapping',
        choices=['mapping', 'query'],
        help='Task name. Defaults to `mapping`. If query, only supported to-databases are UniProtKB-Swiss-Prot and UniProtKB',
    )
    parser.add_argument(
        '--stream', type=bool, default=True, help='Stream mode. Defaults to True'
    )
    parser.add_argument(
        '--compressed', type=bool, default=False, help='Compressed. Defaults to False'
    )
    parser.add_argument(
        '--size',
        type=int,
        default=500,
        help='Batch size. If not stream mode, default 500',
    )
    parser.add_argument(
        '--fields',
        type=str,
        default='default',
        required=False,
        help='Fields to be returned. More fields can be in a file. Supported to-databases are UniProtKB-Swiss-Prot and UniProtKB. If task is mapping, this is ignored',
    )

    args = parser.parse_args()
    if not has_non_default_args(args, parser):
        parser.print_help()
        sys.exit(1)
    main(args)
