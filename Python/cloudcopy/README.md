# CloudCopy: Data Ingest Tool for AWS and Azure

Ingest Tools

# Development Environment Setup

```
$ make virtualenv
$ source .venv/bin/activate
(cloudcopy) $ cloudcopy --help
Usage: cloudcopy [OPTIONS] COMMAND [ARGS]...

  Agnostic Data Ingest Tool for Public Cloud (AWS, Azure)

Options:
  --debug        debug application
  -v, --version  Show the version and exit.
  -h, --help     Show this message and exit.

Commands:
  aws
  azure
  report
  
(cloudcopy) $ cloudcopy aws --help
Usage: cloudcopy aws [OPTIONS] COMMAND [ARGS]...

Options:
  -h, --help  Show this message and exit.

Commands:
  copyfile      Copy the file FILE to AWS S3
  copyfolder    Copy folder files to AWS S3
  copyrdb       Copy RDB data to AWS S3
  getrdbmetada  Get RDB DLL from Database
  
(cloudcopy) $ cloudcopy --debug aws copyfile --transfer-type STREAM /path/to/file

```


# Technology Used

| Technology              | Site                    |
| ----------------------- | ----------------------- |
| Python Setuptools       | <a href="https://setuptools.readthedocs.io/en/latest" target="_blank">Setuptools Docs</a>|
| Click CLI Framework     | <a href="https://click.palletsprojects.com" target="_blank">Click Docs</a>               |
| Openpyxl Excel Library  | <a href="https://openpyxl.readthedocs.io/en/stable" target="_blank">Openpyxl Docs</a>    |
| JayDeBeApi JDBC Python DB-API  | <a href="https://pypi.org/project/JayDeBeApi" target="_blank">JayDeBeApi Docs</a> |


