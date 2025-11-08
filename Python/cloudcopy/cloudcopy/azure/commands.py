import click

from .ingest_request import IngestRequest
from openpyxl.utils.exceptions import InvalidFileException


@click.group()
def azure():
    pass
    """Subcommand-based interface"""


@azure.command('summary')
@click.argument('file')
def azure_summary(file):
    """Summary of excel file"""
    try:
        excel = IngestRequest(file)
        # excel.print_table_summary_real_count()
        excel.print_table_summary()
    except InvalidFileException:
        click.echo("ERROR: File not found or not Excel File!")


@azure.command('show')
@click.argument('file')
@click.argument('sheet')
@click.pass_context
def azure_show(ctx, file, sheet):
    """Show the Excel sheet content"""
    try:
        excel = IngestRequest(file)
        # example of how to handle debug parameter
        if ctx.obj['debug']:
            excel.print_sheet_content(sheet)

        # example of how to handle configuration file parameters in commands
        configfile = ctx.obj['configfile']
        for key in configfile['general']:
            click.echo(key)

        click.echo(configfile['general']['option1'])

    except InvalidFileException:
        click.echo("ERROR: File not found or not Excel File!")

