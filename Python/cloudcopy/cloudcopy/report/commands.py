import click

from openpyxl.utils.exceptions import InvalidFileException


@click.group()
def report():
    pass
    """Subcommand-based interface"""


@report.command('summary')
@click.argument('file')
def report_summary(file):
    """Report Summary content"""
    print("report summary")


@report.command('show')
@click.argument('file')
@click.argument('sheet')
@click.pass_context
def report_show(ctx, file, sheet):
    """Show the Excel sheet content"""
    print("report show")
