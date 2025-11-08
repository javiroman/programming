import click
import configparser

from .aws import commands as aws
from .azure import commands as azure
from .report import commands as report


@click.group(context_settings=dict(help_option_names=['-h', '--help']))
@click.option("--debug", is_flag=True, help='debug application')
@click.version_option(None, "-v", "--version", message="%(version)s")
@click.pass_context
def init(ctx, debug):
    """Agnostic Data Ingest Tool for Public Cloud (AWS, Azure)"""

    configfile = configparser.ConfigParser()
    try:
        with open('conf/cloudcopy.conf') as f:
            configfile.read_file(f)

        ctx.ensure_object(dict)
        ctx.obj['configfile'] = configfile
        ctx.obj['debug'] = debug
    except IOError:
        click.echo("ERROR: Unable to find config file")
        exit(1)


def main():
    init(obj={})


# Add main top level commands
init.add_command(aws.aws)
init.add_command(azure.azure)
init.add_command(report.report)
