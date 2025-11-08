import click

from cloudcopy.aws.libs.copydata import CopyFile


@click.group(context_settings=dict(help_option_names=['-h', '--help']))
def aws():
    pass
    """Subcommand-based interface"""


@aws.command('copyfile')
@click.pass_context
@click.option("--transfer-type",
              type=click.Choice(['PLAIN', 'STEPS', 'STREAM'],
              case_sensitive=False),
              default=None,
              help="\b\nPLAIN: The file is copied to AWS S3 without compress"
                   "\b\nSTEPS: The file is compressed in the file system, and copied to AWS S3"
                   "\b\nSTREAM: The file is compressed on-the-fly and copied to AWS S3 in streaming")
@click.argument('file')
def aws_copyfile(ctx, transfer_type, file):
    """Copy the file FILE to AWS S3"""
    if ctx.obj['debug']:
        click.echo("debugging enabled")
        configfile = ctx.obj['configfile']
        for key in configfile['general']:
            click.echo(key)
        click.echo(configfile['general']['option1'])

    if transfer_type == 'PLAIN':
        click.echo("Copy file " +  file + " in PLAIN mode")
    elif transfer_type == 'STEPS':
        click.echo("Copy file " +  file + " in STEPS mode")
    elif transfer_type == 'STREAM':
        click.echo("Copy file " +  file + " in STREAM mode")

    p = CopyFile()
    p.copyFile()


@aws.command('copyfolder')
def aws_copyfolder():
    """Copy folder files to AWS S3"""
    click.echo("TODO: aws_copyfolder")

@aws.command('copyrdb')
def aws_copyrdb():
    """Copy RDB data to AWS S3"""
    click.echo("TODO: aws_copyrdb")

@aws.command('getrdbmetada')
def aws_getrdbmetadata():
    """Get RDB DLL from Database"""
    click.echo("TODO: aws_getrdbmetadata")

