import click

@click.group()
def cmdgroup1():
    pass
    """First Command"""


@cmdgroup1.command('g1cmd1')
def g1_cmd1():
    """Commands for WP"""
    click.echo("Command 1 Group 1")


@cmdgroup1.command('g1cmd2')
@click.option("--test", help="This is the test option")
def g1_cmd2():
    """Test Install for WP"""
    print('Installing...')

