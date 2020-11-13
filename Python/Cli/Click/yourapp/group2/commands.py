import click

@click.group()
def cmdgroup2():
    pass
    """First Command"""


@cmdgroup2.command('g2cmd1')
def g2_cmd1():
    """Commands for WP"""
    click.echo("Command 1 Group 2")


@cmdgroup2.command('g2cmd2')
@click.option("--test", help="This is the test option")
def g2_cmd2():
    """Test Install for WP"""
    print('Installing...')

