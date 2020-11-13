import click

from .group1 import commands as group1
from .group2 import commands as group2

@click.group()
def entry_point():
    pass

entry_point.add_command(group1.cmdgroup1)
entry_point.add_command(group2.cmdgroup2)
