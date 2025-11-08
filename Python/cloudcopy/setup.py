from setuptools import setup, find_packages

setup(
    name='cloudcopy',
    version='0.0.1',
    author="Javi Roman",
    author_email="jroman.espinar@gmail.com",
    description="Agnostic Data Ingest Tool for Public Cloud (AWS, Azure)",
    packages=find_packages(),
    include_package_data=True,
    entry_points='''
        [console_scripts]
        cloudcopy=cloudcopy.main:main
    ''',
)
