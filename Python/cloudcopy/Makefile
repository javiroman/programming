.PHONY: clean virtualenv test docker dist dist-upload

clean:
	find . -name '*.py[co]' -delete
	rm -fr *.egg-info
	find cloudcopy -name __pycache__ | xargs rm -fr
	rm -fr build dist
	rm -fr __pycache__
	rm -fr .venv


virtualenv:
	virtualenv --prompt 'cloudcopy' .venv
	#.venv/bin/python setup.py develop
	pip install -r requirements.txt
	pip install --editable .
	@echo
	@echo "VirtualENV Setup Complete. Now run: source .venv/bin/activate"
	@echo

install:
	python setup.py install

test:
	python -m pytest \
		-v \
		--cov=myapp \
		--cov-report=term \
		--cov-report=html:coverage-report \
		tests/

dist: clean
	rm -rf dist/*
	python setup.py sdist
	python setup.py bdist_wheel
