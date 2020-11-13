.PHONY: clean virtualenv test docker dist dist-upload

clean:
	find . -name '*.py[co]' -delete
	rm -fr yourapp.egg-info
	find yourapp -name __pycache__ | xargs rm -fr
	rm -fr build dist


virtualenv:
	virtualenv --prompt '(myapp) ' .venv
	env/bin/pip install -r requirements-dev.txt
	env/bin/python setup.py develop
	@echo
	@echo "VirtualENV Setup Complete. Now run: source env/bin/activate"
	@echo

install:
	python setup.py install

dev:
	pip install --editable .

test:
	python -m pytest \
		-v \
		--cov=myapp \
		--cov-report=term \
		--cov-report=html:coverage-report \
		tests/

docker: clean
	docker build -t myapp:latest .

dist: clean
	rm -rf dist/*
	python setup.py sdist
	python setup.py bdist_wheel
