DELIVERABLE := assignment1_$(shell ./scripts/get_student_ids.py).tar.gz
DOCS := \
	docs/report.pdf
PYTHON_SOURCES := \
	integration.py \
	main.py \
	monte_carlo.py

all: configure check pack

.config:
	@./scripts/configure.py

configure: .config

AUTHORS: .config
	@./scripts/get_authors.py > AUTHORS

.check: $(PYTHON_SOURCES)
	@env/bin/pep8 $^
	@env/bin/pyflakes $^
	@touch $@

check: .check

pack: $(PYTHON_SOURCES) $(DOCS) AUTHORS check
	@tar -czf \
		assignment1_$(shell ./scripts/get_student_ids.py).tar.gz \
		$(PYTHON_SOURCES) $(DOCS) AUTHORS

clean:
	@rm -f .config .check AUTHORS \
		assignment1_$(shell ./scripts/get_student_ids.py).tar.gz

.PHONY: all configure check pack clean
