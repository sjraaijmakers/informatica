# REPLACE STUDENT NAMES WITH YOURS
STUDENT_NAMES := student_name1_student_name2
DOCS := $(shell ls docs/report.pdf)
DELIVERABLE := $(STUDENT_NAMES).tar.gz
PYTHON_FILES := simpson_rule.py riemannsom.py \
				efficiency_simp_riemann_trap.py monte_carlo.py \
				efficiency_monte_carlo.py \
				common.py \
				trapezoidal_rule.py

all: $(DELIVERABLE)

check: $(PYTHON_FILES)
	pep8 $^
	pyflakes $^
	touch $(@)

$(DELIVERABLE): $(PYTHON_FILES) $(DOCS) check
	tar -czf $@ $(PYTHON_FILES) $(DOCS)
