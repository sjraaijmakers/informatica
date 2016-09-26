# Set the location of the encoder and decoder programs.
#ENCODER=../freq-encoder
#DECODER=../freq-decoder
ENCODER=../encoder
DECODER=../decoder

decoded_files  = shorter-input.dec short-input.dec sample-input.dec \
				 sample-input-tilde.dec arm-processor.dec david-sedaris.dec

# default opts
ENCODE_OPTS = -l9

# Keep the intermediate .enc files.
.PRECIOUS: %.enc

# Test files encoded with starred-list size 0
l0: ENCODE_OPTS=-l0
l0: $(decoded_files)

# Test files encoded with starred-list size 1
l1: ENCODE_OPTS=-l1
l1: $(decoded_files)

# Test files encoded with starred-list size 9
l9: ENCODE_OPTS=-l9
l9: $(decoded_files)

# All tests
check:
	$(MAKE) clean
	# These tests will pass because -l0 encode does not change the input text
	$(MAKE) l0
	$(MAKE) clean
	# These will fail because the template code is not functional
	$(MAKE) l1
	$(MAKE) clean
	$(MAKE) l9


%.enc: %.txt
	@$(ENCODER) $(ENCODE_OPTS) $< > $@

%.dec: %.enc
	@$(DECODER) $< > $@
	@echo -n "testing $@:\t "
	@diff $(basename $<).txt $@ > /dev/null;\
	if [ $$? -ne 0 ];\
	then\
		echo "\033[0;31mfailed";\
	else \
		echo "\033[0;32mok";\
	fi
	@echo -n "\033[m\017";\

clean:
	rm -f *.dec *.enc
