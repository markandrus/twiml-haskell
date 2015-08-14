INPUT_EXAMPLES=$(shell find test/examples -name \*.hs)
OUTPUT_EXAMPLES=$(INPUT_EXAMPLES:%.hs=%.txt)

all: $(OUTPUT_EXAMPLES)

clean:
	@rm -f $(OUTPUT_EXAMPLES)

test/examples/%.txt: test/xml/%.xml
	example=$$(basename $? .xml); \
sed -e "s/$${example}/example/g" -e 's/^/> /g' <test/examples/$${example}.hs >$@; \
echo '\n>>> show example' >>$@; \
cat $? >>$@

.PHONY: all clean
