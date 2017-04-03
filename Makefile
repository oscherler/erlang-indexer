# http://www.erlang-factory.com/upload/presentations/33/EUnitinPractice.pdf

ERLC_FLAGS=

SOURCES=$(wildcard *.erl)
HEADERS=$(wildcard *.hrl)
OBJECTS=$(SOURCES:%.erl=ebin/%.beam)

all: compile test

compile: $(OBJECTS)

ebin/%.beam: %.erl $(HEADERS) Makefile
	@mkdir -p $(@D)
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS)

test:
	@erl -noshell -pa ebin \
	-eval 'eunit:test( "ebin", [ verbose ] )' \
	-s init stop

gettysburg: text=gettysburg-address.txt
gettysburg: print

dickens: text=dickens-christmas.txt
dickens: print

print: compile
	@erl -noshell -pa ebin \
	-eval 'indexer:print_file_index("text/'$(text)'")' \
	-s init stop
