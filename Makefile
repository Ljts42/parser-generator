HC=ghc -package regex-posix -package mtl
SOURCES=src\Main.hs src\Grammar.hs
GEN_SOURCES=src\Parser.y
GENERATED=src\Parser.hs
PACKAGE=hw0.zip

.PHONY: pack all run clean

all: parser

run: parser
	./parser

clean:
	del src\*.o src\*.hi
	del $(GENERATED)
	del parser.exe

parser: $(GENERATED) $(SOURCES)
	$(HC) -i.\src -tmpdir . .\src\Main.hs .\src\Lexer.hs -o parser

$(GENERATED): $(GEN_SOURCES) $(SOURCES)
	happy src\Parser.y -o src\Parser.hs

pack: $(GENERATED)
	7z a -r $(PACKAGE) Makefile src
