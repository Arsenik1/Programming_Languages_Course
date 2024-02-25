CC=gcc
FLEX=flex
LEXER_SRC=g++_lexer.l
LEXER_OBJ=gpp_lexer.c
EXECUTABLE=gpp_lexer
OUTPUT=output

.PHONY: all clean input_one input_two input_three

all: $(EXECUTABLE)

$(EXECUTABLE): $(LEXER_OBJ)
	$(CC) -o $(EXECUTABLE) $(LEXER_OBJ) -lfl

$(LEXER_OBJ): $(LEXER_SRC)
	$(FLEX) -o $(LEXER_OBJ) $(LEXER_SRC)

clean:
	rm -f $(LEXER_OBJ) $(EXECUTABLE) $(OUTPUT)

input_one: $(EXECUTABLE)
	./$(EXECUTABLE) < input_one > $(OUTPUT)

input_two: $(EXECUTABLE)
	./$(EXECUTABLE) < input_two > $(OUTPUT)

input_three: $(EXECUTABLE)
	./$(EXECUTABLE) < input_three > $(OUTPUT)
