#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include <stddef.h> // For size_t

// Parser Context structure
typedef struct ParserContext {
    const char *input;
    size_t input_len;
    size_t current_pos;
    int line;
    int column;
    const char *error_message;
    size_t error_pos; // Added error_pos
    int error_line;
    size_t error_column;
    size_t captured_text_start_pos; // Start position of the last captured text
    size_t captured_text_end_pos;   // End position of the last captured text
    char *yytext;                   // Captured text for semantic actions
    size_t yyleng;                  // Length of captured text
    int verbose;                    // Flag for verbose debug output
    int indent_level;               // Indentation level for hierarchical logging
} ParserContext;

// Function prototypes for the parser
void parser_init(ParserContext *ctx, const char *input, size_t input_len);
ASTNode *parse_grammar(ParserContext *ctx);
void parser_cleanup(ParserContext *ctx);

// Helper functions (internal to parser.c, but declared here for completeness if needed elsewhere)
// These will typically be static in parser.c
// static void skip_spacing(ParserContext *ctx);
// static int match_literal(ParserContext *ctx, const char *literal);
// static char *match_identifier(ParserContext *ctx);
// static ASTNode *parse_definition(ParserContext *ctx);
// static ASTNode *parse_expression(ParserContext *ctx);
// static ASTNode *parse_sequence(ParserContext *ctx);
// static ASTNode *parse_prefix(ParserContext *ctx);
// static ASTNode *parse_suffix(ParserContext *ctx);
// static ASTNode *parse_primary(ParserContext *ctx);
// static ASTNode *parse_literal(ParserContext *ctx);
// static ASTNode *parse_class(ParserContext *ctx);
// static ASTNode *parse_action(ParserContext *ctx);

#endif // PARSER_H