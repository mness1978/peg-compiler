#include "parser.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h> // For isalnum, isalpha, isdigit
#include "compiler.h" // Added for CompilerContext forward declaration

static void print_indent(int level) {
    for (int i = 0; i < level; ++i) {
        fprintf(stderr, "  ");
    }
}

#define PARSER_DEBUG_LOG_ENTRY(func_name, ctx, rule_str) \
    if ((ctx)->verbose) { \
        print_indent((ctx)->indent_level); \
        fprintf(stderr, "DEBUG: %s ENTRY at line %d, col %d (input_pos %zu) char='%c' (ASCII %d)%s%s\n", \
                func_name, (ctx)->line, (ctx)->column, (ctx)->current_pos, \
                (ctx->current_pos < ctx->input_len ? ctx->input[ctx->current_pos] : ' '), \
                (ctx->current_pos < ctx->input_len ? (int)ctx->input[ctx->current_pos] : 0), \
                (rule_str == NULL ? "" : " Rule: "), \
                (rule_str == NULL ? "" : rule_str)); \
        (ctx)->indent_level++; \
    }
#define PARSER_DEBUG_LOG_EXIT(func_name, ctx, success, rule_str) \
    if ((ctx)->verbose) { \
        (ctx)->indent_level--; \
        print_indent((ctx)->indent_level); \
        fprintf(stderr, "DEBUG: %s EXIT at line %d, col %d (input_pos %zu) char='%c' (ASCII %d) - %s%s%s\n", \
                func_name, (ctx)->line, (ctx)->column, (ctx)->current_pos, \
                (ctx->current_pos < ctx->input_len ? ctx->input[ctx->current_pos] : ' '), \
                (ctx->current_pos < ctx->input_len ? (int)ctx->input[ctx->current_pos] : 0), \
                (success ? "SUCCESS" : "FAIL"), \
                (rule_str == NULL ? "" : " Rule: "), \
                (rule_str == NULL ? "" : rule_str)); \
    }



// --- Helper functions for lexical analysis and backtracking ---

static void debug_print_char(char c) {
    if (c == '\n') {
        fprintf(stderr, "\\n");
    } else if (c == '\r') {
        fprintf(stderr, "\\r");
    } else if (c == '\t') {
        fprintf(stderr, "\\t");
    } else if (isprint(c)) {
        fprintf(stderr, "%c", c);
    } else {
        fprintf(stderr, "\\x%02x", (unsigned char)c);
    }
}

static void debug_print_string(const char *s) {
    if (!s) return;
    for (int i = 0; s[i] != '\0'; i++) {
        debug_print_char(s[i]);
    }
}

// Save current parser state for backtracking
static void save_state(ParserContext *ctx, size_t *pos, int *line, int *column) {
    *pos = ctx->current_pos;
    *line = ctx->line;
    *column = ctx->column;
}

// Restore parser state for backtracking
static void restore_state(ParserContext *ctx, size_t pos, int line, int column) {
    ctx->current_pos = pos;
    ctx->line = line;
    ctx->column = column;
}

// Advance parser position, updating line/column
static void advance(ParserContext *ctx, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        if (ctx->current_pos >= ctx->input_len) return;
        if (ctx->input[ctx->current_pos] == '\n') {
            ctx->line++;
            ctx->column = 1;
        } else {
            ctx->column++;
        }
        ctx->current_pos++;
    }
}

// Check if current position matches a literal string
static int peek_literal(ParserContext *ctx, const char *literal) {
    size_t len = strlen(literal);
    if (ctx->current_pos + len > ctx->input_len) {
        return 0;
    }
    return strncmp(ctx->input + ctx->current_pos, literal, len) == 0;
}

// Match and consume a literal string
static int match_literal(ParserContext *ctx, const char *literal) {
    PARSER_DEBUG_LOG_ENTRY("match_literal", ctx, literal);
    size_t len = strlen(literal);
    if (peek_literal(ctx, literal)) {
        advance(ctx, len);
        PARSER_DEBUG_LOG_EXIT("match_literal", ctx, 1, literal);
        return 1;
    }
    PARSER_DEBUG_LOG_EXIT("match_literal", ctx, 0, literal);
    return 0;
}

// Match and consume a single character
static int match_char(ParserContext *ctx, char c) {
    char c_str[2] = {c, '\0'};
    PARSER_DEBUG_LOG_ENTRY("match_char", ctx, c_str);
    if (ctx->current_pos < ctx->input_len) {

    } else {

    }

    if (ctx->current_pos < ctx->input_len && ctx->input[ctx->current_pos] == c) {
        advance(ctx, 1);
        PARSER_DEBUG_LOG_EXIT("match_char", ctx, 1, c_str);
        return 1;
    }
    PARSER_DEBUG_LOG_EXIT("match_char", ctx, 0, c_str);
    return 0;
}

// Match any single character
static int match_any_char(ParserContext *ctx) {
    if (ctx->current_pos < ctx->input_len) {
        advance(ctx, 1);
        return 1;
    }
    return 0;
}

// Match character from a set (e.g., "abc", "a-z")
// This is a simplified version and might need refinement for full PEG compliance
static int match_char_from_set(ParserContext *ctx, const char *set_str) {
    if (ctx->current_pos >= ctx->input_len) return 0;
    char c = ctx->input[ctx->current_pos];

    const char *p = set_str;
    while (*p) {
        if (p[0] == '-' && p > set_str && p[1] && p[1] != '-') { // Range
            char start = p[-1];
            char end = p[1];
            if (c >= start && c <= end) {
                advance(ctx, 1);
                return 1;
            }
            p += 2; // Skip '-' and end char
        } else if (c == *p) { // Single char
            advance(ctx, 1);
            return 1;
        }
        p++;
    }
    return 0;
}

// Match IdentifierStart: [a-zA-Z_]
static int match_ident_start(ParserContext *ctx) {
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (ctx->current_pos >= ctx->input_len) {
        return 0;
    }

    char c = ctx->input[ctx->current_pos];
    if (isalpha(c) || c == '_') {
        advance(ctx, 1);
        return 1;
    }

    return 0;
}

// Match IdentifierCont: [a-zA-Z_0-9]
static int match_ident_cont(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("match_ident_cont", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (ctx->current_pos >= ctx->input_len) {
        PARSER_DEBUG_LOG_EXIT("match_ident_cont", ctx, 0, NULL);
        return 0;
    }

    char c = ctx->input[ctx->current_pos];
    if (isalnum(c) || c == '_') {
        advance(ctx, 1);
        PARSER_DEBUG_LOG_EXIT("match_ident_cont", ctx, 1, NULL);
        return 1;
    }

    restore_state(ctx, p_pos, p_line, p_col);
    PARSER_DEBUG_LOG_EXIT("match_ident_cont", ctx, 0, NULL);
    return 0;
}

// --- PEG Grammar Rules as Parser Functions ---

// Forward declarations for recursive calls
static ASTNode *parse_definition(ParserContext *ctx);
static ASTNode *parse_expression(ParserContext *ctx);
static ASTNode *parse_sequence(ParserContext *ctx);
static ASTNode *parse_prefix(ParserContext *ctx);
static ASTNode *parse_suffix(ParserContext *ctx);
static ASTNode *parse_primary(ParserContext *ctx);
static ASTNode *parse_capture_expression(ParserContext *ctx);
static ASTNode *parse_semantic_action(ParserContext *ctx); // Forward declaration for semantic action
static char *parse_identifier_text(ParserContext *ctx);
static char *parse_literal_text(ParserContext *ctx);
static char *parse_class_text(ParserContext *ctx);
static char *parse_action_text(ParserContext *ctx);
static int parse_spacing(ParserContext *ctx);
static int parse_comment(ParserContext *ctx);
static int parse_end_of_line(ParserContext *ctx);
static int parse_end_of_file(ParserContext *ctx);
static char parse_char_in_literal_or_class(ParserContext *ctx);


// Spacing <- ((' ' / '\t' / '\n' / '\r') / Comment)*
static int parse_spacing(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_spacing", ctx, NULL);
    while (1) {
        size_t current_pos_before_attempt = ctx->current_pos;

        if (match_char(ctx, ' ')) {
            // Matched space
        } else if (match_char(ctx, '\t')) {
            // Matched tab
        } else if (match_char(ctx, '\n')) {
            // Matched newline
        } else if (match_char(ctx, '\r')) {
            // Matched carriage return
        } else if (parse_comment(ctx)) {
            // Matched comment
        } else {
            // No spacing character or comment found, break the loop
            break;
        }

        if (ctx->current_pos == current_pos_before_attempt) {
            break;
        }
    }
    PARSER_DEBUG_LOG_EXIT("parse_spacing", ctx, 1, NULL); // Always succeeds, even if no spacing
    return 1;
}

// Comment <- '#' (!EndOfLine .)* EndOfLine
static int parse_comment(ParserContext *ctx) {
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (!match_char(ctx, '#')) {
        restore_state(ctx, p_pos, p_line, p_col);
        return 0;
    }

    while (1) {
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        if (parse_end_of_line(ctx)) { // !EndOfLine
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }
        if (!match_any_char(ctx)) { // .
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }
    }

    if (!parse_end_of_line(ctx)) {
        restore_state(ctx, p_pos, p_line, p_col);
        return 0;
    }
    return 1;
}

// EndOfLine <- '\r\n' / '\n' / '\r'
static int parse_end_of_line(ParserContext *ctx) {
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (match_literal(ctx, "\r\n")) return 1;
    if (match_char(ctx, '\n')) return 1;
    if (match_char(ctx, '\r')) return 1;

    restore_state(ctx, p_pos, p_line, p_col);
    return 0;
}

// EndOfFile <- !.
static int parse_end_of_file(ParserContext *ctx) {
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (match_any_char(ctx)) { // .
        restore_state(ctx, p_pos, p_line, p_col);
        return 0; // If we can match any char, it's not EOF
    }
    return 1; // Cannot match any char, so it's EOF
}

// Char <- '\' [abefnrtv'"[\\]
//      / '\' [0-3][0-7][0-7]
//      / '\' [0-7][0-7]?
//      / '\' '-'
//      / !'\' .
static char parse_char_in_literal_or_class(ParserContext *ctx) {
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (match_char(ctx, '\\')) {
        if (ctx->current_pos >= ctx->input_len) {
            restore_state(ctx, p_pos, p_line, p_col);
            return 0; // Malformed escape
        }
        char escaped_char = ctx->input[ctx->current_pos];
        advance(ctx, 1);

        switch (escaped_char) {
            case 'a': return '\a';
            case 'b': return '\b';
            case 'e': return '\x1b'; // ESC
            case 'f': return '\f';
            case 'n': return '\n';
            case 'r': return '\r';
            case 't': return '\t';
            case 'v': return '\v';
            case '\'': return '\'';
            case '"': return '"';
            case '[': return '[';
            case ']': return ']';
            case '\\': return '\\';
            case '-': return '-'; // Custom for PEG
            default:
                // Handle octal escapes: \0-3][0-7][0-7], \0-7][0-7]?, \0-7
                if (isdigit(escaped_char) && escaped_char >= '0' && escaped_char <= '7') {
                    char octal_str[4] = {escaped_char, 0, 0, 0};
                    int octal_len = 1;
                    if (ctx->current_pos < ctx->input_len && isdigit(ctx->input[ctx->current_pos]) && ctx->input[ctx->current_pos] >= '0' && ctx->input[ctx->current_pos] <= '7') {
                        octal_str[octal_len++] = ctx->input[ctx->current_pos];
                        advance(ctx, 1);
                        if (ctx->current_pos < ctx->input_len && isdigit(ctx->input[ctx->current_pos]) && ctx->input[ctx->current_pos] >= '0' && ctx->input[ctx->current_pos] <= '7' && (escaped_char >= '0' && escaped_char <= '3')) { // Only 3 digits if first is 0-3
                            octal_str[octal_len++] = ctx->input[ctx->current_pos];
                            advance(ctx, 1);
                        }
                    }
                    return (char)strtol(octal_str, NULL, 8);
                }
                // If not a recognized escape, treat as literal escaped char
                return escaped_char;
        }
    } else { // !'\' .
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        if (match_char(ctx, '\\')) { // Check for !'\'
            restore_state(ctx, pp_pos, pp_line, pp_col);
            return 0; // It was a backslash, so this alternative fails
        }
        restore_state(ctx, pp_pos, pp_line, pp_col); // Restore after peek
        if (match_any_char(ctx)) { // .
            char matched_char = ctx->input[ctx->current_pos - 1];
            return matched_char; // Return the matched char
        }
    }

    restore_state(ctx, p_pos, p_line, p_col);
    return 0; // No match
}

// Range <- Char '-' Char / Char
static int parse_range_text(ParserContext *ctx, char *start_char, char *end_char) {
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    char c1 = parse_char_in_literal_or_class(ctx);
    if (c1 == 0) {
        restore_state(ctx, p_pos, p_line, p_col);
        return 0;
    }

    if (match_char(ctx, '-')) {
        char c2 = parse_char_in_literal_or_class(ctx);
        if (c2 == 0) {
            restore_state(ctx, p_pos, p_line, p_col);
            return 0;
        }
        *start_char = c1;
        *end_char = c2;
        return 2; // Matched a range
    } else {
        *start_char = c1;
        *end_char = c1; // Single char treated as range of one
        return 1; // Matched a single char
    }
}

// Class <- '[' < (!']' Range)* > ']' Spacing
static char *parse_class_text(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_class_text", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    parse_spacing(ctx);  // Consume any leading whitespace before '['

    if (!match_char(ctx, '[')) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_class_text", ctx, 0, NULL); // Failure
        return NULL;
    }

    size_t content_start_pos = ctx->current_pos;

    // Optional negation
    if (match_char(ctx, '^')) {
        // This is part of the content, so content_start_pos should reflect this
    }

    while (1) {
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);

        if (match_char(ctx, ']')) { // !']'
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }

        char start_c, end_c;
        if (parse_range_text(ctx, &start_c, &end_c)) {
            // Matched a range or single char, continue
        } else {
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }
    }

    if (!match_char(ctx, ']')) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_class_text", ctx, 0, NULL); // Failure
        return NULL;
    }

    size_t content_end_pos = ctx->current_pos - 1; // Exclude ']'

    char *class_content = (char *)malloc(content_end_pos - content_start_pos + 1);
    if (!class_content) {
        perror("Failed to allocate class content string");
        exit(EXIT_FAILURE);
    }
    strncpy(class_content, ctx->input + content_start_pos, content_end_pos - content_start_pos);
    class_content[content_end_pos - content_start_pos] = '\0';

    parse_spacing(ctx); // Spacing

    // fprintf(stderr, "DEBUG: parse_class_text: Parsed class '%s'\n", class_content);
    PARSER_DEBUG_LOG_EXIT("parse_class_text", ctx, 1, class_content); // Success
    return class_content;
}


// Literal <- ['] < (!['] Char )* > ['] Spacing
//          / ["] < (!["] Char )* > ["] Spacing
static char *parse_literal_text(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_literal_text", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    parse_spacing(ctx);  // Consume any leading whitespace before quote

    char quote_char = 0;
    if (match_char(ctx, '\'')) {
        quote_char = '\'';
    } else if (match_char(ctx, '"')) {
        quote_char = '"';
    } else {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_literal_text", ctx, 0, NULL); // Failure
        return NULL;
    }

    StringList *chars = NULL;
    size_t literal_len = 0;

    while (1) {
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);

        if (match_char(ctx, quote_char)) { // !quote_char
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }

        char c = 0;
        size_t ppp_pos;
        int ppp_line, ppp_col;
        save_state(ctx, &ppp_pos, &ppp_line, &ppp_col);
        if (match_char(ctx, '\\')) { // Check for escaped character
            restore_state(ctx, ppp_pos, ppp_line, ppp_col); // Restore to re-parse with parse_char_in_literal_or_class
            c = parse_char_in_literal_or_class(ctx);
        } else if (ctx->current_pos < ctx->input_len && ctx->input[ctx->current_pos] != quote_char) { // Direct literal character
            c = ctx->input[ctx->current_pos];
            advance(ctx, 1);
        } else {
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }
        
        if (c != 0) {
            char *char_str = (char*)malloc(2); // For char + null terminator
            if (!char_str) { perror("malloc"); exit(EXIT_FAILURE); }
            char_str[0] = c;
            char_str[1] = '\0';
            chars = ast_string_list_append(chars, char_str);
            literal_len++;
        } else {
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }
    }

    if (!match_char(ctx, quote_char)) {
        ast_free_string_list(chars);
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_literal_text", ctx, 0, NULL); // Failure
        return NULL;
    }

    parse_spacing(ctx); // Spacing

    char *result_literal = (char *)malloc(literal_len + 1);
    if (!result_literal) {
        perror("Failed to allocate literal string");
        exit(EXIT_FAILURE);
    }
    result_literal[0] = '\0';
    StringList *current = chars;
    while (current) {
        strcat(result_literal, current->str);
        current = current->next;
    }
    ast_free_string_list(chars);

    // fprintf(stderr, "DEBUG: parse_literal_text: Parsed literal '%s'\n", result_literal);
    PARSER_DEBUG_LOG_EXIT("parse_literal_text", ctx, 1, result_literal); // Success
    return result_literal;
}

// Action <- '{' < [^}]* > '}' Spacing
static char *parse_action_text(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_action_text", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (!match_char(ctx, '{')) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_action_text", ctx, 0, NULL); // Failure
        return NULL;
    }

    size_t content_start_pos = ctx->current_pos;
    int brace_count = 1;

    while (ctx->current_pos < ctx->input_len && brace_count > 0) {
        if (match_char(ctx, '{')) {
            brace_count++;
        } else if (match_char(ctx, '}')) {
            brace_count--;
        } else {
            advance(ctx, 1); // Consume any other character
        }
    }

    if (brace_count != 0) { // Unmatched brace
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_action_text", ctx, 0, NULL); // Failure
        return NULL;
    }

    size_t content_end_pos = ctx->current_pos - 1; // Exclude '}'

    char *action_content = (char *)malloc(content_end_pos - content_start_pos + 1);
    if (!action_content) {
        perror("Failed to allocate action content string");
        exit(EXIT_FAILURE);
    }
    strncpy(action_content, ctx->input + content_start_pos, content_end_pos - content_start_pos);
    action_content[content_end_pos - content_start_pos] = '\0';

    parse_spacing(ctx); // Spacing

    // fprintf(stderr, "DEBUG: parse_action_text: Parsed action '%s'\n", action_content);
    PARSER_DEBUG_LOG_EXIT("parse_action_text", ctx, 1, NULL); // Success
    return action_content;
}

// SemanticAction <- '{' (!'}' .)* '}' Spacing
// This function parses a C-style semantic action block, handling nested braces.
static ASTNode *parse_semantic_action(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_semantic_action", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    // Try to parse spacing, but if the action fails, restore to before spacing
    size_t sp_pos;
    int sp_line, sp_col;
    save_state(ctx, &sp_pos, &sp_line, &sp_col);
    parse_spacing(ctx); // Consume leading spacing

    if (ctx->current_pos >= ctx->input_len || *(ctx->input + ctx->current_pos) != '{') {
        restore_state(ctx, p_pos, p_line, p_col); // Restore to before spacing
        PARSER_DEBUG_LOG_EXIT("parse_semantic_action", ctx, 0, NULL); // Failure
        return NULL;
    }

    advance(ctx, 1); // Manually advance after successful match

    size_t content_start_pos = ctx->current_pos;
    int brace_count = 1;

    while (ctx->current_pos < ctx->input_len && brace_count > 0) {
        char current_char = ctx->input[ctx->current_pos];
        if (current_char == '{') {
            brace_count++;
            advance(ctx, 1);
        } else if (current_char == '}') {
            brace_count--;
            advance(ctx, 1);
        } else {
            advance(ctx, 1); // Consume any other character
        }
    }

    if (brace_count != 0) { // Unmatched brace
        restore_state(ctx, p_pos, p_line, p_col);
        ctx->error_message = "Unmatched '{' in semantic action";
        ctx->error_line = p_line; // Error at the start of the action
        ctx->error_column = p_col;
        PARSER_DEBUG_LOG_EXIT("parse_semantic_action", ctx, 0, NULL); // Failure
        return NULL;
    }

    size_t content_end_pos = ctx->current_pos - 1; // Exclude the final '}'

    char *action_content = (char *)malloc(content_end_pos - content_start_pos + 1);
    if (!action_content) {
        perror("Failed to allocate semantic action content string");
        exit(EXIT_FAILURE);
    }
    strncpy(action_content, ctx->input + content_start_pos, content_end_pos - content_start_pos);
    action_content[content_end_pos - content_start_pos] = '\0';

    parse_spacing(ctx); // Spacing after the action block

    ASTNode *action_node = ast_new_node(NODE_SEMANTIC_ACTION, p_line, p_col);
    action_node->value = action_content;
    PARSER_DEBUG_LOG_EXIT("parse_semantic_action", ctx, 1, NULL); // Success
    return action_node;
}

// Identifier <- < IdentStart IdentCont* > Spacing
static char *parse_identifier_text(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_identifier_text", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    size_t id_start_pos = ctx->current_pos;
    if (!match_ident_start(ctx)) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_identifier_text", ctx, 0, NULL); // Failure
        return NULL;
    }

    while (match_ident_cont(ctx)) {}

    size_t id_end_pos = ctx->current_pos;

    char *identifier = (char *)malloc(id_end_pos - id_start_pos + 1);
    if (!identifier) {
        perror("Failed to allocate identifier string");
        exit(EXIT_FAILURE);
    }
    strncpy(identifier, ctx->input + id_start_pos, id_end_pos - id_start_pos);
    identifier[id_end_pos - id_start_pos] = '\0';

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_IDENT: Before parse_spacing - identifier='%s', pos=%zu, line=%d, col=%d\n", 
                identifier, ctx->current_pos, ctx->line, ctx->column);
    }
    
    parse_spacing(ctx); // Spacing after the identifier

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_IDENT: After parse_spacing - identifier='%s', pos=%zu, line=%d, col=%d, next_char='%c' (ASCII %d)\n", 
                identifier, ctx->current_pos, ctx->line, ctx->column,
                ctx->current_pos < ctx->input_len ? ctx->input[ctx->current_pos] : ' ',
                ctx->current_pos < ctx->input_len ? (int)ctx->input[ctx->current_pos] : 0);
    }
    
    // fprintf(stderr, "DEBUG: parse_identifier_text: Parsed identifier '%s'\n", identifier);
    PARSER_DEBUG_LOG_EXIT("parse_identifier_text", ctx, 1, identifier); // Success
    return identifier;
}

// CaptureGroup <- '<' Expression '>'
static ASTNode *parse_capture_expression(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_capture_expression", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    if (!match_char(ctx, '<')) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_capture_expression", ctx, 0, NULL); // Failure
        return NULL;
    }
    parse_spacing(ctx); // Consume spacing after '<'

    // Record start position for capture
    size_t capture_start_pos = ctx->current_pos;

    ASTNode *expr = parse_expression(ctx);
    if (!expr) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_capture_expression", ctx, 0, NULL); // Failure
        return NULL;
    }

    // Record end position for capture
    size_t capture_end_pos = ctx->current_pos;

    parse_spacing(ctx); // Consume spacing before '>'
    if (!match_char(ctx, '>')) {
        ast_free(expr); // Free the parsed expression if '>' is missing
        restore_state(ctx, p_pos, p_line, p_col);
        ctx->error_message = "Expected '>' after capture expression";
        ctx->error_line = ctx->line;
        ctx->error_column = ctx->column;
        PARSER_DEBUG_LOG_EXIT("parse_capture_expression", ctx, 0, NULL); // Failure
        return NULL;
    }
    parse_spacing(ctx); // Consume spacing after '>'

    // Store capture positions in context
    ctx->captured_text_start_pos = capture_start_pos;
    ctx->captured_text_end_pos = capture_end_pos;

    // Populate yytext and yyleng
    if (ctx->yytext) {
        free(ctx->yytext); // Free previous captured text
    }
    ctx->yyleng = capture_end_pos - capture_start_pos;
    ctx->yytext = (char *)malloc(ctx->yyleng + 1);
    if (!ctx->yytext) {
        perror("Failed to allocate yytext");
        exit(EXIT_FAILURE);
    }
    strncpy(ctx->yytext, ctx->input + capture_start_pos, ctx->yyleng);
    ctx->yytext[ctx->yyleng] = '\0';

    // Create a CAPTURE node in the AST
    ASTNode *capture_node = ast_new_node(NODE_CAPTURE, p_line, p_col);
    capture_node->data.child = expr; // The captured expression is a child
    PARSER_DEBUG_LOG_EXIT("parse_capture_expression", ctx, 1, NULL); // Success
    return capture_node;
}

// Primary <- Identifier !LEFTARROW
//          / OPEN Expression CLOSE
//          / Literal
//          / Class
//          / DOT
//          / Action
//          / BEGIN
//          / END
static ASTNode *parse_primary(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_primary", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);
    ASTNode *node = NULL;

    // Try CaptureGroup first
    node = parse_capture_expression(ctx);
    if (node) {
        ASTNode *primary_node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        primary_node->data.child = node;
        primary_node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return primary_node;
    }
    restore_state(ctx, p_pos, p_line, p_col); // Restore if capture group failed

    // Identifier !LEFTARROW SemanticAction?
    // Note: Need to consume spacing first since Identifier rule only consumes trailing spacing
    parse_spacing(ctx);
    char *id_text = parse_identifier_text(ctx);
    if (id_text) {
        if (ctx->verbose) {
            fprintf(stderr, "DEBUG_PRIMARY: Parsed identifier '%s' - pos=%zu\n",  id_text, ctx->current_pos);
        }
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        if (peek_literal(ctx, "<-")) { // !LEFTARROW
            if (ctx->verbose) {
                fprintf(stderr, "DEBUG_PRIMARY: Found '<-' after '%s', rejecting\n", id_text);
            }
            restore_state(ctx, pp_pos, pp_line, pp_col);
            free(id_text); // This is a definition, not a primary identifier
            restore_state(ctx, p_pos, p_line, p_col);
            PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 0, NULL); // Failure
            return NULL;
        }
        if (ctx->verbose) {
            fprintf(stderr, "DEBUG_PRIMARY: No '<-' after '%s', accepting\n", id_text);
        }
        restore_state(ctx, pp_pos, pp_line, pp_col); // Restore after peek
        node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        node->data.child = ast_new_node(NODE_IDENTIFIER, p_line, p_col);
        node->data.child->value = id_text; // Assign to child's value
        node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        if (ctx->verbose) {
            fprintf(stderr, "DEBUG_PRIMARY: Returning identifier '%s' - pos=%zu\n", id_text, ctx->current_pos);
        }
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return node;
    }
    restore_state(ctx, p_pos, p_line, p_col); // Restore after failed identifier

    save_state(ctx, &p_pos, &p_line, &p_col);
    parse_spacing(ctx);  // Consume any leading whitespace before '('
    if (match_char(ctx, '(')) { // OPEN Expression CLOSE
        parse_spacing(ctx);
        ASTNode *expr = parse_expression(ctx);
        if (expr && match_char(ctx, ')')) {
            parse_spacing(ctx);
            node = ast_new_node(NODE_PRIMARY, p_line, p_col);
            node->data.child = ast_new_node(NODE_GROUP, p_line, p_col);
            node->data.child->data.child = expr;
            node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
            PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
            return node;
        }
    }
    restore_state(ctx, p_pos, p_line, p_col);

    save_state(ctx, &p_pos, &p_line, &p_col);
    char *literal_text = parse_literal_text(ctx); // Literal SemanticAction?
    if (literal_text) {
        node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        node->data.child = ast_new_node(NODE_LITERAL, p_line, p_col);
        node->data.child->value = literal_text; // Assign to child's value
        node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return node;
    }
    restore_state(ctx, p_pos, p_line, p_col);

    save_state(ctx, &p_pos, &p_line, &p_col);
    char *class_text = parse_class_text(ctx); // Class SemanticAction?
    if (class_text) {
        node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        node->data.child = ast_new_node(NODE_CLASS, p_line, p_col);
        node->data.child->value = class_text; // Assign to child's value
        node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return node;
    }
    restore_state(ctx, p_pos, p_line, p_col);

    save_state(ctx, &p_pos, &p_line, &p_col);
    if (match_char(ctx, '.')) { // DOT SemanticAction?
        parse_spacing(ctx);
        node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        node->data.child = ast_new_node(NODE_DOT, p_line, p_col);
        node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return node;
    }
    restore_state(ctx, p_pos, p_line, p_col);

    save_state(ctx, &p_pos, &p_line, &p_col);
    char *action_text = parse_action_text(ctx); // Action SemanticAction?
    if (action_text) {
        node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        node->data.child = ast_new_node(NODE_ACTION, p_line, p_col);
        node->data.child->value = action_text; // Assign to child's value
        node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return node;
    }
    restore_state(ctx, p_pos, p_line, p_col);

    // Re-add BEGIN and END cases, now that semantic actions are handled generically
    save_state(ctx, &p_pos, &p_line, &p_col);
    if (match_char(ctx, '<')) { // BEGIN SemanticAction?
        parse_spacing(ctx);
        node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        node->data.child = ast_new_node(NODE_BEGIN_CAPTURE, p_line, p_col);
        node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return node;
    }
    restore_state(ctx, p_pos, p_line, p_col);

    save_state(ctx, &p_pos, &p_line, &p_col);
    if (match_char(ctx, '>')) { // END SemanticAction?
        parse_spacing(ctx);
        node = ast_new_node(NODE_PRIMARY, p_line, p_col);
        node->data.child = ast_new_node(NODE_END_CAPTURE, p_line, p_col);
        node->action = parse_semantic_action(ctx); // Optional semantic action for the primary node
        PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 1, NULL); // Success
        return node;
    }
    restore_state(ctx, p_pos, p_line, p_col);

    PARSER_DEBUG_LOG_EXIT("parse_primary", ctx, 0, NULL); // Failure
    return NULL;
}

// Suffix <- Primary (QUESTION / STAR / PLUS)?
static ASTNode *parse_suffix(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_suffix", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    ASTNode *primary = parse_primary(ctx);
    if (!primary) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_suffix", ctx, 0, NULL); // Failure
        return NULL;
    }

    ASTNode *suffix_node = ast_new_node(NODE_SUFFIX, p_line, p_col);
    suffix_node->data.suffix.primary = primary;
    suffix_node->data.suffix.quantifier_type = 0; // No quantifier by default
    suffix_node->data.suffix.action = NULL; // Initialize

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_SUFFIX: After parse_primary - pos=%zu, line=%d, col=%d, next_char='%c' (ASCII %d)\n",
                ctx->current_pos, ctx->line, ctx->column,
                ctx->current_pos < ctx->input_len ? ctx->input[ctx->current_pos] : ' ',
                ctx->current_pos < ctx->input_len ? (int)ctx->input[ctx->current_pos] : 0);
    }

    if (match_char(ctx, '?')) {
        suffix_node->data.suffix.quantifier_type = NODE_QUANTIFIER_OPTIONAL;
        suffix_node->data.suffix.action = parse_semantic_action(ctx); // Optional semantic action
    } else if (match_char(ctx, '*')) {
        suffix_node->data.suffix.quantifier_type = NODE_QUANTIFIER_ZERO_OR_MORE;
        suffix_node->data.suffix.action = parse_semantic_action(ctx); // Optional semantic action
    } else if (match_char(ctx, '+')) {
        suffix_node->data.suffix.quantifier_type = NODE_QUANTIFIER_ONE_OR_MORE;
        suffix_node->data.suffix.action = parse_semantic_action(ctx); // Optional semantic action
    }

    PARSER_DEBUG_LOG_EXIT("parse_suffix", ctx, 1, NULL); // Success
    return suffix_node;
}

// Prefix <- AND Action
//          / AND Suffix
//          / NOT Suffix
//          / Suffix
static ASTNode *parse_prefix(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_prefix", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    ASTNode *prefix_node = ast_new_node(NODE_PREFIX, p_line, p_col);
    prefix_node->data.prefix.prefix_type = 0; // No predicate by default
    prefix_node->data.prefix.suffix = NULL; // Initialize
    prefix_node->data.prefix.action = NULL; // Initialize for AND Action case

    if (match_char(ctx, '&')) { // AND
        parse_spacing(ctx);
        prefix_node->data.prefix.prefix_type = NODE_PREDICATE_AND;

        // Try to match Action first (which is a semantic action)
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        ASTNode *action_node = parse_semantic_action(ctx); // Use parse_semantic_action
        if (action_node) {
            prefix_node->data.prefix.action = action_node; // Store the action directly
            PARSER_DEBUG_LOG_EXIT("parse_prefix", ctx, 1, NULL); // Success
            return prefix_node;
        }
        restore_state(ctx, pp_pos, pp_line, pp_col);

        // If not Action, then it must be Suffix
        ASTNode *suffix = parse_suffix(ctx);
        if (suffix) {
            prefix_node->data.prefix.suffix = suffix;
            parse_semantic_action(ctx); // Optional semantic action after Suffix
            PARSER_DEBUG_LOG_EXIT("parse_prefix", ctx, 1, NULL); // Success
            return prefix_node;
        }
    } else if (match_char(ctx, '!')) { // NOT
        parse_spacing(ctx);
        prefix_node->data.prefix.prefix_type = NODE_PREDICATE_NOT;
        ASTNode *suffix = parse_suffix(ctx);
        if (suffix) {
            prefix_node->data.prefix.suffix = suffix;
            parse_semantic_action(ctx); // Optional semantic action after Suffix
            PARSER_DEBUG_LOG_EXIT("parse_prefix", ctx, 1, NULL); // Success
            return prefix_node;
        }
    } else { // Just Suffix
        ASTNode *suffix = parse_suffix(ctx);
        if (suffix) {
            prefix_node->data.prefix.suffix = suffix;
            parse_semantic_action(ctx); // Optional semantic action after Suffix
            PARSER_DEBUG_LOG_EXIT("parse_prefix", ctx, 1, NULL); // Success
            return prefix_node;
        } else {
            if (ctx->verbose) {
                fprintf(stderr, "DEBUG_PREFIX: parse_suffix failed at pos=%zu\n", ctx->current_pos);
            }
        }
    }

    ast_free(prefix_node); // Free the partially created node
    restore_state(ctx, p_pos, p_line, p_col);
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_PREFIX: Returning NULL from parse_prefix, restored to pos=%zu\n", ctx->current_pos);
    }
    PARSER_DEBUG_LOG_EXIT("parse_prefix", ctx, 0, NULL); // Failure
    return NULL;
}

// Sequence <- Prefix (Prefix)*
static ASTNode *parse_sequence(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_sequence", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    ASTNodeList *prefixes = NULL;
    ASTNode *first_prefix = parse_prefix(ctx);
    if (!first_prefix) {
        // Handle the '/ { push(makePredicate("1")); }' alternative
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        ASTNode *predicate_action = parse_semantic_action(ctx); // Optional semantic action
        if (predicate_action) {
            ASTNode *sequence_node = ast_new_node(NODE_SEQUENCE, p_line, p_col);
            sequence_node->data.sequence.empty_sequence_action = predicate_action;
            PARSER_DEBUG_LOG_EXIT("parse_sequence", ctx, 1, NULL); // Success
            return sequence_node;
        }
        restore_state(ctx, pp_pos, pp_line, pp_col);

        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_sequence", ctx, 0, NULL); // Failure
        return NULL;
    }
    prefixes = ast_new_node_list(first_prefix);

    while (1) {
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        if (ctx->verbose) {
            fprintf(stderr, "DEBUG_SEQ: Loop iteration - saved_pos=%zu, current_pos=%zu\n", pp_pos, ctx->current_pos);
        }
        
        ASTNode *next_prefix = parse_prefix(ctx);
        if (next_prefix) {
            if (ctx->verbose) {
                fprintf(stderr, "DEBUG_SEQ: Parsed prefix - pos=%zu\n", ctx->current_pos);
            }
            prefixes = ast_node_list_append(prefixes, next_prefix);
            parse_semantic_action(ctx); // Optional semantic action after Prefix
            if (ctx->verbose) {
                fprintf(stderr, "DEBUG_SEQ: After semantic action - pos=%zu\n", ctx->current_pos);
            }
        } else {
            if (ctx->verbose) {
                fprintf(stderr, "DEBUG_SEQ: No more prefixes, restoring to pos=%zu\n", pp_pos);
            }
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }
    }

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_SEQ: Returning from parse_sequence - pos=%zu\n", ctx->current_pos);
    }

    ASTNode *sequence_node = ast_new_node(NODE_SEQUENCE, p_line, p_col);
    sequence_node->data.sequence.prefixes = prefixes;
    sequence_node->data.sequence.empty_sequence_action = NULL; // Initialize
    PARSER_DEBUG_LOG_EXIT("parse_sequence", ctx, 1, NULL); // Success
    return sequence_node;
}

// Expression <- Sequence (SLASH Sequence)* (AND Action)?
static ASTNode *parse_expression(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_expression", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    ASTNodeList *sequences = NULL;
    ASTNode *first_sequence = parse_sequence(ctx);
    if (!first_sequence) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_expression", ctx, 0, NULL); // Failure
        return NULL;
    }
    sequences = ast_new_node_list(first_sequence);

    while (1) {
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        // Consume spacing before checking for SLASH (since SLASH may be on a new line)
        parse_spacing(ctx);
        if (match_char(ctx, '/')) { // SLASH
            parse_spacing(ctx);
            ASTNode *next_sequence = parse_sequence(ctx);
            if (next_sequence) {
                sequences = ast_node_list_append(sequences, next_sequence);
                parse_semantic_action(ctx); // Optional semantic action after SLASH Sequence
            } else {
                // Malformed alternation, but we already matched '/'
                // This is an error case, but for now, just break.
                restore_state(ctx, pp_pos, pp_line, pp_col);
                break;
            }
        } else {
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break;
        }
    }

    ASTNode *expression_node = ast_new_node(NODE_EXPRESSION, p_line, p_col);
    expression_node->data.expression.sequences = sequences;
    expression_node->data.expression.trailing_predicate = NULL; // Initialize

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_EXPR: Before trailing predicate check - pos=%zu, line=%d, col=%d\n",
                ctx->current_pos, ctx->line, ctx->column);
    }

    // Optional trailing predicate (AND SemanticAction)
    size_t tp_pos;
    int tp_line, tp_col;
    save_state(ctx, &tp_pos, &tp_line, &tp_col);
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_EXPR: Saved state - tp_pos=%zu\n", tp_pos);
    }
    // Don't consume spacing here - it should already be consumed by the previous token
    // parse_spacing(ctx); // REMOVED - was causing incorrect backtracking
    if (match_char(ctx, '&')) { // AND
        parse_spacing(ctx);
        ASTNode *trailing_pred_action = parse_semantic_action(ctx); // Use parse_semantic_action
        if (trailing_pred_action) {
            expression_node->data.expression.trailing_predicate = trailing_pred_action;
        } else {
            // Matched '&' but no action, so backtrack
            restore_state(ctx, tp_pos, tp_line, tp_col);
        }
    } else {
        restore_state(ctx, tp_pos, tp_line, tp_col);
    }

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_EXPR: Returning from parse_expression - pos=%zu, line=%d, col=%d\n",
                ctx->current_pos, ctx->line, ctx->column);
    }
    
    PARSER_DEBUG_LOG_EXIT("parse_expression", ctx, 1, NULL); // Success
    return expression_node;
}

static ASTNode *parse_definition(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_definition", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    parse_spacing(ctx); // Consume leading spacing

    char *identifier_text = parse_identifier_text(ctx); // Matches Identifier
    if (!identifier_text) {
        restore_state(ctx, p_pos, p_line, p_col);
        PARSER_DEBUG_LOG_EXIT("parse_definition", ctx, 0, NULL); // Failure
        return NULL;
    }

    parse_spacing(ctx); // Consume spacing before the first semantic action
    // DEBUG: Print current position before matching first semantic action
    // fprintf(stderr, "DEBUG: Before first semantic action at line %d, col %d. Char: '%c' (input_pos %zu)\n", ctx->line, ctx->column, ctx->input[ctx->current_pos], ctx->current_pos);
    ASTNode *pre_arrow_action = parse_semantic_action(ctx); // This is the first semantic action call

    if (!match_literal(ctx, "<-")) { // LEFTARROW
        free(identifier_text);
        ast_free(pre_arrow_action); // Free if it was parsed
        restore_state(ctx, p_pos, p_line, p_col);
        ctx->error_message = "Expected '<-'";
        ctx->error_line = ctx->line;
        ctx->error_column = ctx->column;
        PARSER_DEBUG_LOG_EXIT("parse_definition", ctx, 0, identifier_text); // Failure
        return NULL;
    }
    parse_spacing(ctx);

    ASTNode *expression = parse_expression(ctx);
    if (!expression) {
        free(identifier_text);
        ast_free(pre_arrow_action);
        restore_state(ctx, p_pos, p_line, p_col);
        ctx->error_message = "Expected expression after '<-'";
        ctx->error_line = ctx->line;
        ctx->error_column = ctx->column;
        PARSER_DEBUG_LOG_EXIT("parse_definition", ctx, 0, identifier_text); // Failure
        return NULL;
    }

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_DEF: After expression - pos=%zu, line=%d, col=%d\n",
                ctx->current_pos, ctx->line, ctx->column);
    }
    
    ASTNode *post_expression_action = parse_semantic_action(ctx); // Optional semantic action after Expression

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_DEF: After post-expression action - pos=%zu, line=%d, col=%d\n",
                ctx->current_pos, ctx->line, ctx->column);
    }

    // Handle the '&{ YYACCEPT }' part. This is a trailing predicate with an action.
    // For now, we'll just parse the action if '&' is present.
    ASTNode *trailing_predicate_action = NULL;
    size_t tp_pos;
    int tp_line, tp_col;
    save_state(ctx, &tp_pos, &tp_line, &tp_col);
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_DEF: Before checking for '&' - pos=%zu, line=%d, col=%d, saved_pos=%zu\n",
                ctx->current_pos, ctx->line, ctx->column, tp_pos);
    }
    
    if (match_char(ctx, '&')) {
        if (ctx->verbose) {
            fprintf(stderr, "DEBUG_DEF: Matched '&' - pos=%zu\n", ctx->current_pos);
        }
        parse_spacing(ctx);
        trailing_predicate_action = parse_semantic_action(ctx);
        if (!trailing_predicate_action) {
            // Matched '&' but no action, so backtrack
            if (ctx->verbose) {
                fprintf(stderr, "DEBUG_DEF: No action after '&', restoring to pos=%zu\n", tp_pos);
            }
            restore_state(ctx, tp_pos, tp_line, tp_col);
        }
    } else {
        if (ctx->verbose) {
            fprintf(stderr, "DEBUG_DEF: No '&' found, restoring to pos=%zu (current was %zu)\n", tp_pos, ctx->current_pos);
        }
        restore_state(ctx, tp_pos, tp_line, tp_col);
    }

    if (ctx->verbose) {
        fprintf(stderr, "DEBUG_DEF: After trailing predicate handling - pos=%zu, line=%d, col=%d\n",
                ctx->current_pos, ctx->line, ctx->column);
    }



    ASTNode *definition_node = ast_new_node(NODE_DEFINITION, p_line, p_col);
    definition_node->value = identifier_text; // Store identifier in node->value
    definition_node->data.definition.pre_expression_action = pre_arrow_action;
    definition_node->data.definition.expression = expression;
    definition_node->data.definition.post_expression_action = post_expression_action;
    definition_node->data.definition.rule_predicate_action = trailing_predicate_action; // Store the &{ YYACCEPT } action
    PARSER_DEBUG_LOG_EXIT("parse_definition", ctx, 1, identifier_text); // Success
    return definition_node;
}

// Grammar <- Spacing Definition+ EndOfFile
ASTNode *parse_grammar(ParserContext *ctx) {
    PARSER_DEBUG_LOG_ENTRY("parse_grammar", ctx, NULL);
    size_t p_pos;
    int p_line, p_col;
    save_state(ctx, &p_pos, &p_line, &p_col);

    parse_spacing(ctx); // Spacing

    ASTNode *first_definition = parse_definition(ctx);
    if (!first_definition) {
        ctx->error_message = "Expected at least one definition";
        ctx->error_pos = ctx->current_pos;
        PARSER_DEBUG_LOG_EXIT("parse_grammar", ctx, 0, NULL); // Failure
        return NULL;
    }
    ASTNodeList *definitions = ast_new_node_list(first_definition);
    // Loop to parse zero or more definitions
    while (1) {
        size_t pp_pos;
        int pp_line, pp_col;
        save_state(ctx, &pp_pos, &pp_line, &pp_col);
        ASTNode *next_definition = parse_definition(ctx);
        if (next_definition) {
            definitions = ast_node_list_append(definitions, next_definition);
        } else {
            restore_state(ctx, pp_pos, pp_line, pp_col);
            break; // No more definitions
        }
    }

    // Grammar <- Spacing Definition+ EndOfFile
    // After parsing all definitions, consume any trailing spacing before checking for EOF
    parse_spacing(ctx);
    
    if (!parse_end_of_file(ctx)) {
        ast_free_list(definitions);
        ctx->error_message = "Expected EndOfFile";
        ctx->error_pos = ctx->current_pos;
        PARSER_DEBUG_LOG_EXIT("parse_grammar", ctx, 0, NULL); // Failure
        return NULL;
    }

    ASTNode *grammar_node = ast_new_node(NODE_GRAMMAR, p_line, p_col);
    grammar_node->data.grammar.definitions = definitions;
    PARSER_DEBUG_LOG_EXIT("parse_grammar", ctx, 1, NULL); // Success
    return grammar_node;
}

// --- Public Parser Functions ---

void parser_init(ParserContext *ctx, const char *input, size_t input_len) {
    memset(ctx, 0, sizeof(ParserContext));
    ctx->input = input;
    ctx->input_len = input_len;
    ctx->current_pos = 0;
    ctx->line = 1;
    ctx->column = 1;
    ctx->error_message = NULL;
    ctx->error_pos = 0; // Initialize error_pos
    ctx->yytext = NULL; // Initialize yytext
    ctx->yyleng = 0;   // Initialize yyleng
    ctx->verbose = 0; // Initialize verbose flag
    ctx->indent_level = 0; // Initialize indent_level
}

void parser_cleanup(ParserContext *ctx) {
    if (ctx->yytext) {
        free(ctx->yytext);
        ctx->yytext = NULL;
    }
}