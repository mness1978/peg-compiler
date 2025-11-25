#ifndef AST_H
#define AST_H

#include <stddef.h> // For size_t

// Forward declarations for linked lists
typedef struct ASTNodeList ASTNodeList;
typedef struct StringList StringList;

// Enum for different types of AST nodes
typedef enum {
    NODE_GRAMMAR,
    NODE_DEFINITION,
    NODE_EXPRESSION,
    NODE_SEQUENCE,
    NODE_PREFIX,
    NODE_SUFFIX,
    NODE_PRIMARY,
    NODE_IDENTIFIER,
    NODE_LITERAL,
    NODE_CLASS,
    NODE_DOT,
    NODE_ACTION,
    NODE_PREDICATE_AND, // &
    NODE_PREDICATE_NOT, // !
    NODE_QUANTIFIER_OPTIONAL, // ?
    NODE_QUANTIFIER_ZERO_OR_MORE, // *
    NODE_QUANTIFIER_ONE_OR_MORE, // +
    NODE_GROUP, // ( Expression )
    NODE_BEGIN_CAPTURE,
    NODE_END_CAPTURE,
    NODE_CAPTURE, // New node type for capture groups
    NODE_SEMANTIC_ACTION // New node type for semantic action blocks
} ASTNodeType;

// Structure for a list of AST nodes (e.g., for Sequence, Expression)
struct ASTNodeList {
    struct ASTNode *node;
    ASTNodeList *next;
};

// Structure for a list of strings (e.g., for character class ranges)
struct StringList {
    char *str;
    StringList *next;
};

// Main AST Node structure
typedef struct ASTNode {
    ASTNodeType type;
    // Line and column for error reporting
    int line;
    int column;

    // For NODE_IDENTIFIER, NODE_LITERAL, NODE_CLASS, NODE_ACTION
    // This field holds the actual string value for these terminal nodes.
    char *value;

    // Optional semantic action associated with this node
    struct ASTNode *action;

    // Generic children list for arbitrary ASTs (like AMOS)
    ASTNodeList *children;

    union {
        // NODE_GRAMMAR
        struct {
            ASTNodeList *definitions; // List of Definition nodes
        } grammar;

        // NODE_DEFINITION
        struct {
            char *identifier; // Rule name (string value is in node->value)
            struct ASTNode *pre_expression_action; // Optional action before the expression
            struct ASTNode *expression; // Rule body
            struct ASTNode *post_expression_action; // Optional action after the expression
            ASTNodeType rule_predicate_type; // NODE_PREDICATE_AND, NODE_PREDICATE_NOT, or 0 for none
            struct ASTNode *rule_predicate_action; // Optional action for rule-level predicate
        } definition;

        // NODE_EXPRESSION (alternation)
        struct {
            ASTNodeList *sequences; // List of Sequence nodes
            struct ASTNode *trailing_predicate; // Optional trailing predicate (e.g., &{ action })
        } expression;

        // NODE_SEQUENCE (concatenation)
        struct {
            ASTNodeList *prefixes; // List of Prefix nodes
            struct ASTNode *empty_sequence_action; // Optional action for empty sequence
        } sequence;

        // NODE_PREFIX (predicate or just suffix)
        struct {
            ASTNodeType prefix_type; // NODE_PREDICATE_AND, NODE_PREDICATE_NOT, or 0 for none
            struct ASTNode *suffix;
            struct ASTNode *action; // Optional action for AND Action case
        } prefix;

        // NODE_SUFFIX (quantifier or just primary)
        struct {
            ASTNodeType quantifier_type; // NODE_QUANTIFIER_OPTIONAL, etc., or 0 for none
            struct ASTNode *primary;
            struct ASTNode *action; // Optional semantic action
        } suffix;

        // NODE_PRIMARY (base elements)
        // For NODE_PRIMARY, its 'child' will be one of the actual terminal/grouping nodes.
        // e.g., Primary -> Identifier, Primary -> Group, Primary -> Dot
        struct ASTNode *child;

        // NODE_CHAR_RANGE (for Class)
        struct {
            char start_char;
            char end_char;
        } char_range;

        // NODE_CHAR (for Class)
        char single_char;

    } data;
} ASTNode;

// Function prototypes for AST creation and manipulation
ASTNode *ast_new_node(ASTNodeType type, int line, int column);
ASTNodeList *ast_new_node_list(ASTNode *node);
ASTNodeList *ast_node_list_append(ASTNodeList *list, ASTNode *node);
StringList *ast_new_string_list(char *str);
StringList *ast_string_list_append(StringList *list, char *str);
void ast_free(ASTNode *node);
void ast_free_list(ASTNodeList *list);
void ast_free_string_list(StringList *list);
void print_ast_node(ASTNode *node, int indent);

#endif // AST_H