#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // For debugging, if needed

// Helper to duplicate a string safely
static char *safe_strdup(const char *s) {
    if (!s) return NULL;
    char *new_s = strdup(s);
    if (!new_s) {
        perror("Failed to duplicate string");
        exit(EXIT_FAILURE);
    }
    return new_s;
}

ASTNode *ast_new_node(ASTNodeType type, int line, int column) {
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    if (!node) {
        perror("Failed to allocate ASTNode");
        exit(EXIT_FAILURE);
    }
    memset(node, 0, sizeof(ASTNode)); // Initialize all members to 0
    node->type = type;
    node->line = line;
    node->column = column;
    node->value = NULL; // Initialize value to NULL
    return node;
}

ASTNodeList *ast_new_node_list(ASTNode *node) {
    ASTNodeList *list = (ASTNodeList *)malloc(sizeof(ASTNodeList));
    if (!list) {
        perror("Failed to allocate ASTNodeList");
        exit(EXIT_FAILURE);
    }
    list->node = node;
    list->next = NULL;
    return list;
}

ASTNodeList *ast_node_list_append(ASTNodeList *list, ASTNode *node) {
    if (!list) {
        return ast_new_node_list(node);
    }
    ASTNodeList *current = list;
    while (current->next) {
        current = current->next;
    }
    current->next = ast_new_node_list(node);
    return list;
}

StringList *ast_new_string_list(char *str) {
    StringList *list = (StringList *)malloc(sizeof(StringList));
    if (!list) {
        perror("Failed to allocate StringList");
        exit(EXIT_FAILURE);
    }
    list->str = safe_strdup(str);
    list->next = NULL;
    return list;
}

StringList *ast_string_list_append(StringList *list, char *str) {
    if (!list) {
        return ast_new_string_list(str);
    }
    StringList *current = list;
    while (current->next) {
        current = current->next;
    }
    current->next = ast_new_string_list(str);
    return list;
}

void ast_free(ASTNode *node) {
    if (!node) return;

    // Free the 'value' field if it was allocated
    if (node->value) {
        free(node->value);
        node->value = NULL;
    }

    // Free the 'action' field if it was allocated
    if (node->action) {
        ast_free(node->action);
        node->action = NULL;
    }

    switch (node->type) {
        case NODE_GRAMMAR:
            ast_free_list(node->data.grammar.definitions);
            break;
        case NODE_DEFINITION:
            // node->value holds the identifier, already freed above
            ast_free(node->data.definition.pre_expression_action); // Free pre-action
            ast_free(node->data.definition.expression);
            ast_free(node->data.definition.post_expression_action); // Free post-action
            ast_free(node->data.definition.rule_predicate_action); // Free rule predicate action
            break;
        case NODE_EXPRESSION:
            ast_free_list(node->data.expression.sequences);
            if (node->data.expression.trailing_predicate)
				ast_free(node->data.expression.trailing_predicate);
            break;
        case NODE_SEQUENCE:
            ast_free_list(node->data.sequence.prefixes);
            ast_free(node->data.sequence.empty_sequence_action); // Free empty sequence action
            break;
        case NODE_PREFIX:
            ast_free(node->data.prefix.suffix);
            ast_free(node->data.prefix.action); // Free action node
            break;
        case NODE_SUFFIX:
            ast_free(node->data.suffix.primary);
            ast_free(node->data.suffix.action); // Free action node
            break;
        case NODE_PRIMARY:
            ast_free(node->data.child); // Free the child node
            break;
        case NODE_GROUP:
        case NODE_PREDICATE_AND:
        case NODE_PREDICATE_NOT:
        case NODE_QUANTIFIER_OPTIONAL:
        case NODE_QUANTIFIER_ZERO_OR_MORE:
        case NODE_QUANTIFIER_ONE_OR_MORE:
        case NODE_BEGIN_CAPTURE:
        case NODE_END_CAPTURE:
        case NODE_DOT:
            // These nodes have a child in data.child, which is freed by ast_free(node->data.child)
            // Or they are terminal and have no children.
            // The 'value' field is handled above.
            break;
        case NODE_CAPTURE: // New case for NODE_CAPTURE
            ast_free(node->data.child); // Free the captured expression
            break;
        case NODE_SEMANTIC_ACTION: // New case for NODE_SEMANTIC_ACTION
            // The 'value' field holds the C code string, which is freed above.
            break;
        case NODE_IDENTIFIER:
        case NODE_LITERAL:
        case NODE_CLASS:
        case NODE_ACTION:
            // 'value' field is handled above
            break;
    }
    free(node);
}

void ast_free_list(ASTNodeList *list) {
    ASTNodeList *current = list;
    while (current) {
        ASTNodeList *next = current->next;
        ast_free(current->node);
        free(current);
        current = next;
    }
}

void ast_free_string_list(StringList *list) {
    StringList *current = list;
    while (current) {
        StringList *next = current->next;
        free(current->str);
        free(current);
        current = next;
    }
}

// Function to print the AST (for debugging)
void print_ast_node(ASTNode *node, int indent) {
    if (!node) return;

    for (int i = 0; i < indent; ++i) printf("  ");
    printf("- Type: %d (Line: %d, Col: %d)", node->type, node->line, node->column);

    if (node->value) {
        printf(" Value: \"%s\"", node->value);
    }
    printf("\n");

    // Print generic children
    if (node->children) {
        ASTNodeList *current = node->children;
        while (current) {
            print_ast_node(current->node, indent + 1);
            current = current->next;
        }
    }

    // Handle specific PEG nodes if needed (omitted for brevity as we focus on AMOS)
    // But we should probably include them if we want to debug PEG AST too.
    // For now, generic children is enough for AMOS.
}
