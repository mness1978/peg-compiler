#include "compiler.h"
#include "ast.h"
#include "vm_bytecode.h"
#include "bytecode_file.h" // Include for bytecode file format

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Helper to emit a single instruction
static void emit_instruction(CompilerContext *ctx, OpCode opcode, int operand) {
    if (ctx->bytecode_ptr >= MAX_BYTECODE_SIZE) {
        ctx->error_message = "Bytecode buffer overflow";
        return;
    }
    ctx->bytecode[ctx->bytecode_ptr].opcode = opcode;
    ctx->bytecode[ctx->bytecode_ptr].operand = operand;
    ctx->bytecode_ptr++;
}

typedef struct {
    const char *name;
    ActionOpCode opcode;
    int operand_size; // 0, 1 (byte), 2 (short), 4 (int)
} ActionOpDef;

static const ActionOpDef ACTION_OPS[] = {
    {"PUSH_INT", ACT_PUSH_INT, 4},
    {"DUP", ACT_DUP, 0},
    {"DROP", ACT_DROP, 0},

    {"SWAP", ACT_SWAP, 0},
    {"OVER", ACT_OVER, 0},
    
    {"ADD", ACT_ADD, 0},
    {"SUB", ACT_SUB, 0},
    {"MUL", ACT_MUL, 0},
    {"DIV", ACT_DIV, 0},
    {"EQ", ACT_EQ, 0},
    {"NEQ", ACT_NEQ, 0},
    {"LT", ACT_LT, 0},
    {"GT", ACT_GT, 0},
    {"NOT", ACT_NOT, 0},
    {"LOAD", ACT_LOAD, 1},
    {"STORE", ACT_STORE, 1},
    {"GET_CAPTURE", ACT_GET_CAPTURE, 0},
    {"MAKE_NODE", ACT_MAKE_NODE, 1}, // Operand: Node Type
    {"SET_NODE_VAL", ACT_SET_NODE_VAL, 0},
    {"APPEND_CHILD", ACT_APPEND_CHILD, 0},
    {"PREPEND_CHILD", ACT_PREPEND_CHILD, 0},
    {"SET_TYPE", ACT_SET_TYPE, 0},
    
    {"JUMP", ACT_JUMP, 2},
    {"JUMP_IF_ZERO", ACT_JUMP_IF_ZERO, 2},
    {"HALT", ACT_HALT, 0},
    {"PRINT", ACT_PRINT, 0},
    {NULL, 0, 0}
};

static int compile_action_bytecode(const char *action_text, uint8_t **out_bytecode, uint32_t *out_size) {
    // Simple tokenizer and assembler
    // Assumes space-separated tokens
    // Example: "PUSH_INT 1 MAKE_NODE 0"
    
    // First pass: calculate size
    // Actually, let's just allocate a large buffer and resize later, or use a dynamic buffer
    // For simplicity, max action size 1KB
    uint8_t buffer[1024];
    uint32_t ptr = 0;

    char *text_copy = strdup(action_text);
    char *token = strtok(text_copy, " \t\n\r");
    
    while (token != NULL) {
        // Find opcode
        int found = 0;
        for (int i = 0; ACTION_OPS[i].name != NULL; ++i) {
            if (strcmp(token, ACTION_OPS[i].name) == 0) {
                buffer[ptr++] = ACTION_OPS[i].opcode;
                found = 1;
                
                if (ACTION_OPS[i].operand_size > 0) {
                    token = strtok(NULL, " \t\n\r");
                    if (!token) {
                        fprintf(stderr, "Error: Missing operand for %s\n", ACTION_OPS[i].name);
                        free(text_copy);
                        return 0;
                    }
                    int val = atoi(token); // Simple integer parsing
                    
                    if (ACTION_OPS[i].operand_size == 4) {
                        buffer[ptr++] = (val >> 24) & 0xFF;
                        buffer[ptr++] = (val >> 16) & 0xFF;
                        buffer[ptr++] = (val >> 8) & 0xFF;
                        buffer[ptr++] = val & 0xFF;
                    } else if (ACTION_OPS[i].operand_size == 2) {
                        buffer[ptr++] = (val >> 8) & 0xFF;
                        buffer[ptr++] = val & 0xFF;
                    } else if (ACTION_OPS[i].operand_size == 1) {
                        buffer[ptr++] = val & 0xFF;
                    }
                }
                break;
            }
        }
        
        if (!found) {
            // Ignore unknown tokens? Or error?
            // For now, warn and ignore (could be comments or braces if not cleaned)
            // But peg.peg actions are inside { ... }. The parser might pass the braces.
            // We should probably strip braces before calling this, or ignore them here.
            if (strcmp(token, "{") != 0 && strcmp(token, "}") != 0) {
                fprintf(stderr, "Warning: Unknown action token '%s'\n", token);
            }
        }
        
        token = strtok(NULL, " \t\n\r");
    }
    
    free(text_copy);
    
    *out_size = ptr;
    *out_bytecode = (uint8_t *)malloc(ptr);
    memcpy(*out_bytecode, buffer, ptr);
    return 1;
}

static int add_action(CompilerContext *ctx, const char *action_text) {
    if (ctx->action_table_ptr >= MAX_ACTIONS) {
        ctx->error_message = "Too many actions";
        return -1;
    }
    
    uint8_t *bytecode;
    uint32_t size;
    if (!compile_action_bytecode(action_text, &bytecode, &size)) {
        ctx->error_message = "Failed to compile action";
        return -1;
    }
    
    int idx = ctx->action_table_ptr;
    ctx->action_bytecode[idx] = bytecode;
    ctx->action_sizes[idx] = size;
    ctx->action_table_ptr++;
    return idx;
}

// Helper to add a string to the string table
static int add_string(CompilerContext *ctx, const char *str) {
    for (int i = 0; i < ctx->string_table_ptr; ++i) {
        if (strcmp(ctx->string_table[i], str) == 0) {
            return i; // String already exists
        }
    }
    if (ctx->string_table_ptr >= MAX_STRINGS) {
        ctx->error_message = "String table overflow";
        return -1;
    }
    ctx->string_table[ctx->string_table_ptr] = strdup(str);
    if (!ctx->string_table[ctx->string_table_ptr]) {
        perror("strdup failed");
        exit(EXIT_FAILURE);
    }
    return ctx->string_table_ptr++;
}

// Helper to add a character class to the char class table
static int add_char_class(CompilerContext *ctx, const char *class_str) {
    for (int i = 0; i < ctx->char_class_table_ptr; ++i) {
        if (strcmp(ctx->char_class_table[i], class_str) == 0) {
            return i; // Class already exists
        }
    }
    if (ctx->char_class_table_ptr >= MAX_CHAR_CLASSES) {
        ctx->error_message = "Char class table overflow";
        return -1;
    }
    ctx->char_class_table[ctx->char_class_table_ptr] = strdup(class_str);
    if (!ctx->char_class_table[ctx->char_class_table_ptr]) {
        perror("strdup failed");
        exit(EXIT_FAILURE);
    }
    return ctx->char_class_table_ptr++;
}



// Helper to add a rule definition (name and address)
static int add_rule_definition(CompilerContext *ctx, const char *name, int address) {
    if (ctx->rules_ptr >= MAX_RULES) {
        ctx->error_message = "Rule table overflow";
        return -1;
    }
    ctx->rules[ctx->rules_ptr].name = strdup(name);
    if (!ctx->rules[ctx->rules_ptr].name) {
        perror("strdup failed");
        exit(EXIT_FAILURE);
    }
    ctx->rules[ctx->rules_ptr].address = address;
    ctx->rules[ctx->rules_ptr].id = ctx->rules_ptr;
    return ctx->rules_ptr++;
}

// Helper to get a rule's address by name
// This function is now only used during the fixup pass, not during bytecode generation
static int get_rule_address(CompilerContext *ctx, const char *rule_name) {
    for (int i = 0; i < ctx->rules_ptr; ++i) {
        if (strcmp(ctx->rules[i].name, rule_name) == 0) {
            return ctx->rules[i].address;
        }
    }
    ctx->error_message = "Undefined rule reference (during address lookup)";
    return -1; // Rule not found
}

// Helper to get a rule's ID by name
static int get_rule_id(CompilerContext *ctx, const char *rule_name) {
    for (int i = 0; i < ctx->rules_ptr; ++i) {
        if (strcmp(ctx->rules[i].name, rule_name) == 0) {
            return ctx->rules[i].id;
        }
    }
    return -1; // Rule not found
}

// Helper to add an import (unresolved reference)
static int add_import(CompilerContext *ctx, const char *name, int instruction_index) {
    if (ctx->imports_ptr >= MAX_RULES) { // limiting imports same as rules for now
         ctx->error_message = "Too many imports";
         return -1;
    }
    ctx->imports[ctx->imports_ptr].name = strdup(name);
    // Note: We don't check for strdup failure here for brevity, but should in production
    ctx->imports[ctx->imports_ptr].instruction_index = instruction_index;
    return ctx->imports_ptr++;
}

// Forward declarations for compilation functions
static int compile_expression(CompilerContext *ctx, ASTNode *node);
static int compile_sequence(CompilerContext *ctx, ASTNode *node);
static int compile_prefix(CompilerContext *ctx, ASTNode *node);
static int compile_suffix(CompilerContext *ctx, ASTNode *node);
static int compile_primary(CompilerContext *ctx, ASTNode *node);


// Compiles a primary expression
static int compile_primary(CompilerContext *ctx, ASTNode *node) {
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG: compile_primary ENTRY. Node type: %d\n", node ? node->type : -1);
    }
    if (!node || node->type != NODE_PRIMARY) {
        ctx->error_message = "Expected PRIMARY node";
        return 0;
    }
    ASTNode *child = node->data.child;
    if (!child) {
        ctx->error_message = "PRIMARY node has no child";
        return 0;
    }

    switch (child->type) {
        case NODE_IDENTIFIER: {
            int rule_id = get_rule_id(ctx, child->value);
            if (rule_id == -1) {
                // Undefined rule: treat as Import
                emit_instruction(ctx, OP_CALL, -1); // Placeholder operand
                if (add_import(ctx, child->value, ctx->bytecode_ptr - 1) == -1) return 0;
            } else {
                emit_instruction(ctx, OP_CALL, rule_id); // Emit rule_id, will be fixed up later
            }
            break;
        }
        case NODE_LITERAL: {
            int string_idx = add_string(ctx, child->value);
            if (string_idx == -1) return 0;
            emit_instruction(ctx, OP_MATCH_LITERAL, string_idx);
            break;
        }
        case NODE_CLASS: {
            int class_idx = add_char_class(ctx, child->value);
            if (class_idx == -1) return 0;
            emit_instruction(ctx, OP_MATCH_CHAR_CLASS, class_idx);
            break;
        }
        case NODE_DOT: {
            emit_instruction(ctx, OP_MATCH_ANY, 0);
            break;
        }
        case NODE_ACTION: {
            // Placeholder for action. The operand will be an index to the action table.
            // For now, we just add a dummy entry.
            int action_idx = add_action(ctx, child->value);
            if (action_idx == -1) return 0;
            emit_instruction(ctx, OP_ACTION, action_idx);
            break;
        }
        case NODE_BEGIN_CAPTURE: {
            emit_instruction(ctx, OP_BEGIN_CAPTURE, 0);
            break;
        }
        case NODE_END_CAPTURE: {
            emit_instruction(ctx, OP_END_CAPTURE, 0);
            break;
        }
        case NODE_CAPTURE: {
            emit_instruction(ctx, OP_BEGIN_CAPTURE, 0);
            if (!compile_expression(ctx, child->data.child)) return 0;
            emit_instruction(ctx, OP_END_CAPTURE, 0);
            break;
        }
        case NODE_GROUP: {
            if (!compile_expression(ctx, child->data.child)) return 0;
            break;
        }
        default:
            ctx->error_message = "Unknown primary node type";
            ctx->error_line = child->line;
            ctx->error_column = child->column;
            return 0;
    }

    // Check for attached semantic action
    if (node->action) {
        int action_idx = add_action(ctx, node->action->value);
        if (action_idx == -1) return 0;
        emit_instruction(ctx, OP_ACTION, action_idx);
    }

    return 1;
}

// Compiles a suffix expression (Primary with optional quantifier)
static int compile_suffix(CompilerContext *ctx, ASTNode *node) {
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG: compile_suffix ENTRY. Node type: %d\n", node ? node->type : -1);
    }
    if (!node || node->type != NODE_SUFFIX) {
        if (ctx->verbose) {
            fprintf(stderr, "DEBUG: compile_suffix FAILED. Expected SUFFIX node, got %d\n", node ? node->type : -1);
        }
        ctx->error_message = "Expected SUFFIX node";
        ctx->error_line = node ? node->line : 0;
        ctx->error_column = node ? node->column : 0;
        return 0;
    }

    int success = 1;
    switch (node->data.suffix.quantifier_type) {
        case NODE_QUANTIFIER_OPTIONAL: { // E?
            int current_pos = ctx->bytecode_ptr;
            emit_instruction(ctx, OP_CHOICE, 0); // Placeholder for jump address on failure
            if (!compile_primary(ctx, node->data.suffix.primary)) return 0;
            emit_instruction(ctx, OP_COMMIT, ctx->bytecode_ptr + 1); // Commit and skip FAIL
            // If primary fails, it will jump to the address after this COMMIT
            // The CHOICE instruction's operand needs to be updated to point here.
            ctx->bytecode[current_pos].operand = ctx->bytecode_ptr; // Jump to here on failure (effectively skip primary)
            break;
        }
        case NODE_QUANTIFIER_ZERO_OR_MORE: { // E*
            int loop_start = ctx->bytecode_ptr;
            emit_instruction(ctx, OP_CHOICE, 0); // Placeholder for jump address on failure (L_exit_loop)
            if (!compile_primary(ctx, node->data.suffix.primary)) return 0; // Compile E
            emit_instruction(ctx, OP_JUMP, loop_start); // If E succeeds, jump back to L_loop
            ctx->bytecode[loop_start].operand = ctx->bytecode_ptr; // L_exit_loop: Jump here if E fails
            break;
        }
        case NODE_QUANTIFIER_ONE_OR_MORE: { // E+
            int loop_start = ctx->bytecode_ptr;
            if (!compile_primary(ctx, node->data.suffix.primary)) return 0; // Must match at least once
            int choice_pos = ctx->bytecode_ptr;
            emit_instruction(ctx, OP_CHOICE, 0); // Placeholder for jump address on failure (L_exit_loop)
            if (!compile_primary(ctx, node->data.suffix.primary)) return 0; // Compile E
            emit_instruction(ctx, OP_JUMP, choice_pos); // If E succeeds, jump back to the choice
            ctx->bytecode[choice_pos].operand = ctx->bytecode_ptr; // L_exit_loop: Jump here if E fails
            break;
        }
        default: { // No quantifier, just primary
            success = compile_primary(ctx, node->data.suffix.primary);
            break;
        }
    }
    return success;
}

// Compiles a prefix expression (Predicate or just Suffix)
static int compile_prefix(CompilerContext *ctx, ASTNode *node) {
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG: compile_prefix ENTRY. Node type: %d\n", node ? node->type : -1);
    }
    if (!node || node->type != NODE_PREFIX) {
        ctx->error_message = "Expected PREFIX node";
        ctx->error_line = node ? node->line : 0;
        ctx->error_column = node ? node->column : 0;
        return 0;
    }

    int success = 1;
    switch (node->data.prefix.prefix_type) {
        case NODE_PREDICATE_AND: {   // Handle semantic predicate: &{ Action }
    if (node->data.prefix.action) {
        // Compile the action
        int action_idx = add_action(ctx, node->data.prefix.action->value);
        if (action_idx == -1) return 0;
        emit_instruction(ctx, OP_ACTION, action_idx);
        // Predicates don't consume input, but OP_ACTION might fail
        // The parser handles & by wrapping in CHOICE/FAIL logic usually,
        // but here we just emit the action.
        // Wait, &{ Action } means: run Action. If true, success. If false, fail.
        // Input position is not restored by OP_ACTION itself, but by the backtracking logic if it fails.
        // But semantic predicates usually check state without consuming.
        // So OP_ACTION is fine.
        return 1; // Return 1 for success, as the action itself is compiled.
    } else {
                // Handle &Suffix
                int current_pos = ctx->bytecode_ptr;
                emit_instruction(ctx, OP_CHOICE, 0); // Save state for backtracking
                if (!compile_suffix(ctx, node->data.prefix.suffix)) return 0;
                emit_instruction(ctx, OP_COMMIT, ctx->bytecode_ptr + 1); // Commit and succeed
                // If suffix fails, it will backtrack to the CHOICE, which will then jump to the address after this COMMIT.
                // The CHOICE instruction's operand needs to be updated to point here.
                ctx->bytecode[current_pos].operand = ctx->bytecode_ptr; // Jump here on failure (effectively skip suffix)
            }
            }
            break;
        case NODE_PREDICATE_NOT: { // !E
            int current_pos = ctx->bytecode_ptr;
            emit_instruction(ctx, OP_CHOICE, 0); // Save state for backtracking
            if (!compile_suffix(ctx, node->data.prefix.suffix)) return 0;
            // If suffix succeeds, then !E fails. So, jump to a FAIL instruction.
            emit_instruction(ctx, OP_FAIL, 0); // Fail the current parse path
            // If suffix fails, it will backtrack to the CHOICE, which will then jump to the address after this FAIL.
            // The CHOICE instruction's operand needs to be updated to point here.
            ctx->bytecode[current_pos].operand = ctx->bytecode_ptr; // Jump here if suffix fails (effectively succeed !E)
            break;
        }
        default: { // No predicate, just suffix
            if (!node->data.prefix.suffix) {
                ctx->error_message = "PREFIX node has NULL suffix child";
                ctx->error_line = node->line;
                ctx->error_column = node->column;
                return 0;
            }
            if (node->data.prefix.suffix->type != NODE_SUFFIX) {
                if (ctx->verbose) {
                    fprintf(stderr, "DEBUG: compile_prefix: node->data.prefix.suffix type is %d, expected NODE_SUFFIX. Line: %d, Col: %d\n",
                            node->data.prefix.suffix->type, node->data.prefix.suffix->line, node->data.prefix.suffix->column);
                }
                ctx->error_message = "PREFIX node's suffix child is not a SUFFIX node";
                ctx->error_line = node->data.prefix.suffix->line;
                ctx->error_column = node->data.prefix.suffix->column;
                return 0;
            }
            success = compile_suffix(ctx, node->data.prefix.suffix);
            break;
        }
    }
    return success;
}

// Compiles a sequence of prefixes
static int compile_sequence(CompilerContext *ctx, ASTNode *node) {
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG: compile_sequence ENTRY. Node type: %d\n", node ? node->type : -1);
    }
    if (!node || node->type != NODE_SEQUENCE) {
        ctx->error_message = "Expected SEQUENCE node";
        return 0;
    }
    ASTNodeList *current = node->data.sequence.prefixes;
    while (current) {
        if (!compile_prefix(ctx, current->node)) return 0;
        current = current->next;
    }
    return 1;
}

// Compiles an expression (alternation of sequences)
static int compile_expression(CompilerContext *ctx, ASTNode *node) {
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG: compile_expression ENTRY. Node type: %d\n", node ? node->type : -1);
    }
    if (!node || node->type != NODE_EXPRESSION) {
        ctx->error_message = "Expected EXPRESSION node";
        return 0;
    }
    ASTNodeList *current = node->data.expression.sequences;
    if (!current) {
        // Empty expression, should not happen in valid PEG
        return 1;
    }

    // Store jump targets for each alternative's failure
    // and for successful completion of an alternative
    int *commit_jumps = (int *)malloc(sizeof(int) * MAX_RULES); // Max rules is a rough upper bound for alternatives
    if (!commit_jumps) {
        perror("malloc failed for commit_jumps");
        return 0;
    }
    int commit_jumps_ptr = 0;

    // Compile each sequence
    while (current) {
        int current_choice_pos = ctx->bytecode_ptr;
        emit_instruction(ctx, OP_CHOICE, 0); // Placeholder for jump on failure

        if (!compile_sequence(ctx, current->node)) {
            free(commit_jumps);
            return 0;
        }

        // If this sequence succeeded, commit and jump to the end of the expression
        emit_instruction(ctx, OP_COMMIT, 0); // Placeholder for jump to end of expression
        commit_jumps[commit_jumps_ptr++] = ctx->bytecode_ptr - 1; // Store position of COMMIT instruction

        // Update the CHOICE instruction to jump here if the sequence failed
        ctx->bytecode[current_choice_pos].operand = ctx->bytecode_ptr;

        current = current->next;
    }

    // All alternatives have been tried and failed, so emit a FAIL
    emit_instruction(ctx, OP_FAIL, 0);

    // Now, fixup all COMMIT instructions to jump to the instruction after the FAIL
    int end_expression_addr = ctx->bytecode_ptr;
    for (int i = 0; i < commit_jumps_ptr; ++i) {
        ctx->bytecode[commit_jumps[i]].operand = end_expression_addr;
    }

    free(commit_jumps);
    return 1;
}

// Compiles a rule definition
static int compile_definition(CompilerContext *ctx, ASTNode *node) {
    if (ctx->verbose) {
        fprintf(stderr, "DEBUG: compile_definition ENTRY. Node type: %d, Identifier: %s\n", 
                node ? node->type : -1, node ? node->data.definition.identifier : "NULL");
    }
    if (!node || node->type != NODE_DEFINITION) {
        ctx->error_message = "Expected DEFINITION node";
        return 0;
    }

    // Set the rule's address to the current bytecode pointer
    // This assumes the rule has already been registered in Pass 1
    int rule_id = get_rule_id(ctx, node->value);
    if (rule_id == -1) {
        ctx->error_message = "Rule not registered in first pass";
        ctx->error_line = node->line;
        ctx->error_column = node->column;
        return 0;
    }
    ctx->rules[rule_id].address = ctx->bytecode_ptr;

    if (!compile_expression(ctx, node->data.definition.expression)) return 0;

    emit_instruction(ctx, OP_RETURN, 0); // End of rule, return to caller
    return 1;
}


void compiler_init(CompilerContext *ctx) {
    memset(ctx, 0, sizeof(CompilerContext));
    ctx->bytecode_ptr = 0;
    ctx->string_table_ptr = 0;
    ctx->char_class_table_ptr = 0;
    ctx->action_table_ptr = 0;
    ctx->rules_ptr = 0;
    ctx->error_message = NULL;
    ctx->error_line = 0;
    ctx->error_column = 0;
    ctx->imports_ptr = 0;
}

int compiler_compile(CompilerContext *ctx, ASTNode *grammar_ast) {
    if (!grammar_ast || grammar_ast->type != NODE_GRAMMAR) {
        ctx->error_message = "Expected GRAMMAR node as root";
        return 0;
    }

    // Pass 1: Register all rule definitions and their names
    ASTNodeList *current_def = grammar_ast->data.grammar.definitions;
    while (current_def) {
        ASTNode *def_node = current_def->node;
        if (def_node->type != NODE_DEFINITION) {
            ctx->error_message = "Expected DEFINITION node in grammar list";
            ctx->error_line = def_node->line;
            ctx->error_column = def_node->column;
            return 0;
        }
        int rule_idx = add_rule_definition(ctx, def_node->value, -1);
        if (rule_idx == -1) { // -1 is a placeholder address
            return 0; // Error message already set by add_rule_definition
        }
        
        ctx->rules[rule_idx].flags = def_node->data.definition.flags;
        // printf("DEBUG: Rule '%s' has flags %d\n", ctx->rules[rule_idx].name, ctx->rules[rule_idx].flags);
        
        current_def = current_def->next;
    }

    // Pass 2: Generate bytecode for each rule
    current_def = grammar_ast->data.grammar.definitions;
    while (current_def) {
        if (!compile_definition(ctx, current_def->node)) {
            return 0; // Error message already set by compile_definition
        }
        current_def = current_def->next;
    }

    // Pass 3: Fixup rule call addresses
    for (int i = 0; i < ctx->bytecode_ptr; ++i) {
        if (ctx->bytecode[i].opcode == OP_CALL) {
            int rule_id = ctx->bytecode[i].operand;
            if (rule_id == -1) {
                // Import, skip fixup (will be linked by VM)
                continue;
            }
            if (rule_id < 0 || rule_id >= ctx->rules_ptr) {
                ctx->error_message = "Internal compiler error: invalid rule ID in OP_CALL";
                return 0;
            }
            ctx->bytecode[i].operand = ctx->rules[rule_id].address;
        }
    }

    emit_instruction(ctx, OP_HALT, 0); // End of program

    return 1; // Success
}

void compiler_cleanup(CompilerContext *ctx) {
    for (int i = 0; i < ctx->string_table_ptr; ++i) {
        free(ctx->string_table[i]);
    }
    for (int i = 0; i < ctx->char_class_table_ptr; ++i) {
        free(ctx->char_class_table[i]);
    }
    for (int i = 0; i < ctx->rules_ptr; ++i) {
        free(ctx->rules[i].name);
    }
    // Action table cleanup depends on how actions are managed (e.g., dynamically loaded libraries)
    // For now, no specific cleanup for action_table as it just holds dummy indices.
}

int compiler_write_bytecode_file(CompilerContext *ctx, const char *filename) {
    // Pre-process strings for Exports and Imports
    // We need to ensure all rule names are in the string table before writing the header
    for (int i = 0; i < ctx->rules_ptr; ++i) {
        if (add_string(ctx, ctx->rules[i].name) == -1) return 0;
    }
    for (int i = 0; i < ctx->imports_ptr; ++i) {
        if (add_string(ctx, ctx->imports[i].name) == -1) return 0;
    }

    FILE *f = fopen(filename, "wb");
    if (!f) {
        perror("Failed to open bytecode file for writing");
        return 0;
    }

    // Write Header
    if (!write_uint32_be(f, PEGVM_BYTECODE_MAGIC) ||
        !write_uint32_be(f, PEGVM_BYTECODE_VERSION) ||
        !write_uint32_be(f, (uint32_t)ctx->bytecode_ptr) ||
        !write_uint32_be(f, (uint32_t)ctx->string_table_ptr) ||
        !write_uint32_be(f, (uint32_t)ctx->char_class_table_ptr) ||
        !write_uint32_be(f, (uint32_t)ctx->action_table_ptr) ||
        !write_uint32_be(f, (uint32_t)ctx->rules_ptr) || // Export Table Len
        !write_uint32_be(f, (uint32_t)ctx->imports_ptr)) { // Import Table Len
        perror("Failed to write bytecode file header");
        fclose(f);
        return 0;
    }

    // Write Bytecode
    for (int i = 0; i < ctx->bytecode_ptr; ++i) {
        if (!write_uint32_be(f, (uint32_t)ctx->bytecode[i].opcode) ||
            !write_uint32_be(f, (uint32_t)ctx->bytecode[i].operand)) {
            perror("Failed to write bytecode instructions");
            fclose(f);
            return 0;
        }
    }

    // Write String Table
    for (int i = 0; i < ctx->string_table_ptr; ++i) {
        size_t len = strlen(ctx->string_table[i]);
        if (!write_uint32_be(f, (uint32_t)len)) {
            perror("Failed to write string length");
            fclose(f);
            return 0;
        }
        if (fwrite(ctx->string_table[i], 1, len, f) != len) {
            perror("Failed to write string data");
            fclose(f);
            return 0;
        }
    }

    // Write Char Class Table
    for (int i = 0; i < ctx->char_class_table_ptr; ++i) {
        size_t len = strlen(ctx->char_class_table[i]);
        if (!write_uint32_be(f, (uint32_t)len)) {
            perror("Failed to write char class string length");
            fclose(f);
            return 0;
        }
        if (fwrite(ctx->char_class_table[i], 1, len, f) != len) {
            perror("Failed to write char class string data");
            fclose(f);
            return 0;
        }
    }

    // Write Action Table
    for (int i = 0; i < ctx->action_table_ptr; ++i) {
        uint32_t len = ctx->action_sizes[i];
        if (!write_uint32_be(f, len)) {
            perror("Failed to write action bytecode length");
            fclose(f);
            return 0;
        }
        if (fwrite(ctx->action_bytecode[i], 1, len, f) != len) {
            perror("Failed to write action bytecode data");
            fclose(f);
            return 0;
        }
    }

    // Write Export Table (Formerly Rule Entry Points, now NameIndex, Address, Flags)
    for (int i = 0; i < ctx->rules_ptr; ++i) {
        int name_idx = add_string(ctx, ctx->rules[i].name); // Should be fast (found)
        if (!write_uint32_be(f, (uint32_t)name_idx) ||
            !write_uint32_be(f, (uint32_t)ctx->rules[i].address) ||
            !write_uint32_be(f, (uint32_t)ctx->rules[i].flags)) {
            perror("Failed to write export table entry");
            fclose(f);
            return 0;
        }
    }

    // Write Import Table
    for (int i = 0; i < ctx->imports_ptr; ++i) {
        int name_idx = add_string(ctx, ctx->imports[i].name); // Should be fast (found)
        if (!write_uint32_be(f, (uint32_t)name_idx) ||
            !write_uint32_be(f, (uint32_t)ctx->imports[i].instruction_index)) {
            perror("Failed to write import table entry");
            fclose(f);
            return 0;
        }
    }

    fclose(f);
    return 1;
}
