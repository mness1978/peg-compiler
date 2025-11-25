#ifndef COMPILER_H
#define COMPILER_H

#include "ast.h"
#include "vm_bytecode.h"
#include "vm.h" // For SemanticActionFn
#include "bytecode_file.h" // For BytecodeFileHeader

#include <stddef.h> // For size_t

// Max number of rules, strings, char classes, actions
#define MAX_RULES 256
#define MAX_STRINGS 512
#define MAX_CHAR_CLASSES 256
#define MAX_ACTIONS 256
#define MAX_BYTECODE_SIZE 4096

// Structure to hold rule information during compilation
typedef struct {
    char *name;
    int address; // Starting address of the rule's bytecode
    int id;      // Unique ID for the rule
} CompiledRule;

// Compiler Context structure
typedef struct CompilerContext {
    // Bytecode output
    Instruction bytecode[MAX_BYTECODE_SIZE];
    int bytecode_ptr; // Current position in bytecode array

    // Tables for VM
    char *string_table[MAX_STRINGS];
    int string_table_ptr;

    char *char_class_table[MAX_CHAR_CLASSES];
    int char_class_table_ptr;

    // Action Table (Bytecode)
    uint8_t *action_bytecode[MAX_ACTIONS];
    uint32_t action_sizes[MAX_ACTIONS];
    int action_table_ptr;

    // Rule management
    CompiledRule rules[MAX_RULES];
    int rules_ptr;
    // Mapping from rule name to rule ID/address (for lookup during compilation)
    // This will be built during the first pass.
    // For simplicity, a linear scan for now, but a hash map would be better.

    // Error handling
    const char *error_message;
    int error_line;
    int error_column;

    int verbose; // Debug flag
} CompilerContext;

// Function prototypes for the compiler
void compiler_init(CompilerContext *ctx);
int compiler_compile(CompilerContext *ctx, ASTNode *grammar_ast);
void compiler_cleanup(CompilerContext *ctx);

int compiler_write_bytecode_file(CompilerContext *ctx, const char *filename);

// Helper functions for emitting bytecode (internal to compiler.c)
// static void emit_instruction(CompilerContext *ctx, OpCode opcode, int operand);
// static int add_string(CompilerContext *ctx, const char *str);
// static int add_char_class(CompilerContext *ctx, const char *class_str);
// static int add_action(CompilerContext *ctx, SemanticActionFn action_fn);
// static int get_rule_address(CompilerContext *ctx, const char *rule_name);
// static int get_rule_id(CompilerContext *ctx, const char *rule_name);

#endif // COMPILER_H
