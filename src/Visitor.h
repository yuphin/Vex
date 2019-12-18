// I know this is an anti-pattern but oh well
#include <llvm/IR/Value.h>

// Variable types
struct Type;

struct AST;
struct BaseAST;
struct TopAST;
struct VariableDeclAST;
struct FunctionAST;
struct FunctionDeclAST;
struct FunctionBodyAST;
// Exprs
struct ExprAST;
struct BinaryExprAST;
struct UnaryExprAST;
struct VariableExprAST;
struct IntNumAST;
struct FloatingNumAST;
struct InvocationAST;
// Statements
struct StatementAST;
struct AssignmentStatementAST;
struct ReturnStatementAST;
struct PrintStatementAST;
struct ReadStatementAST;
struct IfStatementAST;
struct ForStatementAST;
struct WhileStatementAST;



class Visitor {
public:
    virtual llvm::Value* visit(BaseAST& el) = 0;
    virtual llvm::Value* visit(TopAST& el) = 0;
    virtual llvm::Value* visit(VariableDeclAST& el) = 0;
    virtual llvm::Value* visit(FunctionAST& el) = 0;
    virtual llvm::Value* visit(FunctionDeclAST& el) = 0;
    virtual llvm::Value* visit(FunctionBodyAST& el) = 0;
    virtual llvm::Value* visit(ExprAST& el) = 0;
    virtual llvm::Value* visit(BinaryExprAST& el) = 0;
    virtual llvm::Value* visit(UnaryExprAST& el) = 0;
    virtual llvm::Value* visit(VariableExprAST& el) = 0;
    virtual llvm::Value* visit(IntNumAST& el) = 0;
    virtual llvm::Value* visit(FloatingNumAST& el) = 0;
    virtual llvm::Value* visit(InvocationAST& el) = 0;
    virtual llvm::Value* visit(StatementAST& el) = 0;
    virtual llvm::Value* visit(AssignmentStatementAST& el) = 0;
    virtual llvm::Value* visit(ReturnStatementAST& el) = 0;
    virtual llvm::Value* visit(PrintStatementAST& el) = 0;
    virtual llvm::Value* visit(ReadStatementAST& el) = 0;
    virtual llvm::Value* visit(IfStatementAST& el) = 0;
    virtual llvm::Value* visit(ForStatementAST& el) = 0;
    virtual llvm::Value* visit(WhileStatementAST& el) = 0;
};