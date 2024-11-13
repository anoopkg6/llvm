#include "SubstrToStartsWithCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"

using namespace clang::ast_matchers;

namespace clang::tidy::modernize {

void SubstrToStartsWithCheck::registerMatchers(MatchFinder *Finder) {
    const auto SubstrCall = cxxMemberCallExpr(
      callee(cxxMethodDecl(hasName("substr"))),
      hasArgument(0, integerLiteral(equals(0))),
      hasArgument(1, expr().bind("length")),
      on(expr().bind("str")))
      .bind("call");

  // Helper for matching comparison operators
  auto AddSimpleMatcher = [&](auto Matcher) {
    Finder->addMatcher(
        traverse(TK_IgnoreUnlessSpelledInSource, std::move(Matcher)), this);
  };

  // Match str.substr(0,n) == "literal"
  AddSimpleMatcher(
      binaryOperation(
          hasOperatorName("=="),
          hasEitherOperand(SubstrCall),
          hasEitherOperand(expr().bind("comparison")))
          .bind("positiveComparison"));

  // Match str.substr(0,n) != "literal"
  AddSimpleMatcher(
      binaryOperation(
          hasOperatorName("!="),
          hasEitherOperand(SubstrCall),
          hasEitherOperand(expr().bind("comparison")))
          .bind("negativeComparison"));
}

std::string SubstrToStartsWithCheck::getExprStr(const Expr *E,
                                               const SourceManager &SM,
                                               const LangOptions &LO) {
    CharSourceRange Range = CharSourceRange::getTokenRange(E->getSourceRange());
    return Lexer::getSourceText(Range, SM, LO).str();
}

void SubstrToStartsWithCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *Call = Result.Nodes.getNodeAs<CXXMemberCallExpr>("call");
  if (!Call)
    return;

  const auto *PositiveComparison = Result.Nodes.getNodeAs<Expr>("positiveComparison");
  const auto *NegativeComparison = Result.Nodes.getNodeAs<Expr>("negativeComparison");
  
  if (!PositiveComparison && !NegativeComparison)
    return;

  bool Negated = NegativeComparison != nullptr;
  const auto *Comparison = Negated ? NegativeComparison : PositiveComparison;
  
  // Skip if in macro
  if (Call->getBeginLoc().isMacroID())
    return;

  const auto *Str = Result.Nodes.getNodeAs<Expr>("str");
  const auto *CompareExpr = Result.Nodes.getNodeAs<Expr>("comparison");
  
  if (!Str || !CompareExpr)
    return;

  // Emit the diagnostic
  auto Diag = diag(Call->getExprLoc(), 
                   "use starts_with() instead of substr(0, n) comparison");

  // Build the replacement text
  std::string ReplacementStr = 
      (Negated ? "!" : "") +
      Lexer::getSourceText(CharSourceRange::getTokenRange(Str->getSourceRange()),
                          *Result.SourceManager, getLangOpts()).str() +
      ".starts_with(" +
      Lexer::getSourceText(CharSourceRange::getTokenRange(CompareExpr->getSourceRange()),
                          *Result.SourceManager, getLangOpts()).str() +
      ")";

  // Create the fix-it
  Diag << FixItHint::CreateReplacement(
      CharSourceRange::getTokenRange(Comparison->getSourceRange()),
      ReplacementStr);
}

} // namespace clang::tidy::modernize