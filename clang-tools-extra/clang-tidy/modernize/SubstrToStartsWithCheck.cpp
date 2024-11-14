#include "SubstrToStartsWithCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"

using namespace clang::ast_matchers;

namespace clang::tidy::modernize {

void SubstrToStartsWithCheck::registerMatchers(MatchFinder *Finder) {
    // Match the substring call
    const auto SubstrCall = cxxMemberCallExpr(
      callee(cxxMethodDecl(hasName("substr"))),
      hasArgument(0, integerLiteral(equals(0))),
      hasArgument(1, expr().bind("length")),
      on(expr().bind("str")))
      .bind("call");

    // Match string literals on the right side
    const auto StringLiteral = stringLiteral().bind("literal");
    
    // Helper for matching comparison operators
    auto AddSimpleMatcher = [&](auto Matcher) {
        Finder->addMatcher(
            traverse(TK_IgnoreUnlessSpelledInSource, std::move(Matcher)), this);
    };

    // Match str.substr(0,n) == "literal"
    AddSimpleMatcher(
        binaryOperation(
            hasOperatorName("=="),
            hasLHS(SubstrCall),
            hasRHS(StringLiteral))
            .bind("positiveComparison"));

    // Also match "literal" == str.substr(0,n)
    AddSimpleMatcher(
        binaryOperation(
            hasOperatorName("=="),
            hasLHS(StringLiteral),
            hasRHS(SubstrCall))
            .bind("positiveComparison"));

    // Match str.substr(0,n) != "literal" 
    AddSimpleMatcher(
        binaryOperation(
            hasOperatorName("!="),
            hasLHS(SubstrCall),
            hasRHS(StringLiteral))
            .bind("negativeComparison"));

    // Also match "literal" != str.substr(0,n)
    AddSimpleMatcher(
        binaryOperation(
            hasOperatorName("!="),
            hasLHS(StringLiteral),
            hasRHS(SubstrCall))
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
  
  if (Call->getBeginLoc().isMacroID())
    return;

  const auto *Str = Result.Nodes.getNodeAs<Expr>("str");
  const auto *Literal = Result.Nodes.getNodeAs<StringLiteral>("literal");
  
  if (!Str || !Literal)
    return;

  // Get the string expression
  std::string StrText = Lexer::getSourceText(
      CharSourceRange::getTokenRange(Str->getSourceRange()),
      *Result.SourceManager, getLangOpts()).str();

  // Get the literal text
  std::string LiteralText = Lexer::getSourceText(
      CharSourceRange::getTokenRange(Literal->getSourceRange()),
      *Result.SourceManager, getLangOpts()).str();

  // Build the replacement
  std::string ReplacementText = (Negated ? "!" : "") + StrText + ".starts_with(" + 
                               LiteralText + ")";

  auto Diag = diag(Call->getExprLoc(),
                   "use starts_with() instead of substr(0, n) comparison");

  Diag << FixItHint::CreateReplacement(
      CharSourceRange::getTokenRange(Comparison->getSourceRange()),
      ReplacementText);
}

} // namespace clang::tidy::modernize