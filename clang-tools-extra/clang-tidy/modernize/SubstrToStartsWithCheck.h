#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MODERNIZE_SUBSTRTOSTARTSWITHCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MODERNIZE_SUBSTRTOSTARTSWITHCHECK_H

#include "../ClangTidyCheck.h"

namespace clang::tidy::modernize {

class SubstrToStartsWithCheck : public ClangTidyCheck {
public:
  SubstrToStartsWithCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;

private:
  std::string getExprStr(const Expr *E, const SourceManager &SM,
                        const LangOptions &LO);
};

} // namespace clang::tidy::modernize

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MODERNIZE_SUBSTRTOSTARTSWITHCHECK_H