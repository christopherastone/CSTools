#ifndef ISSUE_HPP_INC
#define ISSUE_HPP_INC

#include <string>

enum class Severity {
  ERROR,
  WARNING
};

class Issue {
public:
  Issue(std::string file, unsigned int line, unsigned int col,
        std::string title, std::string message, Severity severity);

  bool operator<(const Issue&) const;
  bool operator==(const Issue&) const;
  bool operator!=(const Issue&) const;
  std::string getText() const;
  std::string getHTML() const;

private:
  std::string file_;
  unsigned int line_;
  unsigned int column_;
  std::string title_;
  std::string message_;
  Severity severity_;

  static std::string cwd_;

  std::string getSeverityText() const;
  std::string getSeverityANSI() const;
  std::string getSeverityHTML() const;
};

#endif
