#include <algorithm>
#include <string>
#include <sstream>
#include "unistd.h"
#include "sys/param.h"

#include "llvm/Support/Path.h"

#include "issue.hpp"

std::string Issue::cwd_ = std::string(getcwd(new char[MAXPATHLEN], MAXPATHLEN)) + "/";

Issue::Issue(std::string file, int line, int col, std::string title, std::string message,
              Severity severity):
  //file_{llvm::sys::path::filename(file).str()},
  line_{line},
  column_{col},
  title_{title},
  message_{message},
  severity_{severity}
{
  if (cwd_.size() < file.size()) {
  auto pair = std::mismatch(cwd_.begin(), cwd_.end(), file.begin());
  file_ = std::string{pair.second, end(file)};
  } else {
    file_ = file;
  }
}

bool Issue::operator<(const Issue& rhs) const
{
  return line_ < rhs.line_;
}

std::string Issue::getSeverityANSI() const
{
  switch (severity_) {
    case Severity::WARNING:
      return "\033[33mwarning\033[0m";
    case Severity::ERROR:
      return "\033[31merror\033[0m";
  }
}

std::string Issue::getSeverityHTML() const
{
  switch (severity_) {
    case Severity::WARNING:
      return "<span style=\"color:yellow\">[WARNING]</span>";
    case Severity::ERROR:
      return "<span style=\"red\">[WARNING]</span>";
  }
}

std::string Issue::getText() const
{
  std::stringstream ss;

  ss << file_ << ":";
  ss << line_ << ":";
  ss << column_ << ": ";
  ss << getSeverityANSI() << ": " ;
  ss << title_ << "\n";
  ss << message_ << "\n";

  return ss.str();
}


std::string Issue::getHTML() const
{
  std::stringstream ss;

  ss << "<h3>" << getSeverityHTML() << ": ";
  ss << "("<< line_ << "," << column_ << ") ";
  ss << title_ << "</h3>\n";
  ss << "<pre>" << message_ << "</pre>\n";

  return ss.str();

}


