include ../../llvm.mk
include ../../clang.mk
CLANGCOMMAND = clang++ $(CLANG_RUNTIME_INCLUDES) -std=c++11 -stdlib=libc++ -c

test:
	../replacer -f "IntList::insert_after(IntList::Iterator, int)" -f "IntList::IntList()" -f "IntList::size()" -r cs70-intlist-bad1.cpp  cs70-intlist-good.cpp -- $(CLANGCOMMAND)

test2:
	cp bigint-solution/bigint.cpp bigint-solution/bi.cpp
	cp bigint-submission/bigint.cpp bigint-submission/bi.cpp
	../replacer bigint-solution/bi.cpp bigint-submission/bi.cpp  -- clang++ -isystem/Users/stone/llvm-install/lib/clang/3.7.0/include -isystem/usr/include/c++/4.2.1 -std=c++11 -c
	ls -lrt bigint-solution
	ls -lrt bigint-submission

tidy:
	~/llvm-install/bin/clang-tidy bigint-submission/bigint.cpp -checks='*' -header-filter='.*' -- clang++ -isystem/Users/stone/llvm-install/lib/clang/3.7.0/include -isystem/usr/include/c++/4.2.1 -std=c++11 -c

mod:
	cp bigint-submission/bigint.cpp bigint-submission/bi.cpp
	(cd bigint-submission; ~/llvm-install/bin/clang-modernize -summary -loop-convert -use-auto -use-nullptr bi.cpp -- clang++ -isystem/Users/stone/llvm-install/lib/clang/3.7.0/include -isystem/usr/include/c++/4.2.1 -std=c++11 -c)
	diff bigint-submission/{bigint.cpp,bi.cpp}

