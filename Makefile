.PHONY : all llvm jvm clean 

all: llvm jvm

llvm:
	ghc -isrc/parser -isrc/compiler -isrc/ src/MainLLVM.hs -o insc_llvm

jvm:
	ghc -isrc/parser -isrc/compiler -isrc/ src/MainJVM.hs -o insc_jvm

clean :
	-find . -type f \( -name '*.hi' -o -name '*.o' \) -delete
	-rm -f insc_llvm
	-rm -f insc_jvm
