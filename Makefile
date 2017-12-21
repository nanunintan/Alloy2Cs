all: alloy2ocl alloy2cd cd2alloy ocl2alloy

alloy2ocl:
	ghc --make Tools/Alloy2OCL.hs -o alloy2ocl

alloy2cd:
	ghc --make Tools/Alloy2CD.hs -o alloy2cd

cd2alloy:
	ghc --make Tools/CD2Alloy.hs -o cd2alloy

ocl2alloy:
	ghc --make Tools/OCL2Alloy.hs -o ocl2alloy

alloy2cs:
	ghc --make Tools/alloy2cs.hs -o alloy2cs

clean: 
	rm -f alloy2ocl alloy2cd cd2alloy ocl2alloy alloy2cs; rm -f *~;\
	for dir in Alloy CD OCL Cs Transform Tools; do \
	  rm -f $${dir}/*.hi; rm -f $${dir}/*.o; rm -f $${dir}/*~; \
	done
