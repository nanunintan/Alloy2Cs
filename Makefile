all: alloy2ocl alloy2cd cd2alloy ocl2alloy

alloy2ocl:
	ghc --make Tools/Alloy2OCL.hs -o alloy2ocl

alloy2cd:
	ghc --make Tools/Alloy2CD.hs -o alloy2cd

cd2alloy:
	ghc --make Tools/CD2Alloy.hs -o cd2alloy

ocl2alloy:
	ghc --make Tools/OCL2Alloy.hs -o ocl2alloy

clean: 
	rm -f alloy2ocl alloy2cd cd2alloy ocl2alloy; rm -f *~;\
	for dir in Alloy CD OCL Transform Tools; do \
	  rm -f $${dir}/*.hi; rm -f $${dir}/*.o; rm -f $${dir}/*~; \
	done
