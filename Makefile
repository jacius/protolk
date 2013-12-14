default: test

test:
	csi -s tests/run.scm
spec: test

install:
	chicken-install

uninstall:
	chicken-uninstall protolk

clean:
	rm -rf *.c *.o *.so *.import.scm salmonella.log
