.PHONY: day1 pre

all: day1

pre:
	mkdir -p bin include obj

day1: pre
	ghc -hidir include -odir obj -o bin/ChronalCalibration day1/ChronalCalibration.hs
	bin/ChronalCalibration

clean:
	rm -r bin include obj