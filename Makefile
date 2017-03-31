HSFILES = Source/APC40mkII.hs Source/Loop.hs Source/Sequencer.hs Source/Looper.hs 

all:
	export MACOSX_DEPLOYMENT_TARGET=10.11
	ghc -Wall -O -c $(HSFILES) -odir Builds/MacOSX/build/Debug -hidir Builds/MacOSX/build/Debug
	xcodebuild -project Builds/MacOSX/lukeper.xcodeproj
