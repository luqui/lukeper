HSFILES = Source/Signals.hs Source/Control.hs Source/APC40mkII_Raw.hs Source/PureLoop.hs Source/Loop.hs Source/Sequencer.hs Source/Looper.hs 

all:
	export MACOSX_DEPLOYMENT_TARGET=10.11
	ghc -Wall -O -c $(HSFILES) -odir Builds/MacOSX/build/Debug -hidir Builds/MacOSX/build/Debug
	xcodebuild -project Builds/MacOSX/lukeper.xcodeproj
