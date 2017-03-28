all:
	ghc -c Source/Looper.hs -odir Builds/MacOSX/build/Debug -hidir Builds/MacOSX/build/Debug
	xcodebuild -project Builds/MacOSX/lukeper.xcodeproj
