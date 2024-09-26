CMAKE       := cmake -GNinja ..
CMAKE_DBG   := $(CMAKE) -DCMAKE_BUILD_TYPE=Debug
NINJA       := ninja
MKDIR       := mkdir -p
MKDIR_BUILD := $(MKDIR) build && cd build

.PHONY: test
test: 
	$(MKDIR_BUILD) && $(CMAKE_DBG) && $(NINJA) && ctest -VV

.PHONY: clean
clean:
	rm -r build
