# JISMO Makefile

FC = gfortran
FFLAGS = -O3 -ffree-line-length-none -ffree-form
SRC = src/jsm_utils.f90 \
      src/jsm_mods.f90 \
      src/jsm_subroutines.f90 \
      src/jsm_main.f90 

OBJ = $(SRC:src/%.f90=build/%.o)
MODDIR = build
BINDIR = build
BIN = $(BINDIR)/af90.j

all: $(BIN)

# Create build directory and compile
$(BIN): $(OBJ) | $(BINDIR)
	$(FC) $(FFLAGS) -I$(MODDIR) $(OBJ) -o $@

# Compile each source file to object in build/
build/%.o: src/%.f90 | $(MODDIR)
	$(FC) $(FFLAGS) -I$(MODDIR) -J$(MODDIR) -c $< -o $@

# Ensure build dir exists
$(MODDIR):
	mkdir -p $(MODDIR)

clean:
	rm -rf build af90.j