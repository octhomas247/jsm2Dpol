# JSMF90 Makefile

FC = gfortran
FFLAGS = -O3 -ffree-line-length-none -ffree-form
SRC = src/jsm2Dpol_mods.f90 \
      src/jsm2Dpol_utils.f90 \
      src/jsm2Dpol_subroutines.f90 \
      src/jsm2Dpol_main.f90

BIN = af90.j

all: $(BIN)

$(BIN): $(SRC)
	$(FC) $(FFLAGS) $(SRC) -o $(BIN)

clean:
	rm -f $(BIN) src/*.o src/*.mod