#!/bin/bash

TARGET=$1
NOSCREEN=$2  # optional second argument

# IDL_VARS=""
# if [ "$NOSCREEN" = "noscreen" ]; then
#   IDL_VARS="noscreen=1"
# fi

# Create temporary IDL script
echo "; ==== JISMO startup ===="                         >  temp_idl_input.pro
echo "COMMON jismo_flags, noscreen"                    >> temp_idl_input.pro
echo "noscreen = 0"                                    >> temp_idl_input.pro

if [ "$NOSCREEN" = "noscreen" ]; then
  echo "noscreen = 1"                                  >> temp_idl_input.pro
else
  echo "IF noscreen EQ 0 THEN Device, Retain=2, Decomposed=0, true_color=24" >> temp_idl_input.pro
fi


# Add paths
cat >> temp_idl_input.pro <<EOF
; ----- Set environment -----
!PATH = '../../IDL/AAstron/:' + !PATH
!PATH = '../../IDL/Coyote/:'  + !PATH
!PATH = '../../IDL/COSMOS/:'  + !PATH 
!PATH = '../../IDL/MPFIT/:'   + !PATH
!PATH = '../../IDL/:'         + !PATH

; ----- Compile all needed procedures before any call -----
.r alias
.r mp_absreDgaia
.r get_pol2xdr
.r mk_chi2pol
.r get_bestchi2pol
.r mk_rsv2aled
.r mp_absred_fixedLed
.r fit_target

; ----- Run actual procedure -----
fit_target, '$TARGET'
EOF

# Run IDL with the full script
idl < temp_idl_input.pro

# Clean up
rm temp_idl_input.pro