#!/usr/bin/env bash

set -x
set -eo pipefail

. .github/workflows/common.sh

get_key() {
	local key=$1
	local server=$2
	gpg --batch --keyserver "${server}" --recv-keys "${key}"
	echo -e "${key}:6:" | gpg --import-ownertrust
}

# verify signature
keys=(
    7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C # Julian Ospald <maerwald@hasufell.de>
    FFEB7CE81E16A36B3E2DED6F2DE04D4E97DB64AD # Ben Gamari <ben@well-typed.com>
    88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4 # Zubin Duggal <zubin@well-typed.com>
    EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF # Hécate <hecate@glitchbra.in>
)

for key in "${keys[@]}" ; do
	get_key "${key}" keys.openpgp.org || get_key "${key}" keyserver.ubuntu.com
done
unset key
gpg --verify "${METADATA_FILE}.sig"

