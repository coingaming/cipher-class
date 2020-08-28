#!/bin/sh

#
# app
#

export CIPHER_CLASS_LOG_ENV="dev"
export CIPHER_CLASS_LOG_FORMAT="Bracket" # Bracket | JSON
export CIPHER_CLASS_LIBPQ_CONN_STR="postgresql://nixbld1@localhost/cipher-class"
export CIPHER_CLASS_ENDPOINT_PORT="3000"
